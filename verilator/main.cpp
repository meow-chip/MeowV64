#include "VMulticore.h"
#include <elf.h>
#include <iostream>
#include <map>
#include <string>
#include <sys/time.h>
#include <verilated.h>
#include <verilated_vcd_c.h>

// memory mapping
// 8 byte aligned
std::map<uint64_t, uint64_t> memory;

// align to 8 byte boundary
uint64_t align(uint64_t addr) { return (addr >> 3) << 3; }

VMulticore *top;

vluint64_t main_time = 0;

double sc_time_stamp() { return main_time; }

bool finished = false;
int res = 0;

// initialize signals
void init() {
  top->io_axi_AWREADY = 0;
  top->io_axi_WREADY = 0;
  top->io_axi_BVALID = 0;

  top->io_axi_ARREADY = 0;
  top->io_axi_RVALID = 0;

  // Always have interrupt 1 set at high
  top->io_eints_1 = 1;
}

// step per clock fall
void step() {
  // handle read
  static bool pending_read = false;
  static uint64_t pending_read_id = 0;
  static uint64_t pending_read_addr = 0;
  static uint64_t pending_read_len = 0;
  static uint64_t pending_read_size = 0;
  if (!pending_read && top->io_axi_ARVALID) {
    top->io_axi_ARREADY = 1;
    pending_read = true;
    pending_read_id = top->io_axi_ARID;
    pending_read_addr = top->io_axi_ARADDR;
    pending_read_len = top->io_axi_ARLEN;
    pending_read_size = top->io_axi_ARSIZE;
  } else {
    top->io_axi_ARREADY = 0;
  }

  if (pending_read) {
    top->io_axi_RVALID = 1;
    top->io_axi_RID = pending_read_id;
    uint64_t r_data;
    if (pending_read_addr == 0x10001014) {
      // serial lsr
      r_data = 1L << (32 + 5);
    } else {
      r_data = memory[align(pending_read_addr)];
    }
    uint64_t mask = 0xffffffffffffffffL;
    if (pending_read_size != 3) {
      mask = (1L << ((1L << pending_read_size) * 8)) - 1L;
    }
    uint64_t shifted_mask = mask << ((pending_read_addr & 7) * 8);
    top->io_axi_RDATA = r_data & shifted_mask;
    top->io_axi_RLAST = pending_read_len == 0;

    // RREADY might be stale without eval()
    top->eval();
    if (top->io_axi_RREADY) {
      if (pending_read_len == 0) {
        pending_read = false;
      } else {
        pending_read_addr += 1 << pending_read_size;
        pending_read_len--;
      }
    }
  } else {
    top->io_axi_RVALID = 0;
  }

  // handle write
  static bool pending_write = false;
  static bool pending_write_finished = false;
  static uint64_t pending_write_addr = 0;
  static uint64_t pending_write_len = 0;
  static uint64_t pending_write_size = 0;
  if (!pending_write && top->io_axi_AWVALID) {
    top->io_axi_AWREADY = 1;
    pending_write = 1;
    pending_write_addr = top->io_axi_AWADDR;
    pending_write_len = top->io_axi_AWLEN;
    pending_write_size = top->io_axi_AWSIZE;
    pending_write_finished = 0;
  } else {
    top->io_axi_AWREADY = 0;
  }

  if (pending_write && !pending_write_finished) {
    top->io_axi_WREADY = 1;

    // WVALID might be stale without eval()
    top->eval();
    if (top->io_axi_WVALID) {
      uint64_t base = memory[align(pending_write_addr)];
      uint64_t input = top->io_axi_WDATA;
      uint64_t be = top->io_axi_WSTRB;
      uint64_t offset = pending_write_addr & 7;
      uint64_t size = 1 << pending_write_size;

      uint64_t muxed = 0;
      for (int i = 0; i < 8; i++) {
        uint64_t sel;
        if (i < offset) {
          sel = (base >> (i * 8)) & 0xff;
        } else if (i >= offset + size) {
          sel = (base >> (i * 8)) & 0xff;
        } else if (((be >> i) & 1) == 1) {
          sel = (input >> (i * 8)) & 0xff;
        } else {
          sel = (base >> (i * 8)) & 0xff;
        }
        muxed |= (sel << (i * 8));
      }

      memory[align(pending_write_addr)] = muxed;

      if (pending_write_addr == 0x10001000) {
        // serial
        printf("%c", input & 0xFF);
      } else if (pending_write_addr == 0x20000000) {
        // tohost
        uint32_t data = input & 0xFFFFFFFF;
        if (input == ((data & 0xFF) | 0x0101000000000000L)) {
          // serial
          printf("%c", input & 0xFF);
        } else if (data == 1) {
          // pass
          printf("ISA testsuite pass\n");
          finished = true;
        } else if ((data & 1) == 1) {
          uint32_t c = data >> 1;
          printf("ISA testsuite failed case %d\n", c);
          finished = true;
          res = 1;
        }
      }

      pending_write_addr += 1L << pending_write_size;
      pending_write_len--;
      if (top->io_axi_WLAST) {
        assert(pending_write_len == -1);
        pending_write_finished = true;
      }
    }
  } else {
    top->io_axi_WREADY = 0;
  }

  if (pending_write_finished) {
    top->io_axi_BVALID = 1;
    top->io_axi_BRESP = 0;
    top->io_axi_BID = 0;

    // BREADY might be stale without eval()
    top->eval();
    if (top->io_axi_BREADY) {
      pending_write = false;
      pending_write_finished = false;
    }
  } else {
    top->io_axi_BVALID = 0;
  }
}

// load file
void load_file(const std::string &path) {
  size_t i = path.rfind('.');
  std::string ext;
  if (i != std::string::npos) {
    ext = path.substr(i);
  }
  if (ext == ".bin") {
    // load as bin
    FILE *fp = fopen(path.c_str(), "rb");
    assert(fp);
    uint64_t addr = 0x80000000;

    // read whole file and pad to multiples of 8
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    size_t padded_size = align(size + 7);
    uint8_t *buffer = new uint8_t[padded_size];
    memset(buffer, 0, padded_size);

    size_t offset = 0;
    while (!feof(fp)) {
      ssize_t read = fread(&buffer[offset], 1, size - offset, fp);
      if (read <= 0) {
        break;
      }
      offset += read;
    }

    for (int i = 0; i < padded_size; i += 8) {
      memory[addr + i] = *((uint64_t *)&buffer[i]);
    }
    printf("Loaded %ld bytes from BIN %s\n", size, path.c_str());
    fclose(fp);
    delete[] buffer;
  } else {
    // load as elf

    // read whole file
    FILE *fp = fopen(path.c_str(), "rb");
    assert(fp);
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *buffer = new uint8_t[size];
    memset(buffer, 0, size);

    size_t offset = 0;
    while (!feof(fp)) {
      ssize_t read = fread(&buffer[offset], 1, size - offset, fp);
      if (read <= 0) {
        break;
      }
      offset += read;
    }

    Elf64_Ehdr *hdr = (Elf64_Ehdr *)buffer;
    assert(hdr->e_ident[EI_MAG0] == ELFMAG0);
    assert(hdr->e_ident[EI_MAG1] == ELFMAG1);
    assert(hdr->e_ident[EI_MAG2] == ELFMAG2);
    assert(hdr->e_ident[EI_MAG3] == ELFMAG3);
    // 64bit
    assert(hdr->e_ident[EI_CLASS] == ELFCLASS64);
    // little endian
    assert(hdr->e_ident[EI_DATA] == ELFDATA2LSB);

    // https://github.com/eklitzke/parse-elf/blob/master/parse_elf.cc
    // iterate program header
    size_t total_size = 0;
    for (int i = 0; i < hdr->e_phnum; i++) {
      size_t offset = hdr->e_phoff + i * hdr->e_phentsize;
      Elf64_Phdr *hdr = (Elf64_Phdr *)&buffer[offset];
      if (hdr->p_type == PT_LOAD) {
        // load memory
        size_t size = hdr->p_filesz;
        size_t offset = hdr->p_offset;
        size_t dest = hdr->p_paddr;
        total_size += size;
        for (int i = 0; i < size; i += 8) {
          uint64_t data = *(uint64_t *)&buffer[offset + i];
          memory[dest + i] = data;
        }
      }
    }

    printf("Loaded %ld bytes from ELF %s\n", size, path.c_str());
    fclose(fp);
    delete[] buffer;
  }
}

uint64_t get_time_us() {
  struct timeval tv = {};
  gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  if (argc != 2) {
    printf("Usage: %s input_file\n", argv[0]);
    return 1;
  }
  load_file(argv[1]);

  top = new VMulticore;

  Verilated::traceEverOn(true);

  VerilatedVcdC *tfp = new VerilatedVcdC;
  top->trace(tfp, 99);
  tfp->open("dump.vcd");

  top->reset = 1;
  top->clock = 0;
  init();

  printf("Simulation started\n");
  uint64_t begin = get_time_us();
  while (!Verilated::gotFinish() && !finished) {
    if (main_time > 50) {
      top->reset = 0;
    }
    if ((main_time % 10) == 0) {
      top->clock = 1;

      // return address for meow testcases
      if (top->io_debug_0_pc == 0x100000) {
        finished = true;
      }

      // log per 10000 mcycle
      if ((top->io_debug_0_mcycle % 10000) == 0 && top->io_debug_0_mcycle > 0) {
        printf("mcycle: %ld\n", top->io_debug_0_mcycle);
        printf("minstret: %ld\n", top->io_debug_0_minstret);
        printf("pc: %lx\n", top->io_debug_0_pc);
      }
    }
    if ((main_time % 10) == 5) {
      top->clock = 0;
      step();
    }
    top->eval();
    tfp->dump(main_time);
    main_time++;
  }
  uint64_t elapsed_us = get_time_us() - begin;
  printf("Simulation finished\n");
  printf("mcycle: %ld\n", top->io_debug_0_mcycle);
  printf("minstret: %ld\n", top->io_debug_0_minstret);
  printf("IPC: %.2lf\n",
         (double)top->io_debug_0_minstret / top->io_debug_0_mcycle);
  printf("Simulation speed: %.2lf mcycle/s\n",
         (double)top->io_debug_0_mcycle * 1000000 / elapsed_us);

  top->final();
  delete top;
  return res;
}