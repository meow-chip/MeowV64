#include "VRiscVSystem.h"
#include <elf.h>
#include <iostream>
#include <map>
#include <signal.h>
#include <string>
#include <sys/time.h>
#include <unistd.h>
#include <verilated.h>
#include <verilated_fst_c.h>

// memory mapping
// 8 byte aligned
std::map<uint64_t, uint64_t> memory;

// align to 8 byte boundary
uint64_t align(uint64_t addr) { return (addr >> 3) << 3; }

VRiscVSystem *top;

vluint64_t main_time = 0;

double sc_time_stamp() { return main_time; }

bool finished = false;
int res = 0;

// tohost/fromhost
// default at 0x60000000
uint64_t tohost_addr = 0x60000000;
uint64_t fromhost_addr = 0x60000040;

// signature generation for riscv-torture
uint64_t begin_signature = 0;
uint64_t begin_signature_override = 0;
uint64_t end_signature = 0;

void ctrlc_handler(int arg) {
  fprintf(stderr, "Received Ctrl-C\n");
  finished = true;
  res = 1;
}

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
        fflush(stdout);
      } else if (pending_write_addr == tohost_addr) {
        // tohost
        uint32_t data = input & 0xFFFFFFFF;
        if (input == ((data & 0xFF) | 0x0101000000000000L)) {
          // serial
          printf("%c", input & 0xFF);
        } else if (data == 1) {
          // pass
          fprintf(stderr, "> ISA testsuite pass\n");
          finished = true;
        } else if ((data & 1) == 1) {
          uint32_t c = data >> 1;
          fprintf(stderr, "> ISA testsuite failed case %d\n", c);
          finished = true;
          res = 1;
        } else {
          fprintf(stderr, "> Unhandled tohost: %x\n", input);
          assert(false);
        }
      } else if (pending_write_addr == fromhost_addr) {
        // write to fromhost
        // clear tohost
        memory[tohost_addr] = 0;
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
    printf("> Loaded %ld bytes from BIN %s\n", size, path.c_str());
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

    // find symbol table
    uint64_t symbol_table_offset = 0;
    uint64_t symbol_table_size = 0;
    uint64_t string_table = 0;
    for (int i = 0; i < hdr->e_shnum; i++) {
      size_t offset = hdr->e_shoff + i * hdr->e_shentsize;
      Elf64_Shdr *hdr = (Elf64_Shdr *)&buffer[offset];
      if (hdr->sh_type == SHT_SYMTAB) {
        symbol_table_offset = hdr->sh_offset;
        symbol_table_size = hdr->sh_size;
      } else if (hdr->sh_type == SHT_STRTAB) {
        if (!string_table) {
          string_table = hdr->sh_offset;
        }
      }
    }

    // iterate symbol table
    for (int i = 0; i < symbol_table_size; i += sizeof(Elf64_Sym)) {
      size_t offset = symbol_table_offset + i;
      Elf64_Sym *symbol = (Elf64_Sym *)&buffer[offset];
      std::string name = (char *)&buffer[string_table + symbol->st_name];
      if (name == "tohost") {
        tohost_addr = symbol->st_value;
      } else if (name == "fromhost") {
        fromhost_addr = symbol->st_value;
      } else if (name == "begin_signature") {
        begin_signature = symbol->st_value;
      } else if (name == "begin_signature_override") {
        begin_signature_override = symbol->st_value;
      } else if (name == "end_signature") {
        end_signature = symbol->st_value;
      }
    }

    printf("> Loaded %ld bytes from ELF %s\n", size, path.c_str());
    fclose(fp);
    delete[] buffer;
  }
  fprintf(stderr, "> Using tohost at %x\n", tohost_addr);
  fprintf(stderr, "> Using fromhost at %x\n", fromhost_addr);
}

uint64_t get_time_us() {
  struct timeval tv = {};
  gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000000 + tv.tv_usec;
}

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  signal(SIGINT, ctrlc_handler);

  // https://man7.org/linux/man-pages/man3/getopt.3.html
  int opt;
  bool trace = false;
  bool progress = false;
  while ((opt = getopt(argc, argv, "tp")) != -1) {
    switch (opt) {
    case 't':
      trace = true;
      break;
    case 'p':
      progress = true;
      break;
    default: /* '?' */
      fprintf(stderr, "Usage: %s [-t] name\n", argv[0]);
      return 1;
    }
  }

  assert(optind < argc);
  load_file(argv[optind]);

  top = new VRiscVSystem;

  VerilatedFstC *tfp = nullptr;
  if (trace) {
    Verilated::traceEverOn(true);
    tfp = new VerilatedFstC;
    top->trace(tfp, 99);
    tfp->open("dump.fst");
    printf("> Enable tracing\n");
  }

  top->reset = 1;
  top->clock = 0;
  init();

  const size_t MAX_RS_COUNT = 8;
  size_t rs_free_cycle_count[MAX_RS_COUNT] = {};
  size_t cycles = 0;
  size_t issue_num_bounded_by_rob_size = 0;

  fprintf(stderr, "> Simulation started\n");
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
      if ((top->io_debug_0_mcycle % 10000) == 0 && top->io_debug_0_mcycle > 0 &&
          progress) {
        fprintf(stderr, "> mcycle: %ld\n", top->io_debug_0_mcycle);
        fprintf(stderr, "> minstret: %ld\n", top->io_debug_0_minstret);
        fprintf(stderr, "> pc: %lx\n", top->io_debug_0_pc);
      }

      if (top->io_debug_0_mcycle > 1000000) {
        fprintf(stderr, "> Timed out\n");
        finished = true;
        res = 1;
      }

      // accumulate rs free cycles
      for (int i = 0; i < MAX_RS_COUNT; i++) {
        if ((top->io_debug_0_rsFreeMask >> i) & 1) {
          rs_free_cycle_count[i]++;
        }
      }

      if (top->io_debug_0_issueNumBoundedByROBSize) {
        issue_num_bounded_by_rob_size++;
      }

      cycles++;
    }
    if ((main_time % 10) == 5) {
      top->clock = 0;
      step();
    }
    top->eval();
    if (tfp)
      tfp->dump(main_time);
    main_time++;
  }
  uint64_t elapsed_us = get_time_us() - begin;
  fprintf(stderr, "> Simulation finished\n");
  fprintf(stderr, "> mcycle: %ld\n", top->io_debug_0_mcycle);
  fprintf(stderr, "> minstret: %ld\n", top->io_debug_0_minstret);
  fprintf(stderr, "> IPC: %.2lf\n",
          (double)top->io_debug_0_minstret / top->io_debug_0_mcycle);
  fprintf(stderr, "> Simulation speed: %.2lf mcycle/s\n",
          (double)top->io_debug_0_mcycle * 1000000 / elapsed_us);
  fprintf(stderr, "> RS free cycle:");
  for (int i = 0; i < MAX_RS_COUNT; i++) {
    if (rs_free_cycle_count[i]) {
      fprintf(stderr, " %.2lf%%", rs_free_cycle_count[i] * 100.0 / cycles);
    }
  }
  fprintf(stderr, "\n");
  fprintf(stderr, "> Cycles when issue num is bounded by ROB size: %.2lf%%\n",
          issue_num_bounded_by_rob_size * 100.0 / cycles);

  if (begin_signature && end_signature) {
    if (begin_signature_override) {
      // signature is copied
      end_signature =
          end_signature - begin_signature + begin_signature_override;
      begin_signature = begin_signature_override;
    }
    fprintf(stderr, "> Dumping signature(%lx:%lx) to dump.sig\n",
            begin_signature, end_signature);
    FILE *fp = fopen("dump.sig", "w");
    for (uint64_t addr = begin_signature; addr < end_signature; addr += 16) {
      fprintf(fp, "%016llx%016llx\n", memory[addr + 8], memory[addr]);
    }
    fclose(fp);
  }

  if (tfp) {
    tfp->flush();
    tfp->close();
  }
  top->final();
  delete top;
  return res;
}