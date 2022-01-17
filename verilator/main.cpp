#include "VMulticore.h"
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

// initialize signals
void init() {
  top->io_axi_AWREADY = 0;
  top->io_axi_WREADY = 0;
  top->io_axi_BVALID = 0;

  top->io_axi_ARREADY = 0;
  top->io_axi_RVALID = 0;
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
    uint64_t r_data = memory[align(pending_read_addr)];
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
}

// load file
void load_file(const std::string &path) {
  size_t i = path.rfind('.');
  if (i == std::string::npos) {
    // load as elf
    // TODO
    assert(false);
  } else {
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
    printf("Loaded %ld bytes\n", size);
    fclose(fp);
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
  while (!Verilated::gotFinish()) {
    if (main_time > 50) {
      top->reset = 0;
    }
    if ((main_time % 10) == 0) {
      top->clock = 1;
    }
    if ((main_time % 10) == 5) {
      top->clock = 0;
      step();
    }
    top->eval();
    tfp->dump(main_time);
    main_time++;

    // return address for meow testcases
    if (top->io_debug_0_pc == 0x100000) {
      break;
    }
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
}