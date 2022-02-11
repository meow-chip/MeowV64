#include "rtl.h"
#include "bridge.h"

#include <verilated_fst_c.h>

#include <concepts>
#include <vector>
#include <string>
#include <optional>
#include <utility>

uint64_t clk;
double sc_time_stamp() {
  return clk;
}

namespace meowv64::bridge {
  const size_t ADDR_WIDTH = 64;
  const size_t DATA_WIDTH = 64;

  template<std::unsigned_integral T>
  void assign_data(T &data, const rust::Vec<uint64_t> &input) {
    data = input[0];
  }

  class CPUImpl : public CPU {
  public:
    CPUImpl(const rust::Str trace) {
      backend.reset = 1; // Initialize with reset
      backend.clock = 1; // Initialize to inactive clock edge

      if(trace.size() != 0) {
        Verilated::traceEverOn(true);
        tracer.emplace();
        backend.trace(&tracer.value(), 128);
        std::string std_trace = std::string(trace);
        tracer->open(std_trace.c_str());
      }

      this->backend.eval();
    }

    ~CPUImpl() {
      backend.final();
      if(tracer)
        tracer->close();
    }

    void set_int(size_t n, bool set) override {
      auto updated = this->backend.eints &= ~(1 << n);
      if(set) updated |= 1 << n;
      this->backend.eints = updated;
    }

    void set_rst(bool rst) override {
      this->backend.reset = rst;
    }

    bool tick() override {
      // clk % 2 == 0, we are on the inactive clock edge
      backend.clock = 0; // Negedge clk
      backend.eval();
      ++clk;

      // clk % 2 == 1, we are on the active clock edge
      if(tracer)
        tracer->dump(clk);
      backend.clock = 1; // Posedge clk
      backend.eval();
      ++clk;

      if(tracer)
        tracer->dump(clk);
      
      // Reset accepts and validity
      backend.mem_cmd_ready = false;
      backend.mem_downlink_ready = false;
      if(backend.mem_uplink_ready) backend.mem_uplink_valid = false;

      return Verilated::gotFinish();
    }

    bool mem_cmd_accept(MemCmd &dest) override {
      dest.id = backend.mem_cmd_bits_id;
      dest.addr = backend.mem_cmd_bits_addr;
      dest.op = static_cast<MemOp>(backend.mem_cmd_bits_op);
      dest.size = backend.mem_cmd_bits_size;
      dest.burst = backend.mem_cmd_bits_burst;

      backend.mem_cmd_ready = true;
      return backend.mem_cmd_valid; // Valid should never comb depends on ready, so we don't have to re evaluate
    }

    bool mem_write_accept(MemWrite &dest) override {
      dest.data = backend.mem_downlink_bits_data;
      backend.mem_downlink_ready = true;
      return backend.mem_downlink_valid;
    }

    // Returns false if the previous request is not yet accepted.
    // In that case, the data is NOT updated
    bool mem_resp_enqueue(const MemResp &data) override {
      if(backend.mem_uplink_valid) {
        return false;
      }

      backend.mem_uplink_valid = true;
      backend.mem_uplink_bits_id = data.id;
      backend.mem_uplink_bits_data = data.data;
      return true;
    }

  private:

    rtl backend;
    std::optional<VerilatedFstC> tracer = std::nullopt;
  };

  std::unique_ptr<CPU> init(const rust::Vec<rust::String> &args, const rust::Str trace) {
    // TODO: configurable clock rate

    std::vector<std::string> std_args(args.size());
    std::vector<const char *> argv(args.size());
    for(size_t i = 0; i < args.size(); ++i) {
      std_args[i] = std::string(args[i]);
      argv[i] = std_args[i].c_str();
    }

    // Initialize verilator
    Verilated::commandArgs(args.size(), argv.data());

    // Initialize clk
    clk = 0;

    return std::make_unique<CPUImpl>(trace);
  }
}
