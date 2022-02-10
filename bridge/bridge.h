#include <cstdint>
#include <memory>

#include "generated.h"
#include "cxx.h"

#ifndef __BRIDGE_H__
#define __BRIDGE_H__

namespace meowv64::bridge {
  struct MemCmd;
  struct MemWrite;
  struct MemResp;

  class CPU {
  public:
    virtual ~CPU() {};

    virtual void set_int(size_t n, bool set) = 0;
    virtual void set_rst(bool rst) = 0;
    virtual bool tick() = 0;

    // TODO: use option when cxx supports it

    // Accept new data current cycle. Data updated regardless
    virtual bool mem_cmd_accept(MemCmd &dest) = 0;

    // Accept new data current cycle. Data updated regardless
    virtual bool mem_write_accept(MemWrite &dest) = 0;

    // Returns false if the previous request is not yet accepted.
    // In that case, the data is NOT updated
    virtual bool mem_resp_enqueue(const MemResp &data) = 0;
  };

  std::unique_ptr<CPU> init(const rust::Vec<rust::String> &args, const rust::Str trace);
}

#endif // __BRIDGE_H__
