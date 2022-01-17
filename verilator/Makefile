VERILATOR = verilator
VERILATOR_FLAGS = -Wno-fatal -trace
VERILOG_SRCS = ../Multicore.v

all: VMulticore

VMulticore: main.cpp $(VERILOG_SRCS)
	$(VERILATOR) $(VERILATOR_FLAGS) --cc $(VERILOG_SRCS) --exe $<
	make -j8 -C obj_dir -f VMulticore.mk VMulticore
	cp obj_dir/VMulticore .