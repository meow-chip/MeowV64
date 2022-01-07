##
# MeowV64 testcases
#
# @file
# @version 0.1

PREFIX ?= "riscv64-linux-gnu-"

.PHONY: all clean meow riscv-tests

all: meow riscv-tests

clean:
	make -C meow -f Makefile clean
	[ ! -d riscv-tests/build ] || make -C riscv-tests/build clean

meow:
	make -C meow -f Makefile all PREFIX=$(PREFIX)

riscv-tests:
	bash -c "cd riscv-tests && autoconf && mkdir -p build && cd build && ../configure"
	make -C riscv-tests/build -f Makefile RISCV_PREFIX=$(PREFIX)

# end
