#!/bin/bash
set -e

make
for filename in ../../testcases/meow/bin/*.bin ../../testcases/riscv-tests/build/isa/*.bin; do
	./VRiscVSystem $filename
done
