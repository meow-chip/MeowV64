#!/bin/bash
set -e

make
for filename in ../testcases/riscv-tests/build/benchmarks/*.riscv; do
	./VMulticore $filename
done
