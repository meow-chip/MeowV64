#!/bin/bash
set -e

make
echo -n > benchmark.log
for filename in ../../testcases/riscv-tests/build/benchmarks/*.riscv; do
	./VSystem $filename | tee -a benchmark.log
done
