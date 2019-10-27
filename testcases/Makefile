SRCS:=$(wildcard src/*.S)
BINS:=$(patsubst src/%.S,bin/%.bin,$(SRCS))

.PHONY: list all

all: $(BINS)

bin/%.bin: src/%.S Makefile
	$(PREFIX)as $< -o $@
	$(PREFIX)objcopy -O binary $@

list:
	@echo "Sources:"
	@echo $(SRCS)
	@echo
	@echo "Will generate:"
	@echo $(BINS)

clean:
	rm -f $(BINS)

disasm:
	@echo "Disassembly for $(FILE)"
	$(PREFIX)objdump -D $(FILE) -b binary -m riscv
