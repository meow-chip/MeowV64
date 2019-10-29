SRCS:=$(wildcard src/*.S)
BINS:=$(patsubst src/%.S,bin/%.bin,$(SRCS))
HEXS:=$(patsubst src/%.S,hex/%.hex,$(SRCS))

.PHONY: list all clean

all: $(BINS) $(HEXS)

bin/%.bin: src/%.S Makefile
	mkdir -p bin
	$(PREFIX)as $< -o $@
	$(PREFIX)objcopy -O binary $@

hex/%.hex: bin/%.bin Makefile
	mkdir -p hex
	hexdump $< -ve '1/1 "%02x" "\n"' > $@

list:
	@echo "Sources:"
	@echo $(SRCS)
	@echo
	@echo "Will generate:"
	@echo $(BINS)

clean:
	rm -f $(BINS) $(HEXS)

disasm:
	@echo "Disassembly for $(FILE)"
	$(PREFIX)objdump -D $(FILE) -b binary -m riscv
