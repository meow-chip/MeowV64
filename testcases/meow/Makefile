SRCS:=$(wildcard src/*.S)
BINS:=$(patsubst src/%.S,bin/%.bin,$(SRCS))
LINKEDS:=$(patsubst src/%.S,bin/%.bin.linked,$(SRCS))
ASSEMBLED:=$(patsubst src/%.S,bin/%.bin.assembled,$(SRCS))
HEXS:=$(patsubst src/%.S,hex/%.hex,$(SRCS))
DUMPS:=$(patsubst src/%.S,dump/%.S,$(SRCS))

CSRCS:=$(wildcard c/*.c)
REFS:=$(patsubst c/%.c,ref/%.S,$(CSRCS))

PREFIX ?= riscv64-linux-gnu-

.PHONY: list all clean ref

all: $(BINS) $(HEXS) $(DUMPS)

bin/%.assembled: src/%.S Makefile
	mkdir -p bin
	$(PREFIX)cpp $< | $(PREFIX)as -march=rv64imac -o $@ -

bin/%.linked: bin/%.assembled
	$(PREFIX)ld $< -T linker.lds -o $@

bin/%.bin: bin/%.linked
	cp $< $@
	$(PREFIX)objcopy -O binary $@

hex/%.hex: bin/%.bin Makefile
	mkdir -p hex
	hexdump -ve '1/1 "%02x" "\n"' $< > $@

dump/%.S: bin/%.linked
	$(PREFIX)objdump -S $^ > $@

ref: $(REFS)

ref/%.S : c/%.c Makefile
	mkdir -p ref
	$(PREFIX)gcc -S -nostdlib $< -o $@

list:
	@echo "Sources:"
	@echo $(SRCS)
	@echo
	@echo "Will generate:"
	@echo $(BINS)

list-ref:
	@echo "Sources:"
	@echo $(CSRCS)
	@echo
	@echo "Will generate:"
	@echo $(REFS)

clean:
	rm -f $(BINS) $(HEXS) $(TMPS) $(DUMPS)

disasm:
	@echo "Disassembly for $(FILE)"
	$(PREFIX)objdump -D $(FILE) -b binary -m riscv
