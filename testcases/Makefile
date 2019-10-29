SRCS:=$(wildcard src/*.S)
BINS:=$(patsubst src/%.S,bin/%.bin,$(SRCS))
TMPS:=$(patsubst src/%.S,bin/%.bin.tmp,$(SRCS))
HEXS:=$(patsubst src/%.S,hex/%.hex,$(SRCS))

CSRCS:=$(wildcard c/*.c)
REFS:=$(patsubst c/%.c,ref/%.S,$(CSRCS))

.PHONY: list all clean ref

all: $(BINS) $(HEXS)

bin/%.bin: src/%.S Makefile
	mkdir -p bin
	$(PREFIX)as $< -o $@.tmp
	$(PREFIX)ld $@.tmp -o $@
	$(PREFIX)objcopy -O binary $@

hex/%.hex: bin/%.bin Makefile
	mkdir -p hex
	hexdump $< -ve '1/1 "%02x" "\n"' > $@

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
	rm -f $(BINS) $(HEXS) $(TMPS)

disasm:
	@echo "Disassembly for $(FILE)"
	$(PREFIX)objdump -D $(FILE) -b binary -m riscv
