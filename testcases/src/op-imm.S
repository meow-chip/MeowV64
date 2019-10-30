  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  li a0, 0x1145141919810
  slli a0, a0, 0x2

  li a1, (0x1145141919810 * 4)

  # Check
  bne a0, a1, fail
  jr ra
fail:
  j fail
