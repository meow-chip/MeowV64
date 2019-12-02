  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  li t0, 0x1
  j skip
  li t0, 0x2
skip:
  li t1, 0x1

  # Check
  bne t0, t1, fail
  jr ra
fail:
  j fail
