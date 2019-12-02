  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  csrrsi t0, misa, 0
  li t1, 0x8000000000001105
  bne t1, t0, fail

  li t1, 1145141919810
  csrrw zero, mtvec, t1
  csrrc t0, mtvec, zero
  bne t0, t1, fail
  jr ra
fail:
  j fail
