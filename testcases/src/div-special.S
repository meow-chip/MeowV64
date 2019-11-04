  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  # Div by zero
  li a0, 0x1145141919810
  li a1, 0x0

  div t0, a0, a1
  rem t1, a0, a1

  li t2, -1
  bne t2, t0, fail
  bne t1, a0, fail

  # Unsigned div by zero
  divu t0, a0, a1
  remu t1, a0, a1

  li t2, -1
  bne t2, t0, fail
  bne t1, a0, fail

  # Overflow
  li a0, -0x8000000000000000 # Smallest number possible
  li a1, -1

  div t0, a0, a1
  rem t1, a0, a1

  bne t0, a0, fail
  bne t1, zero, fail

  jr ra
fail:
  j fail
