  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  # Computes 16!
  li a0, 1
  li a1, -1
  li a2, -15

loop:
  mul a0, a0, a1
  addi a1, a1, -1
  bge a1, a2, loop

  li a3, -1307674368000
  bne a3, a0, fail
  jr ra
fail:
  j fail
