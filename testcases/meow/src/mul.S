  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  # Computes 16!
  li a0, 1
  li a1, 1
  li a2, 16

loop:
  mul a0, a0, a1
  addi a1, a1, 1
  ble a1, a2, loop

  li a3, 20922789888000
  bne a3, a0, fail
  jr ra
fail:
  j fail
