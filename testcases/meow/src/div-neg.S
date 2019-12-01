  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  li a0, -1
  li a1, -2
  li a2, -5

loop:
  mul a0, a0, a1
  addi a1, a1, -1
  bge a1, a2, loop

  addi a1, a1, 1

check:
  
  blt a0, zero, neg
  sub t1, zero, a1
  addi t1, t1, -1
  j addrem
neg:
  addi t1, a1, 1
addrem:
  add a0, a0, t1

  rem t0, a0, a1
  div a0, a0, a1

  addi a1, a1, 1

  bne t0, t1, fail
  li t1, -1
  bne t1, a1, check

  li t1, -1
  bne a0, t1, fail
  jr ra
fail:
  j fail
