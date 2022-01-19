#define ITERATION 10
	li ra, 0x100000
  li s0, 0x90000000 // Memcpy from
  li s1, 0x90001000 // Memcpy to
  li s2, 0x1000     // Memcpy size
.section .text
.globl _start
_start:
  csrr a0, mhartid
  bnez a0, other_hart

  li t0, 0
  li t1, ITERATION
  // memcpy
  mv t2, s0
  mv t3, s1
  add t4, s0, s2
memcpy_loop:
  ld t5, 0(t2)
  // sd t5, 0(t3)
  amoswap.d zero, t5, 0(t3)
  addi t2, t2, 8
  addi t3, t3, 8
  bne t2, t4, memcpy_loop

loop:
  
  // memcmp
  mv t2, s0
  mv t3, s1
  add t4, s0, s2
memcmp_loop:
  ld t5, 0(t2)
  ld t6, 0(t3)
  bne t5, t6, fail
  addi t2, t2, 8
  addi t3, t3, 8
  bne t2, t4, memcmp_loop

  addi t0, t0, 1
  bne t0, t1, loop

  // end
  jr ra

fail:
  j fail

other_hart:
  // loop amoadd.w
  mv a0, s0
  add a1, s1, s2

amo_loop:
  amoadd.w zero, zero, 0(a0)
  addi a0, a0, 4
  bne a0, a1, amo_loop
  mv a0, a1
  j amo_loop