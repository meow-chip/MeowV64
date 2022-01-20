#define ITERATION 0x200

  // probe number of cores from CLINT
  li t1, 0x02000000 // CLINT address
  li t2, 0 // core count
  li t3, 1 // msip
clint_loop:
  sw t3, 0(t1)
  lw t3, 0(t1)
  addi t2, t2, 1
  addi t1, t1, 4
  bnez t3, clint_loop

  // a1 stores core count
  mv a1, t2
 
  csrr a0, mhartid
  slli sp, a0, 12
  li t0, 0x80000000
  add sp, sp, t0
	li ra, 0x100000
  li s0, 0x90000000 // Lock
  li s1, 0x90000008 // Counter
  li s2, 0x90000010 // Completed
.section .text
.globl _start
_start:
  li t0, 0
  li t1, ITERATION

  // While t0 < t1
loop:
  li t3, 1
acquire:
  lw t2, 0(s0)
  bnez t2, acquire
  amoswap.d.aq t2, t3, 0(s0)
  bnez t2, acquire

  lw t2, 0(s1)
  addi t2, t2, 1
  sw t2, 0(s1)

  // Release
  amoswap.d.rl x0, x0, 0(s0)

  addi t0, t0, 1
  blt t0, t1, loop

// Add complete counter
  amoadd.d t2, t3, 0(s2)

// Only check on the last core that completes the test
  addi t3, a1, -1
  bne t2, t3, wait

// Acquire again to flush all writes
acquire_again:
  lw t2, 0(s0)
  bnez t2, acquire_again
  amoswap.d.aq t2, t3, 0(s0)
  bnez t2, acquire_again

  lw t0, 0(s1)

  // Expected result = core count * ITERATION
  li t3, ITERATION
  mul t1, a1, t3
  bne t0, t1, die

  li s0, 0x60000000
  li t0, 1
  sd t0, 0(s0)

wait:
  j wait

die:
  j die