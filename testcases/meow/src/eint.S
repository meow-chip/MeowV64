  .globl _start
_start:
  csrr a0, mhartid

  bnez a0, wait // Only run on hart 0

  // Die on all ex/int before we allow them
  la t0, die
  csrw mtvec, t0

  // PLIC base addr
  li s0, 0x0C000000
  li s1, 0x0C002000
  li s2, 0x0C200000

  // Enable set source priority for source 1
  li t0, 0x07
  sw t0, 0x4(s0) # 

  // Set threshold for context 0 (hart 0 M-mode)
  li t0, 0x07
  sw t0, 0(s2) # 

  // Enable interrupt 1 for context 0
  li t0, 0x2
  sw t0, 0(s1)

  // Enable external interrupt in MIE
  li t0, (1 << 11)
  csrw mie, t0

  // Enable all interrupts in MSTATUS
  li t0, 1 << 3
  csrs mstatus, t0

  // Set correct mtvec
  la t0, pass
  csrw mtvec, t0

  // Lower threshold for context 0
  li t0, 0x5
  sw t0, 0(s2)

.align 4
wait:
  j wait

.align 4
die:
  j die

.align 4
pass:
  // Pass the test
  li s0, 0x60000000
  li t0, 1
  sd t0, 0(s0)
