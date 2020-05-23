  li sp, 0x10000
  li ra, 0x100000
.section .text
.globl _start
_start:
  li s1, 0x70000000

  li t0, 0x123456789ABCDEF
  li t1, 0xFF
  sd t0, 0x0(s1)
  sb t1, 0x5(s1)

  lw a0, 0x4(s1)
  lb a1, 0x1(s1)

  li a2, 0x0123FF67
  li a3, -51 # 0xcd sign extended to 64-bit

  # Check
  bne a0, a2, fail
  bne a1, a3, fail
  jr ra
fail:
  j fail
