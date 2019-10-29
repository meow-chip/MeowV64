.section .text
.globl _start
_start:
  li s1, 0x100

  li t0, 0x123456789ABCDEF
  li t1, 0xFF
  sd t0, 0x0(s1)
  sb t1, 0x5(s1)

  lb a1, 0x1(s1)
  lw a0, 0x4(s1)
