.section .text
.globl _start
_start:
  li t0, 0x10
  li t1, 0x5
  li t2, 0x1
loop:
  bltu t0, t1, exit# if t0 < t1 then target
  sub t0, t0, t2 # t0 = t1 - t2
  j loop
exit:
  li a0, 0x1
