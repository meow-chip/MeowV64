.section .text
.globl _start
_start:
  li t0, 0x1
  j skip
  li t0, 0x2
skip:
  li t1, 0x1
