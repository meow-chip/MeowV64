.section .text
.globl _start
_start:
  li t0, 0x20
  li t1, 0x7E
  li s0, 0x100000 # Serial

loop:
  beq t0, t1, end
  sb t0, 0(s0)
  addi t0, t0, 1
  j loop

end:
  li a0, 0x1
  