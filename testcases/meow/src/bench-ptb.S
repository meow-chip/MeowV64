  li sp, 0x10000
	li ra, 0x100000
UTEST_1PTB:
    li t0, 0x00001000
.LC0:
    addi t0, t0, -1
    ori t1, zero, 0
    ori t2, zero, 1
    ori t3, zero, 2
    bne t0, zero, .LC0
    jr ra
