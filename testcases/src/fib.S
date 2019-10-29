  li sp, 0x10000
	j main
div10:
	addi	sp,sp,-32
	sd	s0,24(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	sd	a1,-32(s0)
	ld	a5,-32(s0)
	sw	zero,0(a5)
	j	.L2
.L3:
	ld	a5,-32(s0)
	lw	a5,0(a5)
	addiw	a5,a5,1
	sext.w	a4,a5
	ld	a5,-32(s0)
	sw	a4,0(a5)
	ld	a5,-24(s0)
	lw	a5,0(a5)
	addiw	a5,a5,-10
	sext.w	a4,a5
	ld	a5,-24(s0)
	sw	a4,0(a5)
.L2:
	ld	a5,-24(s0)
	lw	a5,0(a5)
	mv	a4,a5
	li	a5,10
	bgt	a4,a5,.L3
	nop
	nop
	ld	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	div10, .-div10
	.align	1
	.globl	print
	.type	print, @function
print:
	addi	sp,sp,-48
	sd	ra,40(sp)
	sd	s0,32(sp)
	addi	s0,sp,48
	mv	a5,a0
	sw	a5,-36(s0)
	lw	a5,-36(s0)
	bnez	a5,.L7
	li	a5,1048576
	li	a4,48
	sb	a4,0(a5)
	li	a5,1048576
	li	a4,10
	sb	a4,0(a5)
	j	.L9
.L8:
	addi	a4,s0,-20
	addi	a5,s0,-36
	mv	a1,a4
	mv	a0,a5
	call	div10
	lw	a5,-36(s0)
	andi	a4,a5,0xff
	li	a5,1048576
	addiw	a4,a4,48
	andi	a4,a4,0xff
	sb	a4,0(a5)
	lw	a5,-20(s0)
	sw	a5,-36(s0)
.L7:
	lw	a5,-36(s0)
	bnez	a5,.L8
	li	a5,1048576
	li	a4,10
	sb	a4,0(a5)
.L9:
	nop
	ld	ra,40(sp)
	ld	s0,32(sp)
	addi	sp,sp,48
	jr	ra
main:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	addi	s0,sp,32
	sw	zero,-20(s0)
	li	a5,1
	sw	a5,-24(s0)
	sw	zero,-28(s0)
	j	.L11
.L12:
	lw	a5,-20(s0)
	mv	a0,a5
	call	print
	lw	a4,-20(s0)
	lw	a5,-24(s0)
	addw	a5,a4,a5
	sw	a5,-32(s0)
	lw	a5,-24(s0)
	sw	a5,-20(s0)
	lw	a5,-32(s0)
	sw	a5,-24(s0)
	lw	a5,-28(s0)
	addiw	a5,a5,1
	sw	a5,-28(s0)
.L11:
	lw	a5,-28(s0)
	sext.w	a4,a5
	li	a5,9
	ble	a4,a5,.L12
	li	a5,0
	mv	a0,a5
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	jr	ra
