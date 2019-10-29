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
print:
	addi	sp,sp,-64
	sd	ra,56(sp)
	sd	s0,48(sp)
	addi	s0,sp,64
	mv	a5,a0
	sw	a5,-52(s0)
	addi	a5,s0,-40
	sd	a5,-24(s0)
	lw	a5,-52(s0)
	bnez	a5,.L7
	li	a5,1048576
	li	a4,48
	sb	a4,0(a5)
	li	a5,1048576
	li	a4,10
	sb	a4,0(a5)
	j	.L11
.L8:
	addi	a4,s0,-28
	addi	a5,s0,-52
	mv	a1,a4
	mv	a0,a5
	call	div10
	lw	a5,-52(s0)
	andi	a5,a5,0xff
	addiw	a5,a5,48
	andi	a4,a5,0xff
	ld	a5,-24(s0)
	sb	a4,0(a5)
	ld	a5,-24(s0)
	addi	a5,a5,1
	sd	a5,-24(s0)
	lw	a5,-28(s0)
	sw	a5,-52(s0)
.L7:
	lw	a5,-52(s0)
	bnez	a5,.L8
	j	.L9
.L10:
	ld	a5,-24(s0)
	addi	a5,a5,-1
	sd	a5,-24(s0)
	li	a5,1048576
	ld	a4,-24(s0)
	lbu	a4,0(a4)
	sb	a4,0(a5)
.L9:
	addi	a5,s0,-40
	ld	a4,-24(s0)
	bne	a4,a5,.L10
	li	a5,1048576
	li	a4,10
	sb	a4,0(a5)
.L11:
	nop
	ld	ra,56(sp)
	ld	s0,48(sp)
	addi	sp,sp,64
	jr	ra
	.size	print, .-print
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	addi	s0,sp,32
	sw	zero,-20(s0)
	li	a5,1
	sw	a5,-24(s0)
	sw	zero,-28(s0)
	j	.L13
.L14:
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
.L13:
	lw	a5,-28(s0)
	sext.w	a4,a5
	li	a5,9
	ble	a4,a5,.L14
	li	a5,0
	mv	a0,a5
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	li ra, 0x100000
	jr ra
