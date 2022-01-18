  # only one core
  csrr a0, mhartid
  bnez a0, end

  li sp, 0x10000
  li ra, 0x100000
  j _start
print:
  addi	sp,sp,-64
  sd	s0,56(sp)
  addi	s0,sp,64
  mv	a5,a0
  sw	a5,-52(s0)
  addi	a5,s0,-40
  sd	a5,-24(s0)
  lw	a5,-52(s0)
  sext.w	a5,a5
  bne	a5,zero,.L4
  li	a5,268439552
  li	a4,48
  sb	a4,0(a5)
  li	a5,268439552
  li	a4,10
  sb	a4,0(a5)
  fence	iorw,iorw
  j	.L8
.L5:
  lw	a4,-52(s0)
  li	a5,10
  remw	a5,a4,a5
  sext.w	a5,a5
  andi	a5,a5,0xff
  addiw	a5,a5,48
  andi	a4,a5,0xff
  ld	a5,-24(s0)
  sb	a4,0(a5)
  ld	a5,-24(s0)
  addi	a5,a5,1
  sd	a5,-24(s0)
  lw	a4,-52(s0)
  li	a5,10
  divw	a5,a4,a5
  sw	a5,-52(s0)
.L4:
  lw	a5,-52(s0)
  sext.w	a5,a5
  bne	a5,zero,.L5
  j	.L6
.L7:
  ld	a5,-24(s0)
  addi	a5,a5,-1
  sd	a5,-24(s0)
  li	a5,268439552
  ld	a4,-24(s0)
  lbu	a4,0(a4)
  sb	a4,0(a5)
.L6:
  addi	a5,s0,-40
  ld	a4,-24(s0)
  bne	a4,a5,.L7
  li	a5,268439552
  li	a4,10
  sb	a4,0(a5)
.L8:
  nop
  ld	s0,56(sp)
  addi	sp,sp,64
  jr	ra

_start:
  addi	sp,sp,-32
  sd	ra,24(sp)
  sd	s0,16(sp)
  addi	s0,sp,32
  sw	zero,-20(s0)
  li	a5,1
  sw	a5,-24(s0)
  sw	zero,-28(s0)
  j	.L10
.L11:
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
.L10:
  lw	a5,-28(s0)
  sext.w	a4,a5
  li	a5,9
  ble	a4,a5,.L11
  li	a5,0
  mv	a0,a5
  ld	ra,24(sp)
  ld	s0,16(sp)
  addi	sp,sp,32

end:
  jr	ra
