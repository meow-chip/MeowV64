  li sp, 0x10000
	li ra, 0x100000
.section .text
.globl _start
_start:
  # TODO: setup mtvec
  # Setup paging
  la t0, BOOT_PAGE_TBL_SV48
  srli t0, t0, 12
  li t1, 9 << 60 # SV48
  or t0, t0, t1
  csrw satp, t0
  sfence.vma

  la t0, BEGIN
  csrw mepc, t0
  li t0, (3 << 11)
  csrc mstatus, t0 # Set MPP -> 0 = User mode
  mret

  # Jump to first terrapage
BEGIN:
  la t0, FIRST
  li t1, (1<<39)
  add t0, t0, t1
  jr t0
  unimp
  unimp
  unimp
  unimp
  unimp

FIRST:
  # Jump to first terrapage
  la t0, SECOND # This will load by using AUIPC, so the difference here is only one terrapage
  li t1, (1<<39)
  add t0, t0, t1
  jr t0
  unimp
  unimp
  unimp
  unimp
  unimp

SECOND:
  la t0, THIRD
  li t1, (1<<39)
  add t0, t0, t1
  jr t0
  unimp
  unimp
  unimp
  unimp
  unimp

THIRD:
  la t0, LAST
  li t1, ((511 - 3)<<39)
  add t0, t0, t1
  jr t0
  unimp
  unimp
  unimp
  unimp
  unimp

LAST:
  nop
  nop
  nop
  nop
  ret

  .align 12
BOOT_PAGE_TBL_SV48:
  .quad (0x0 << 10) | 0xcf # SV48, VPN = 0x0 -> 0x0 512G terapage w/ VRWXAD
  .quad (0x0 << 10) | 0xcf # SV48, VPN = 0x1 -> 0x0 512G terapage w/ VRWXAD
  .quad (0x0 << 10) | 0xcf # SV48, VPN = 0x2 -> 0x0 512G terapage w/ VRWXAD
  .quad (0x0 << 10) | 0xcf # SV48, VPN = 0x2 -> 0x0 512G terapage w/ VRWXAD
  .zero 8 * 507
  .quad (0x0 << 10) | 0xcf # SV48, VPN = 0x1FF -> 0x0 512G terapage w/ VRWXAD
