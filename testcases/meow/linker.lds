SECTIONS {
    . = 0x80000000;
    .text 0x80000000 : {
        *(.text.init)
        *(.text.*)
    }
    .rodata : {
        __srodata_begin = .;
        *(.srodata)
        __rodata_begin = .;
        *(.rodata)
        *(.rodata.*)
    }
    .data : {
        *(.sdata)
        *(.data)
    }
    .bss : {
        *(.sbss)
        *(.bss)
    }

    /DISCARD/ : {
        *(.comment)
        *(.eh_frame)
        *(.riscv.attributes)
    }
}
