        str     r0, [fp, #-24]
        ldr     r3, [fp, #-24]
        rsb     r3, r3, #0
        mov     r2, r3
        ldr     r3, [fp, #-24]
        and     r3, r2, r3
        str     r3, [fp, #-20]
        ldr     r2, [fp, #-24]
        ldr     r3, [fp, #-20]
        add     r3, r2, r3
        str     r3, [fp, #-16]
        ldr     r2, [fp, #-24]
        ldr     r3, [fp, #-16]
        eor     r3, r2, r3
        str     r3, [fp, #-12]
        ldr     r0, [fp, #-12]
        ldr     r1, [fp, #-20]
        bl      __aeabi_uidiv
        mov     r3, r0
        str     r3, [fp, #-8]
        ldr     r3, [fp, #-8]
        mov     r3, r3, lsr #2
        str     r3, [fp, #-8]
        ldr     r2, [fp, #-8]
        ldr     r3, [fp, #-16]
        orr     r3, r2, r3
        mov     r0, r3
