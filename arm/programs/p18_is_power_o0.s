        str     r0, [fp, #-24]
        ldr     r3, [fp, #-24]
        sub     r3, r3, #1
        str     r3, [fp, #-16]
        ldr     r2, [fp, #-16]
        ldr     r3, [fp, #-24]
        and     r3, r2, r3
        str     r3, [fp, #-16]
        ldr     r3, [fp, #-16]
        cmp     r3, #0
        movne   r3, #0
        moveq   r3, #1
        str     r3, [fp, #-12]
        ldr     r3, [fp, #-12]
        ldr     r2, [fp, #-24]
        cmp     r2, #0
        moveq   r2, #0
        andne   r2, r3, #1
        mov     r0, r2
