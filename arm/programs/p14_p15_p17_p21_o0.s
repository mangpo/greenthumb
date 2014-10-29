	str	r4, [fp, #-16]
	str	r5, [fp, #-20]
	ldr	r12, [fp, #-16]
	ldr	r13, [fp, #-20]
	and	r13, r12, r13
	str	r13, [fp, #-12]
	ldr	r12, [fp, #-16]
	ldr	r13, [fp, #-20]
	eor	r13, r12, r13
	str	r13, [fp, #-8]
	ldr	r13, [fp, #-8]
	mov	r13, r13, asr #1
	str	r13, [fp, #-8]
	ldr	r12, [fp, #-12]
	ldr	r13, [fp, #-8]
	add	r13, r12, r13
	mov	r4, r13

 	str	r0, [fp, #-16]
	str	r4, [fp, #-20]
	ldr	r12, [fp, #-16]
	ldr	r13, [fp, #-20]
	orr	r13, r12, r13
	str	r13, [fp, #-12]
	ldr	r12, [fp, #-16]
	ldr	r13, [fp, #-20]
	eor	r13, r12, r13
	str	r13, [fp, #-8]
	ldr	r13, [fp, #-8]
	mov	r13, r13, asr #1
	str	r13, [fp, #-8]
	ldr	r12, [fp, #-12]
	ldr	r13, [fp, #-8]
	rsb	r13, r13, r12
	mov	r0, r13

        str	r0, [fp, #-24]
	str	r1, [fp, #-28]
	str	r2, [fp, #-32]
	str	r3, [fp, #-36]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-36]
	cmp	r2, r3
	movne	r3, #0
	moveq	r3, #1
	rsb	r3, r3, #0
	str	r3, [fp, #-20]
	ldr	r2, [fp, #-28]
	ldr	r3, [fp, #-36]
	eor	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-28]
	cmp	r2, r3
	movne	r3, #0
	moveq	r3, #1
	rsb	r3, r3, #0
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-32]
	ldr	r3, [fp, #-36]
	eor	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-16]
	and	r3, r2, r3
	str	r3, [fp, #-20]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	and	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-12]
	eor	r3, r2, r3
	str	r3, [fp, #-20]
	ldr	r2, [fp, #-20]
	ldr	r3, [fp, #-36]
	eor	r3, r2, r3
	mov	r0, r3

        str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	sub	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	and	r3, r2, r3
	mov	r0, r3

        