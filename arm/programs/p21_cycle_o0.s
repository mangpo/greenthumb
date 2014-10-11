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
	
