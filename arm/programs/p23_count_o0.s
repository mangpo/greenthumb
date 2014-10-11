	str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #1
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	movw	r3, #21845
	movt	r3, 21845
	and	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-12]
	rsb	r3, r3, r2
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-16]
	movw	r3, #13107
	movt	r3, 13107
	and	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #2
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	movw	r3, #13107
	movt	r3, 13107
	and	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-8]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #4
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-12]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-16]
	movw	r3, #3855
	movt	r3, 3855
	and	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #8
	ldr	r2, [fp, #-16]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #16
	ldr	r2, [fp, #-16]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r3, [fp, #-16]
	and	r3, r3, #63
	mov	r0, r3
	
