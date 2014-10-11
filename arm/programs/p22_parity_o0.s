	str	r0, [fp, #-16]
	ldr	r3, [fp, #-16]
	mov	r3, r3, asr #1
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-16]
	eor	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r3, [fp, #-12]
	mov	r3, r3, asr #2
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-12]
	eor	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	movw	r3, #4369
	movt	r3, 4369
	and	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	mov	r3, r2
	mov	r3, r3, asl #4
	add	r3, r3, r2
	mov	r2, r3, asl #8
	add	r3, r3, r2
	mov	r2, r3, asl #16
	add	r3, r3, r2
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #28
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	and	r3, r3, #1
	mov	r0, r3
	
