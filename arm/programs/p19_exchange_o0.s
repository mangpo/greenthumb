	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r2, [fp, #-16]
	ldr	r3, [fp, #-24]
	mov	r3, r2, asr r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-16]
	eor	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-20]
	and	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r2, [fp, #-12]
	ldr	r3, [fp, #-24]
	mov	r3, r2, asl r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-12]
	eor	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	eor	r3, r2, r3
	mov	r0, r3
	
