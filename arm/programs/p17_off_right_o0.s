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
	
