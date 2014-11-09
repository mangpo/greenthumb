	mov	r7, r6, asl #1
	mov	r8, r6, asl #3
	add	r1, r6, r7
	mov	r9, r6, asl #2
	add	r0, r6, r1
	ldr	r13, [r10, #208]
	add	r12, r6, r0
	rsb	r5, r6, r8
	add	r4, r6, r12
	ldr	r11, [r10, #212]
	add	r0, r9, r0
	add	r12, r9, r12
	add	r4, r9, r4
	add	r1, r9, r1
	add	r2, r6, r9
	add	r3, r9, r7
	add	r12, r13, r12
	add	r0, r13, r0
	add	r1, r13, r1
	add	r11, r11, r5, asl #2
	add	r4, r13, r4
	str	r12, [r10, #172]
	str	r0, [r10, #160]
	add	r2, r13, r2
	str	r1, [r10, #176]
	add	r3, r13, r3
	add	r5, r13, r5
	add	r12, r13, r9
	sub	r0, r6, #7
	mov	r1, #7