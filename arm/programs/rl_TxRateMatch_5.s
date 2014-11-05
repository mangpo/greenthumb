	ldr	r1, [r4, #4]
	movw	ip, #43691
	movt	ip, 10922
	movw	r3, #65524
	movt	r3, 65535
	smull	lr, ip, ip, r1
	mov	lr, r1, asr #31
	rsb	ip, lr, ip, asr #10
	add	ip, ip, ip, asl #1
	sub	r1, r1, ip, asl #11
	str	r1, [r0, r2, asl #2]
	ldr	lr, [r4, #24]
	ldr	r1, [r5, #40]
	ldr	ip, [r5, #68]
	sub	lr, lr, #1
	ldr	r2, [r4, #36]
	str	r1, [r4, #8]
	str	ip, [r4, #12]
	ldr	r6, [r0, lr, asl #2]
	mov	r0, r2, asl #2
	rsb	r5, r0, r2
	mul	r2, r6, r2
	mov	r5, r5, asl #11
	sub	r5, r5, #12
	rsb	r3, r2, r3
	mla	r3, r5, lr, r3
	mla	r1, ip, r1, r3
