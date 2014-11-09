	movw	ip, #43691
	movt	ip, 10922
	movw	r3, #65524
	movt	r3, 65535
	smull	lr, ip, ip, r1
	mov	lr, r1, asr #31
	rsb	ip, lr, ip, asr #10
	add	ip, ip, ip, asl #1
	sub	r1, r1, ip, asl #11
