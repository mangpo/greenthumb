	movw	r11, #43691
	movt	r11, 10922
	movw	r3, #65524
	movt	r3, 65535
	smull	r10, r11, r11, r1
	mov	r10, r1, asr #31
	rsb	r11, r10, r11, asr #10
	add	r11, r11, r11, asl #1
	sub	r1, r1, r11, asl #11