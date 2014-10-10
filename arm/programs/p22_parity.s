	eor	r0, r0, r0, asr #1
	movw	r3, #4369
	movt	r3, 4369
	eor	r0, r0, r0, asr #2
	and	r3, r0, r3
	add	r3, r3, r3, asl #4
	add	r3, r3, r3, asl #8
	add	r0, r3, r3, asl #16
        