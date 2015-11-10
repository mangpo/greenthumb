mov r0, r0
	cmp	r0, r1
	movge	r0, r1
	ubfx	r0, r0, #15, #16
