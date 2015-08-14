	rsb	r1, r0, #0
	and	r1, r1, r0
	add	r4, r1, r0
	eor	r0, r4, r0
	bl	__aeabi_uidiv
	orr	r0, r4, r0, lsr #2
