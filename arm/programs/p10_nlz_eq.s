	eor	r3, r1, r0
	and	r0, r1, r0
	cmp	r3, r0
	movhi	r0, #0
	movls	r0, #1
