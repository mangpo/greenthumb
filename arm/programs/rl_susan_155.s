	cmp	r0, #7
	movhi	r0, r2
	orrls	r0, r2, #1
	cmp	r8, #7
	movhi	r8, r5
	orrls	r8, r5, #1
	and	r1, r0, r1
	cmp	r9, #7
	movhi	r9, r3
	orrls	r9, r3, #1
	and	r5, r4, r5
	add	r5, r5, r1
	add	r4, r8, r4
	and	r2, r9, r2
	and	r3, r8, r3
	add	r4, r4, r0
	add	r5, r5, r2
	add	r4, r4, r9
	add	r3, r5, r3
	rsb	r4, r3, r4
