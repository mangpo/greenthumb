        cmp	r0, r1
	movne	r2, r3
	cmp	r0, r3
	eoreq	r0, r3, r1
	movne	r0, #0
	eor	r0, r2, r0

        sub	r3, r0, #1
	orr	r3, r3, r0
	add	r3, r3, #1
	and	r0, r3, r0