	cmp	r0, r1
	movne	r2, r3
	cmp	r0, r3
	eoreq	r0, r3, r1
	movne	r0, #0
	eor	r0, r2, r0
        