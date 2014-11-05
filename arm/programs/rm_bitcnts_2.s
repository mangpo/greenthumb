	and	r1, r3, #1
  cmp r3, r4
	add	r2, r2, #1
	add	r0, r0, r1
	moveq	r1, #0
	movne	r1, #1
	cmp	r2, #31
	movhi	r1, #0
	andls	r1, r1, #1
