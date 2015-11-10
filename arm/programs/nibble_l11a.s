	mov	r5, r1, asl #2
	mov	r10, r0, lsr r2
	mov	r4, r0, lsr r5
	and	r10, r10, #15
	and	r4, r4, #15
	cmp	r10, r4
	movlt	r1, r3
	add	r3, r3, #1
	movlt	r5, r2
	cmp	r4, r10
	movlt	r10, r4
	movge	r10, r10
	cmp	r3, #16
	add	r2, r2, #4
        
