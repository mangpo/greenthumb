	mov	r6, r0, ror #17
	mov	r10, r2, ror #7
	eor	r6, r6, r0, ror #19
	eor	r0, r6, r0, lsr #10
	add	r4, r5, r4
	eor	r10, r10, r2, ror #18
	add	r0, r4, r0
	eor	r2, r10, r2, lsr #3
	add	r2, r0, r2
        
