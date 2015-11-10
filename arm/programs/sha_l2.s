	mov	r6, r4, ror #17
	eor	r6, r6, r4, ror #19
	mov	r1, r10, ror #17
	eor	r4, r6, r4, lsr #10
	eor	r1, r1, r10, ror #19
	add	r4, r4, r9
	mov	r7, r5, ror #7
	eor	r10, r1, r10, lsr #10
	mov	r6, r0, ror #7
	rsb	r1, r2, #62
	add	r2, r2, #2
	eor	r7, r7, r5, ror #18
	add	r10, r10, r11
	eor	r6, r6, r0, ror #18
	cmp	r2, #62
	eor	r7, r7, r5, lsr #3
	add	r4, r4, r8
	add	r5, r5, r10
	eor	r6, r6, r0, lsr #3
	add	r4, r4, r7
	add	r10, r5, r6
        
