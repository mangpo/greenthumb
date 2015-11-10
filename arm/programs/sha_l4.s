	mov	r9, r3, ror #6
	eor	r9, r9, r3, ror #11
	eor	r9, r9, r3, ror #25
	bic	r7, r4, r3
	add	r11, r11, r8
	and	r8, r5, r3
	add	r11, r11, r9
	eor	r7, r7, r8
	eor	r9, r10, r0
	mov	r8, r2, ror #2
	add	r7, r11, r7
	and	r9, r9, r2
	add	r1, r1, #4
	eor	r8, r8, r2, ror #13
	and	r11, r0, r10
	add	r6, r7, r6
	eor	r11, r9, r11
	eor	r8, r8, r2, ror #22
	cmp	r1, #256
	add	r11, r11, r8
	add	r9, r7, r6
	add	r11, r11, r6
        
