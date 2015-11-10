	cmp	r1, r8
	mov	r8, r6
	bicne	r0, r0, r9, asl r7
	add	r6, r8, #1
	orrne	r0, r0, r10, asl r7
	add	r7, r7, #4
	bicne	r0, r0, r9, asl r5
	orrne	r0, r0, r11, asl r5
        
