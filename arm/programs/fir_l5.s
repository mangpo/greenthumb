	cmp	r0, #-1073741824
	movlt	r0, #-1073741824
	cmp	r0, r1
	movge	r0, r1
	ubfx	r0, r0, #15, #16
        
