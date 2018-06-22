and.u32 	%r0, %r0, 31;
mul.wide.u32 	%r2, %r1, %r0, -1431655765;
shr.u32 	%r1, %r1, 1;
mul.lo.s32 	%r1, %r1, 3;
sub.s32 	%r1, %r0, %r1;
