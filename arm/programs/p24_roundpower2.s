sub r0, r0, 1
clz r1, r0
mvn r0, 0
rsb r0, r0, r0, lsr r1
