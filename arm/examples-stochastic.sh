# Bit hack
racket optimize.rkt --live-reg 0 -s programs/md5.s
racket optimize.rkt --live-reg 0 -s programs/p13_sign.s
racket optimize.rkt --live-reg 0 -o programs/p16_max.s
racket optimize.rkt --live-reg 0 -o programs/p16_max2.s
racket optimize.rkt --live-reg 0 -s programs/p18_power.s

#  QCT's Sangwan
# assembly input
racket optimize.rkt --live-reg 2 -s programs/sangwan0.s
# llvm input
racket optimize.rkt --live-reg 15 -l -o programs/sangwan0.ll

racket optimize.rkt --live-reg 1 -s programs/sangwan1.s
racket full-asm-optimize.rkt -o -t 36000 programs/sangwan0-full.s programs/sangwan0-full.live

# Wireless signal processing kernel
# assembly input
racket optimize.rkt -o --live-reg 0,1,3,4,5,6,8 -t 36000 -d ntt16 programs/ntt.s
# llvm ir input
racket optimize.rkt --live-reg 2,3,7,8,26,27,28 -l -o programs/nttBB2.ll

racket optimize.rkt -o --live-reg 1,2,3,4,5,6,14 -t 36000 -d bcjr -n 29 programs/bcjr.s
racket full-asm-optimize.rkt -o -d bcjr_o -t 36000 programs/bcjr_full.s programs/bcjr_full.live