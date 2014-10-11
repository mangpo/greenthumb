type=half
racket optimize.rkt --live-reg 0 --dead-mem --stoch -s -c 24 -t 60 -d /scratch/mangpo/results/p13_sign_o0-half-s-1 programs/p13_sign_o0.s > /scratch/mangpo/results/p13_sign_o0-half-s-1.log
for name in p14_floor_avg_o0 p15_ceil_avg_o0 p17_off_right_o0 p21_cycle_o p22_parity_o0 p23_count_o0 p24_roundpower_o0
do
    for mode in s o
    do
	racket optimize.rkt --live-reg 0 --dead-mem --stoch -$mode -c 24 -t 3600 -d /scratch/mangpo/results/$name-$type-$mode-1 programs/$name.s > /scratch/mangpo/results/$name-$type-$mode-1.log
    done
done

sh run4.sh
