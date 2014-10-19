type=hybrid+2p+half

for name in p14_floor_avg_o0 p15_ceil_avg_o0 #p17_off_right_o0 #p21_cycle_o0 p22_parity_o0 p23_count_o0 p24_roundpower_o0                                              
do
    for mode in s #o                                                                
    do
        for t in 1 2
        do
        echo "$name mode=$mode"
        racket optimize.rkt --live-out 0 --dead-mem --hybrid -$mode -c 24 -t 150 -d /scratch/mangpo/results/$name-$type-$mode-$t programs/$name.s > /scratch/mangpo/results/$name-$type-$mode-$t.log
        done
    done
done

sh run_kaofang2.sh