extra=misal

for name in p14_floor_avg_o0 p15_ceil_avg_o0 #p17_off_right_o0 #p21_cycle_o0 p22_parity_o0 p23_count_o0 p24_roundpower_o0                                              
do
    for type in stoch #hybrid
    do
    for mode in s #o                                                                
    do
        for t in 1 2 3 4 5
        do
        echo "$name type=$type mode=$mode"
        racket optimize.rkt --live-out 0 --live-in 0,1 --dead-mem --$type -$mode -c 32 -t 400 -d results/$name-$type+$extra-$mode-$t programs/$name.s > results/$name-$type+$extra-$mode-$t.log
        done
    done
    done
done

sh dummy.sh
