extra=livein+half

for name in p17_off_right_o0 p21_cycle_o0 p22_parity_o0 p23_count_o0 p24_roundpower_o0                                              
do
    for type in stoch hybrid
    do
    for mode in s #o                                                                
    do
        for t in 1 2
        do
        echo "$name mode=$mode"
        racket optimize.rkt --live-out 0 --live-in 0 --dead-mem --$type -$mode -c 24 -t 3600 -d results/$name-$type+$extra-$mode-$t programs/$name.s > results/$name-$type+$extra-$mode-$t.log
        done
    done
    done
done

sh run_kaofang3.sh
