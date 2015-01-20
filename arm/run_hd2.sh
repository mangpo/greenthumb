extra=livein+half

for name in p10_nlz_eq_o0 p11_nlz_lt_o0 p12_nlz_le_o0 p16_max_o0 p18_is_power_o0
do
    for type in hybrid
    do
    for mode in s #o                                                                
    do
        for t in 1 2
        do
        echo "$name mode=$mode"
        racket optimize.rkt --live-out 0 --live-in 0 --dead-mem --$type -$mode -c 32 -t 3600 -d results/$name-$type+$extra-$mode-$t programs/$name.s > results/$name-$type+$extra-$mode-$t.log
        done
    done
    done
done

#sh run_hd3.sh
sh dummpy.sh
