extra=livein+half

for name in p17_off_right p22_parity p22_parity_full p23_count p24_roundpower                                              
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

sh run_hd3_o3.sh
