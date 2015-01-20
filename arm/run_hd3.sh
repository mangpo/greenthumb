extra=livein+half

for name in p20_next_higher_o0 p25_smmul_o0
do
    for type in hybrid
    do
    for mode in s #o                                                                
    do
        for t in 1 2
        do
        echo "$name mode=$mode"
        racket optimize.rkt --live-out 0 --live-in 0,1,2,3 --dead-mem --$type -$mode -c 32 -t 3600 -d results/$name-$type+$extra-$mode-$t programs/$name.s > results/$name-$type+$extra-$mode-$t.log
        done
    done
    done
done

sh run_hd4.sh
