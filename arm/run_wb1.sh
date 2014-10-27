extra=livein+half

for name in wb_txrate5a                                           
do
    for type in stoch hybrid
    do
    for mode in s #o                                                                
    do
        for t in 1 2
        do
        echo "$name mode=$mode"
        racket optimize.rkt --live-out 1 --live-in 1 --dead-mem --$type -$mode -c 8 -t 3600 -d results/$name-$type+$extra-$mode-$t programs/$name.s > results/$name-$type+$extra-$mode-$t.log
        done
    done
    done
done

sh dummy.sh