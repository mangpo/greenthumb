type=hybrid
mode=s
# for name in complexC
# do
#     for t in 1 2
#     do
# 	echo "$name $t"
# 	racket optimize.rkt --hybrid -$mode -c 32 -t 100 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
#     done
# done

#t=1

# name=complexC
# racket optimize.rkt --$type -$mode -c 8 -t 100 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

# name=rrotate
# racket optimize.rkt --$type -$mode -c 8 -t 1200 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

# name=interp
# racket optimize.rkt --$type -$mode -c 8 -t 1600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log

# name=iii
# racket optimize.rkt --$type -$mode -c 8 -t 3600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log


for name in shaf shag iii_ex
do
    for t in 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 8 -t 1000 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done

for name in fff ggg hhh
do
    for t in 2
    do
	echo "$name $t"
	racket optimize.rkt --hybrid -$mode -c 8 -t 3600 -d results/$name-$type-$mode-$t programs/$name.s > results/$name-$type-$mode-$t.log
    done
done


sh dummy.sh
