x=slvr
for file in rrotate complexA complexB complexC interp shaf iii
do
    echo $file
    racket programs/$file.rkt > $file-$x-1.log
    mv output $file-$x-1
done
