
for name in rm_TxRateMatch_98 rl_TxRateMatch_5 rl_susan_391
do
    bash run_real_w.sh $name 3600 100
done

for name in rl_TxRateMatch_5 rl_susan_391
do
    bash run_real_w.sh $name 3600 15
done

for name in rm_TxRateMatch_98 rl_TxRateMatch_5 rl_susan_391
do
    bash run_real_w.sh $name 3600 7
done

bash dummy.sh