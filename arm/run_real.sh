for name in rm_TxRateMatch_5a rm_TxRateMatch_5b rs_TxRateMatch_98a rm_bitarray_1 rl_bitarray_2 rm_bitarray_3 rm_bitcnts_2 rl_bitcnt_2_0 
do 
    bash run.sh $name 3600 hybrid p
done

bash dummy.sh
