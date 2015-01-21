for name in p10_nlz_eq_o0 p11_nlz_lt_o0 p12_nlz_le_o0 p16_max_o0 p18_is_power_o0
do
    bash run.sh $name 3600
done

sh dummy.sh