for name in fir_l5 sha_l2 sha_l3 sha_l4 nibble_read nibble_write nibble_l11a nibble_l11b
do 
    bash run.sh $name 3600 hybrid p
done

bash dummy.sh
