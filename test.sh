#!/bin/sh
#This script runs the Rust and Haskell implementations of Wring and compares
#their outputs. They should be identical.

tempname=temp$$.
key="keyκλειςמפתחключ"

for nbytes in 8 9 11 16 25 27 32 49 64 81 121 125 128 243 256 343 512 625 729 \
	      1024 1331 2048 2187 2401 3125 4096 6561 8192 14641 15625 16384 16807 \
	      19683 32768 59049 65536 78125 117649 131072 161051 177147 262144 \
	      390625 524288 531441
do
  tn=${tempname}${nbytes}.
  dd if=/dev/urandom of=${tn}orig bs=1 count=$nbytes
  stack run -- -k $key -e ${tn}orig -o ${tn}he
  cargo run -- -k $key -e ${tn}orig -o ${tn}re
  stack run -- -k $key -d ${tn}he -o ${tn}hehd
  cargo run -- -k $key -d ${tn}he -o ${tn}herd
  stack run -- -k $key -d ${tn}re -o ${tn}rehd
  cargo run -- -k $key -d ${tn}re -o ${tn}rerd
  diff ${tn}orig ${tn}hehd >/dev/null; same0=$?
  diff ${tn}orig ${tn}herd >/dev/null; same1=$?
  diff ${tn}orig ${tn}rehd >/dev/null; same2=$?
  diff ${tn}orig ${tn}rerd >/dev/null; same3=$?
  same=`expr $same0 \| $same1 \| $same2 \| $same3`
  # expr says 1 is true, sh says 0 is true
  if [ $same = 0 ]
  then
    echo Test $nbytes passed
    rm ${tn}*
  else
    echo Test $nbytes failed
  fi
done
