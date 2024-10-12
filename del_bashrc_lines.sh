#!/bin/bash

grep_out=$(grep -n $1 $2)
if [ $? ]
then
  line_num=$(echo $grep_out | cut -d ":" -f 1)
  sed -i $(( $line_num-1 )),"$line_num"d $2
fi
