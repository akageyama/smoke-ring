#!/bin/bash

extract_used_obj()
{
  fortranfile=$1
  cut -d'!' -f1 ${fortranfile} \
             | grep '^ *use \+[a-zA-Z][_0-9a-zA-Z]\+_m\s*$' \
             | tr ',' ' ' \
             | awk '{print $2}' \
             | sed -e '/vv_[a-zA-z][0-9a-zA-Z]*/d' \
             | sed 's/\(.*\)_m/\1/' \
             | sed 's/$/.o/' \
             | tr '\012' ' '
}


for i in `ls -1 *.ef`
do
  #  ${target}: ${used}
  used=`extract_used_obj $i`
  target=`echo $i | sed 's/\(.*\).ef/\1.o/'`

  echo -n ${target}:
  for j in ${used}
    do
      echo -n ' '$j
    done
  echo
done
