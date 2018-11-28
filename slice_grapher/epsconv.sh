#!/bin/sh

cd Workfiles

for i in `ls *.eps`; do
  echo converting $i
  convert -alpha Remove -density 300x300 $i ${i}conv.gif
done

#
# make a gif-animation
#
echo 'making animation.gif'
convert -delay 10 *conv.gif animation.gif

