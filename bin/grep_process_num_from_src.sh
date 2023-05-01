#!/bin/bash

# 
# COLUM:  01       02   03   04   05 06 07 08 ...
#  LINE: integer <const> :: NPROC_X = 1  ! Comment line
#

function grep_nproc_x()
{
  cat $1 | grep '^ *integer <const> :: NPROC_X'  | awk '{print $6}'
}

function grep_nproc_y()
{
  cat $1 | grep '^ *integer <const> :: NPROC_Y'  | awk '{print $6}'
}

function grep_nproc_z()
{
  cat $1 | grep '^ *integer <const> :: NPROC_Z'  | awk '{print $6}'
}

src_file=../src/constants.ef

nproc_x=`grep_nproc_x $src_file`
nproc_y=`grep_nproc_y $src_file`
nproc_z=`grep_nproc_z $src_file`

total_nprocs=`echo "$nproc_x * $nproc_y * $nproc_z" | bc`

#>debug echo '#--------------------------------'
#>debug echo '# From '$src_file
#>debug echo '#   NPROC_X = ' $nproc_x
#>debug echo '#   NPROC_Y = ' $nproc_y
#>debug echo '#   NPROC_Z = ' $nproc_z
#>debug echo "# Total nprocs = $nproc_x * $nproc_y * $nproc_z = "
#>debug echo '#--------------------------------'

echo $total_nprocs


