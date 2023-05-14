#!/bin/bash
#
# jsg.sh  
#   job script generator
#   by Akira Kageyama.
#
#   History
#    2023.05.12: Copied from crayfish.csh
#
#

#################################
  Viscous_diffusivity=1.e-2
  Thermal_diffusivity=1.e-2
  node_and_cpu='select=4:ncpus=32:mpiprocs=32'
  nprocs=128
#################################


function print_usage() {
cat<<EOF
  ------------------------------------------------------------------
  Usage: ./jsg.sh que time seq nloop
   e.g., ./jsg.sh large 03:00:00 000 10000
  ------------------------------------------------------------------
EOF
}

function make_namelist() {
     hour=`echo $elaps_time | cut -d: -f1`
      min=`echo $elaps_time | cut -d: -f2`
  max_min=`expr $hour \* 60 + $min`

cat<<EOF
!---------------------------------------------
  &data00        Data_dir_name = '$data_dir' /
  &data01             Job_name = '$job_name' /
  &data02              Job_seq = $current_seq /
  &data03      Nloops_this_job = $job_nloop /
  &data04  Viscous_diffusivity = $Viscous_diffusivity /
  &data05  Thermal_diffusivity = $Thermal_diffusivity /
!---------------------------------------------
EOF
}


function make_jobscript {
cat<<EOF
#!/bin/bash
#
#PBS -q $que
#PBS -l $node_and_cpu
#PBS -l walltime=$elaps_time
#PBS -N $current_seq
#PBS -j oe

source /etc/profile.d/modules.sh
module load compiler mpi
cd \${PBS_O_WORKDIR}
mpiexec -n $nprocs ../../src/smoke_ring ./$current_seq.namelist
EOF
}

function print_system_info() {
  if [ $this_host = "alf" ]; then         # Alfven
    echo alfven
  elif [ $this_host = "pif" ]; then    # Pi-computer
    grep -E '^pi-computer' jsg_computer_systems.txt
  elif [ $this_host = "ofp" ]; then    # Oakforest PACS
    echo ofp
  elif [ $this_host = "fes" ]; then    # NIFS
    echo fes
  else                            # default
    echo default
  fi
}



this_host=`hostname | cut -c1-3`

if [ $# -ne 4 ]; then
  print_system_info
  print_usage
  exit 0
fi

        que=$1
 elaps_time=$2
current_seq=`printf %03d $3`
  job_nloop=$4

   job_name=`pwd | rev | cut -d'/' -f1 | rev`
   data_dir=../../data/$job_name

mkdir -p $data_dir/restart
mkdir -p $data_dir/vis2d

make_namelist  > ./$current_seq.namelist
make_jobscript > ./$current_seq.js

