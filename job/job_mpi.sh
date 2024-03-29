#!/bin/bash
#
#   To submit,
#      ./qsub job_mpi.sh
#
#PBS -q large
#PBS -l select=4:ncpus=32:mpiprocs=32
#PBS -l walltime=05:00:00
#PBS -N may14c
#  #PBS -o stdoe
#PBS -j oe

source /etc/profile.d/modules.sh
module load compiler mpi
cd ${PBS_O_WORKDIR}
mpiexec -n 128 ../src/smoke_ring ../src/params.namelist
