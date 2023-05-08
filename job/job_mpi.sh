#!/bin/bash
#
#   To submit,
#      ./qsub job_mpi.sh
#
#PBS -q large
#PBS -l select=4:ncpus=32:mpiprocs=32
#PBS -l walltime=00:02:00
#PBS -N job_h
#PBS -o stdoe
#PBS -j oe

source /etc/profile.d/modules.sh
module load compiler mpi
cd ${PBS_O_WORKDIR}
mpiexec ../src/smoke_ring ../src/params.namelist
