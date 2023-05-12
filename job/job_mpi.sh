#!/bin/bash
#
#   To submit,
#      ./qsub job_mpi.sh
#
#PBS -q large
#PBS -l select=2:ncpus=32:mpiprocs=32

  ! 格子サイズのメモ
  ! NX_GLOBAL = 302  ! Vis=2e-3, Kap=2e-3: 1000 steps 発散
  ! NY_GLOBAL = 102  !
  ! NZ_GLOBAL = 102  !
#PBS -l walltime=05:00:00
#PBS -N job_h
#PBS -o stdoe
#PBS -j oe

source /etc/profile.d/modules.sh
module load compiler mpi
cd ${PBS_O_WORKDIR}
mpiexec -n 64 ../src/smoke_ring ../src/params.namelist
