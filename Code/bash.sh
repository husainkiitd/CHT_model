#!/bin/bash
#PBS -N FortSan
#PBS -P cart
#PBS -M $USER@iitd.ac.in
#PBS -l select=1:ncpus=1
#PBS -l walltime=60:00:00
#PBS -q low
cd $PBS_O_WORKDIR
gfortran -c *.f
gfortran -o ibm *.o
./ibm
