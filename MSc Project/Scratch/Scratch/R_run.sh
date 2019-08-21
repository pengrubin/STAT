#!/bin/bash -l

#$ -S /bin/bash
#$ -pe mpi 32
#$ -l tmpfs=30G 
#$ -N clonealign 
#$ -l h_rt=48:0:0


#$ -wd /home/uczlhpe/Scratch/

#$ -m eas
#$ -M h.peng@ucl.ac.uk

module unload compilers
module unload mpi
module load r/recommended

R CMD BATCH /home/uczlhpe/Scratch/run.R
