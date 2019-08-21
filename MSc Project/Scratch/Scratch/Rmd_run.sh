#!/bin/bash -l

#$ -S /bin/bash
#$ -l h_rt=72:0:0
#$ -l mem=500G

#$ -pe smp 1

#$ -N clonealign
#$ -wd /home/uczlhpe/Scratch/

module unload compilers
module unload mpi
module load r/recommended

R CMD BATCH /home/uczlhpe/Scratch/Rmd_run.R
