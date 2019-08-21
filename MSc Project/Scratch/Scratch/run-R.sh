# software stack under SGE. This version works with the modules
# environment upgraded in 2015.

# R Version 3.4.2

# 1. Request ten minutes of wallclock time (format hours:minutes:seconds).
#    Change this to suit your requirements.
#$ -l h_rt=72:0:0

# 2. Request 1 gigabyte of RAM. Change this to suit your requirements.
#$ -l mem=500G

# 3. Set the name of the job. You can change this if you wish.
#$ -N R_job_1

# 4. Set the working directory to somewhere in your scratch space.  This is
# a necessary step with the upgraded software stack as compute nodes cannot
# write to your $HOME.
#
# NOTE: this directory must exist.
#
# Replace "<your_UCL_id>" with your UCL user ID :)
#$ -wd /home/uczlhpe/Scratch/R_output
 
# 5. Your work *must* be done in $TMPDIR 
cd $TMPDIR

# 6. Run your R program.
module unload compilers
module unload mpi
module load r/recommended

R --no-save < /home/uczlhpe/Scratch/myR_job.R > myR_job.out

# 7. Preferably, tar-up (archive) all output files onto the shared scratch area
#    this will include the R_output file above.
tar zcvf $HOME/Scratch/R_output/files_from_job_$JOB_ID.tgz $TMPDIR

# Make sure you have given enough time for the copy to complete!
