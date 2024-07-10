#!/bin/sh
#SBATCH --job-name=write_mto
#SBATCH --output="toto-%j"
#SBATCH --cpus-per-task=20
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=ayulrich@yahoo.fr

# access to conda
source /home/sila/miniconda3/etc/profile.d/conda.sh

# activate my environment
conda activate r_agbyenv

echo "BEGIN"
hostname
echo  "#######################################"

# the command lines I want to run on the cluster
Rscript write_mto_new.R

echo "waiting"
wait
echo "END"

