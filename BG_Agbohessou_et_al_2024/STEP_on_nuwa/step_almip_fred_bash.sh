#!/bin/sh
#SBATCH --job-name=step_shl
#SBATCH --cpus-per-task=20
#SBATCH --output="step_almip-%j"
#SBATCH --error=error
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-type=fail
#SBATCH --mail-user=ayulrich@yahoo.fr

# access to conda
source /home/sila/miniconda3/etc/profile.d/conda.sh

# activate my environment
conda activate r_agbyenv

echo "BEGIN"

# the command lines I want to run on the cluster
Rscript step_almip_fred.R

echo "waiting"

echo "END"
