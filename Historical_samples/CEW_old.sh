#!/bin/bash
#SBATCH -J CEW_old
#SBATCH -o CEW_old_out
#SBATCH -e CEW_old_err
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --time=3-00:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=ALL

module load Miniforge3
source activate snakemake-env
snakemake --executor slurm -s snpArcher/workflow/Snakefile -d CEW_old --workflow-profile CEW_old/workflow-profiles/default --jobs 50
conda deactivate
