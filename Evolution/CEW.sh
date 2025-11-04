#!/bin/bash
#SBATCH -J CEW
#SBATCH -o CEW_out
#SBATCH -e CEW_err
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --time=3-00:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=ALL

module load Miniforge3
source activate snakemake-env
snakemake -s snpArcher/workflow/Snakefile --profile snpArcher/profiles/slurm
conda deactivate
