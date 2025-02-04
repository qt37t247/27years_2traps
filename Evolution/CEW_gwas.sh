#!/bin/sh 
#SBATCH --partition=serial_requeue  # partition (queue) (serial_requeue is for jobs that can be preempted)  
#SBATCH -c 12                        # this specifies the number of cores 
#SBATCH --mem=480G                  # this specifies memory in Mb, i.e. 120Gb in this case 
#SBATCH --time=3-00:00:00             # this specifies a job length of two days 
#SBATCH -J MW                    # add a short job name here 
#SBATCH -o MW_%A.out        # stdout log   
#SBATCH -e MW_%A.err        # stderr log 
#SBATCH --mail-type=ALL             # send email on job start, end, and fail 

module load Mambaforge

source activate GWAS

cd /n/netscratch/aowens_lab/Lab/qtang/SNPA/CEW/results/GCA_022581195.1/analyses

# quick and dirty GWAS
vcf2gwas -T 12 -M 480000 -v CEW_variant.vcf.gz -pf pheno.csv -p 1 -lm 4 -o CEW_light_phero

source deactivate
