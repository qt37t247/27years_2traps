#!/bin/sh 
#SBATCH --partition=serial_requeue  # partition (queue) (serial_requeue is for jobs that can be preempted)  
#SBATCH -c 12                        # this specifies the number of cores 
#SBATCH --mem=480G                  # this specifies memory in Mb, i.e. 120Gb in this case 
#SBATCH --time=3-00:00:00             # this specifies a job length of two days 
#SBATCH -J MW                    # add a short job name here 
#SBATCH -o MW_%A.out        # stdout log   
#SBATCH -e MW_%A.err        # stderr log 
#SBATCH --mail-type=ALL             # send email on job start, end, and fail 

module load Miniforge3

source activate GWAS

cd /n/netscratch/aowens_lab/Lab/qtang/CEW_old/results/GCA_022581195.1/analyses

## Remove possible bad individuals
plink --vcf CEW_old_raw.vcf.gz --threads 12 --allow-extra-chr --allow-no-sex --remove remove.txt --out CEW_old --recode vcf

# computing selective sweep
./RAiSD/raisd-master/RAiSD -n CEW_2002_sweep -I CEW_old.vcf -S RAiSD_SampleList.CEW_2002_sweep -G 3752 -f -P -A 0.995
./RAiSD/raisd-master/RAiSD -n CEW_2012_sweep -I CEW_old.vcf -S RAiSD_SampleList.CEW_2012_sweep -G 3752 -f -P -A 0.995
./RAiSD/raisd-master/RAiSD -n CEW_2017_sweep -I CEW_old.vcf -S RAiSD_SampleList.CEW_2017_sweep -G 3752 -f -P -A 0.995

source deactivate
