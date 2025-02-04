#!/bin/sh 
#SBATCH --partition=serial_requeue  # partition (queue) (serial_requeue is for jobs that can be preempted)  
#SBATCH -c 12                        # this specifies the number of cores 
#SBATCH --mem=120G                  # this specifies memory in Mb, i.e. 120Gb in this case 
#SBATCH --time=3-00:00:00             # this specifies a job length of two days 
#SBATCH -J CEW                    # add a short job name here 
#SBATCH -o CEW_%A.out        # stdout log   
#SBATCH -e CEW_%A.err        # stderr log 
#SBATCH --mail-type=ALL             # send email on job start, end, and fail 

module load Miniforge3
source activate T2

## Set working directory
cd /n/netscratch/aowens_lab/Lab/qtang/SNPA/CEW/results/GCA_022581195.1/analyses

# Split variant and invariant sites
vcftools --vcf CEW_raw.vcf --max-maf 0 --recode --stdout | bgzip -c > CEW_invariant.vcf.gz

vcftools --vcf CEW_raw.vcf --mac 1 --max-missing 0.8 --recode --stdout | bgzip -c > CEW_variant.vcf.gz

tabix CEW_invariant.vcf.gz

tabix CEW_variant.vcf.gz

# Concatenate variant and invariant sites
bcftools concat --threads 12 --allow-overlaps CEW_variant.vcf.gz CEW_invariant.vcf.gz -O z -o CEW_filtered.vcf.gz

tabix CEW_filtered.vcf.gz

./PopLDdecay/bin/PopLDdecay -InVCF CEW_filtered.vcf.gz -OutStat LDdecay

pixy --stats pi fst dxy --vcf CEW_raw.vcf.gz --populations CEW_popfile.txt --window_size 100000 --n_cores 12 --bypass_invariant_check 'yes'

#plink --vcf CEW_variant.vcf.gz --threads 12 --allow-extra-chr --allow-no-sex --out CEW_auto_g2 --recode vcf

#plink --vcf CEW_auto_g2.vcf --threads 12 --allow-extra-chr --allow-no-sex --geno 0 --out CEW_auto_g0 --recode vcf

#plink --vcf CEW_auto_g2.vcf --threads 12 --allow-extra-chr --allow-no-sex --maf 0.05 --out CEW_auto_g2m5 --recode vcf

#plink --vcf CEW_auto_g0.vcf --threads 12 --allow-extra-chr --allow-no-sex --maf 0.05 --out CEW_auto_g0m5 --recode vcf

#plink2 --vcf CEW_auto_g2.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

#plink --vcf CEW_auto_g2.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_auto_g2L --recode vcf

#plink2 --vcf CEW_auto_g0.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

#plink --vcf CEW_auto_g0.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_auto_g0L --recode vcf

#plink2 --vcf CEW_auto_g2m5.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

#plink --vcf CEW_auto_g2m5.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_auto_g2m5L --recode vcf

#plink2 --vcf CEW_auto_g0m5.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

#plink --vcf CEW_auto_g0m5.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_auto_g0m5L --recode vcf

#for i in *.vcf
#do
#plink --vcf $i --threads 12 --allow-extra-chr --allow-no-sex --pca --out ${i}_pca
#done

#plink --vcf CEW_auto_g2m5L.vcf --threads 12 --allow-extra-chr --allow-no-sex --out CEW_auto_adm --recode12

#for K in 1 2 3 4 5
#do
#/hpctmp/dbstq/dist/admixture_linux-1.3.0/admixture -j12 --cv CEW_auto_adm.ped $K | tee log${K}.out
#done

source deactivate


