#!/bin/sh 
#SBATCH --partition=serial_requeue  # partition (queue) (serial_requeue is for jobs that can be preempted)  
#SBATCH -c 12                        # this specifies the number of cores 
#SBATCH --mem=480G                  # this specifies memory in Mb, i.e. 120Gb in this case 
#SBATCH --time=3-00:00:00             # this specifies a job length of two days 
#SBATCH -J CEW                    # add a short job name here 
#SBATCH -o CEW_%A.out        # stdout log   
#SBATCH -e CEW_%A.err        # stderr log 
#SBATCH --mail-type=ALL             # send email on job start, end, and fail 

module load Miniforge3
source activate T2

## Set working directory
cd /n/netscratch/aowens_lab/Lab/qtang/SNPA/CEW/results/GCA_022581195.1/analyses

## Remove females
plink --vcf CEW_raw.vcf.gz --threads 12 --allow-extra-chr --allow-no-sex --remove remove.txt --out CEW --recode vcf

# Split variant and invariant sites
vcftools --vcf CEW.vcf --max-maf 0 --recode --stdout | bgzip -c > CEW_invariant.vcf.gz

vcftools --vcf CEW.vcf --mac 1 --max-missing 0.8 --recode --stdout | bgzip -c > CEW_variant.vcf.gz

tabix CEW_invariant.vcf.gz

tabix CEW_variant.vcf.gz

# Concatenate variant and invariant sites

# Windowed pi
vcftools --vcf CEW.vcf --keep light_vcftools.txt --window-pi 10000 --out CEW_pi_light_10kb

vcftools --vcf CEW.vcf --keep phero_vcftools.txt --window-pi 10000 --out CEW_pi_phero_10kb 

# Windowed Tajima's D
vcftools --vcf CEW.vcf --keep light_vcftools.txt --TajimaD 10000 --out CEW_TD_light_10kb

vcftools --vcf CEW.vcf --keep phero_vcftools.txt --TajimaD 10000 --out CEW_TD_phero_10kb

# Windowed fst
vcftools --vcf CEW.vcf --weir-fst-pop light_vcftools.txt --weir-fst-pop phero_vcftools.txt --fst-window-size 10000 --out light_vs_phero_FST_10kb

# Make some subsets with different filters for PCA
plink --vcf CEW_variant.vcf.gz --threads 12 --allow-extra-chr --allow-no-sex --out CEW_g2 --recode vcf

plink --vcf CEW_g2.vcf --threads 12 --allow-extra-chr --allow-no-sex --geno 0 --out CEW_g0 --recode vcf

plink --vcf CEW_g2.vcf --threads 12 --allow-extra-chr --allow-no-sex --maf 0.05 --out CEW_g2m5 --recode vcf

plink --vcf CEW_g0.vcf --threads 12 --allow-extra-chr --allow-no-sex --maf 0.05 --out CEW_g0m5 --recode vcf

plink2 --vcf CEW_g2.vcf --threads 12 --allow-extra-chr --set-all-var-ids --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

plink --vcf CEW_g2.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_g2L --recode vcf

plink2 --vcf CEW_g0.vcf --threads 12 --allow-extra-chr --set-all-var-ids --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

plink --vcf CEW_g0.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_g0L --recode vcf

plink2 --vcf CEW_g2m5.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

plink --vcf CEW_g2m5.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_g2m5L --recode vcf

plink2 --vcf CEW_g0m5.vcf --threads 12 --allow-extra-chr --allow-no-sex --indep-pairwise 50 10 0.01 --bad-ld

plink --vcf CEW_g0m5.vcf --extract plink2.prune.in --allow-extra-chr --allow-no-sex --out CEW_g0m5L --recode vcf

for i in *.vcf
do
plink --vcf $i --threads 12 --allow-extra-chr --allow-no-sex --pca --out ${i}_pca
done

conda deactivate

