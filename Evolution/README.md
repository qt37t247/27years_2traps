# Genome-wide association study

We performed whole genome sequencing on 48 individuals colleted from both blacklight traps and pheromone traps in summer 2024 in Delaware, USA.

Raw reads of whole genome resequencing are deposited in NCBI under Bioproject PRJNA1216269

We used Genome assembly ilHelZeax1.1 (Genbank Accession: GCA_022581195.1) as the reference genome to map genome-wide single nucleotide polymorphisms. 

All analyses was done in research computing (slrum scheduler) in the Faculty of Arts and Science at the Harvard University. 

## Step 1. Bioinformatics for variant calling

We used automated bioinformatic snakemake pipeline, SNPArcher.
https://github.com/harvardinformatics/snpArcher

See folder "SNPArcher" for customized configuration and rule files. 

You can download the rerun our pipeline by directly downloading the repository.
```bash
module load Mambaforge
mamba create -c conda-forge -c bioconda -n snakemake-env "snakemake>=8" "python==3.11.4"
mamba activate snakemake-env
git clone https://github.com/harvardinformatics/snpArcher.git
```

The sample information is in /config/samples.csv (see "**samples.csv**" for this project). You can fill the "BioProject" column with PRJNA1216269 to direct access the reads.

Adjust parameters in /profiles/slrum/config.yaml (see "**config.yaml**" for the parameters we used for this project) to optimize your capacity in slurm scheduler. 

Submit the slrum job ("**CEW.sh**").
```bash
sbatch CEW.sh
```

## Step 2. Data filtering and standard population genomic analyses (under construction...)

We created a conda environment, named "T2", for this step. In this conda environment, we installed:

vcftools (https://vcftools.sourceforge.net/)

bcftools (https://samtools.github.io/bcftools/)

plink (https://zzz.bwh.harvard.edu/plink/)

Run these analyses using the code in "**SNP_filter.sh**"
```bash
sbatch SNP_filter.sh
```


## Step 3. GWAS (under construction...)
We created a conda environment, named "GWAS", for this step. In this conda environment, we installed:

vcf2gwas (https://github.com/frankvogt/vcf2gwas)

Run these analyses using the code in "CEW_gwas.sh"
```bash
sbatch CEW_gwas.sh
```

