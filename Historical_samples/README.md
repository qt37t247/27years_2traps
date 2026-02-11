# Re-analyses of historical corn earworm samples

We examined signals of positive selection on 35 wild caught individuals colleted from pheromone traps in 2002, 2012, and 2017 in Louisiana, USA.

The original paper for these samples are published in an article titled, "Genome evolution in an agricultural pest following adoption of transgenic crops" in PNAS 2021.
https://www-pnas-org/doi/abs/10.1073/pnas.2020853118

Raw reads of whole genome resequencing are deposited in NCBI under Bioproject PRJNA751583

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

The sample information is in /config/samples.csv (see "**samples.csv**" for this project). 

Adjust parameters in /profiles/slrum/config.yaml (see "**config.yaml**" for the parameters we used for this project) to optimize your capacity in slurm scheduler. 

Submit the slrum job ("**CEW_old.sh**").
```bash
sbatch CEW.sh
```

## Step 2. Estimate genome-wide signals of positive selection

We created a conda environment, named "GWAS", for this step. In this conda environment, we installed:

RAiSD (https://github.com/alachins/raisd)

Run these analyses using the code in "CEW_raisd_old.sh"
```bash
sbatch CEW_raisd_old.sh
```

Results of RAiSD is processed with the R script "**Visualize_RAiSD.R**". 


