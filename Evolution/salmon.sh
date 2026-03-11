#!/bin/sh 
#SBATCH --partition=serial_requeue  # partition (queue) (serial_requeue is for jobs that can be preempted)  
#SBATCH -c 12                        # this specifies the number of cores 
#SBATCH --mem=480G                  # this specifies memory in Mb, i.e. 120Gb in this case 
#SBATCH --time=3-00:00:00             # this specifies a job length of two days 
#SBATCH -J amigera                    # add a short job name here 
#SBATCH -o amigera_%A.out        # stdout log   
#SBATCH -e amigera_%A.err        # stderr log 
#SBATCH --mail-type=ALL             # send email on job start, end, and fail 

module load Miniforge3
source activate meta
cd /n/netscratch/aowens_lab/Lab/qtang/amigera_RNA/
#datasets download genome accession GCF_030705265.1 --include genome,gff3
#unzip ncbi_dataset.zip
#gffread -w reference_transcriptome.fa -g ncbi_dataset/data/GCF_030705265.1/GCF_030705265.1_ASM3070526v1_genomic.fna ncbi_dataset/data/GCF_030705265.1/genomic.gff

# ================= CONFIGURATION =================
# Path to your reference transcriptome (FASTA format)
REF="reference_transcriptome.fa"
# Directory for the Salmon index
INDEX="salmon_index"
# Number of threads to use
THREADS=12
# List of SRA IDs
SRA_FILE="sra_list.txt"
# =================================================

# 1. Build Salmon Index (Only needs to be done once)
if [ ! -d "$INDEX" ]; then
    echo "Building Salmon Index..."
    salmon index -t $REF -i $INDEX
else
    echo "Index found. Skipping build."
fi

# 2. Process each sample
mkdir -p fastq
mkdir -p quants

while read SRR; do
    echo "=========================================="
    echo "Processing sample: $SRR"
    echo "=========================================="

    # Step A: Download SRA data
    # 'prefetch' locates the file, 'fasterq-dump' extracts FASTQ
    prefetch $SRR
    fasterq-dump --split-3 --outdir fastq/ --progress $SRR

    # Step B: Quantify with Salmon (Assuming Paired-End data)
    # Note: If Single-End, remove -2 and use -r with the single fastq file
    salmon quant -i $INDEX -l A \
         -1 fastq/${SRR}_1.fastq \
         -2 fastq/${SRR}_2.fastq \
         -p $THREADS \
         --validateMappings \
         -o quants/${SRR}_quant

    # Optional: Clean up large FASTQ files to save space
    # rm fastq/${SRR}_*.fastq

done < $SRA_FILE

echo "Pipeline finished. Results are in 'quants/' folder."