#!/bin/bash
#SBATCH --job-name=Weka
#SBATCH --mail-user=brittany.rife@ufl.edu
#SBATCH --mail-type=FAIL,END,ABORT
#SBATCH --account=salemi
#SBATCH --qos=salemi
#SBATCH --output=out_weka_log_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=20gb
#SBATCH --time=90:00:00
##SBATCH --partition=gpu
##SBATCH --gpus=geforce:2
#SBATCH --array=1-17
#SBATCH --cpus-per-task=4

module load java/1.8.0_31
module load weka/3.8
 

file=$(ls test*.arff | sed -n ${SLURM_ARRAY_TASK_ID}p)


java weka.classifiers.rules.PART \
 -l SIVE \
 -T $file
