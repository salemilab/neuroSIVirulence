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
#SBATCH --cpus-per-task=4

module load java/1.8.0_31
module load weka/3.8

java weka.classifiers.rules.PART \
 -C 0.10 -M 2 -no-cv -t SIVE_filtered.arff \
 -d SIVE