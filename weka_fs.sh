#!/bin/bash
#SBATCH --job-name=Weka
#SBATCH --mail-user=brittany.rife@ufl.edu
#SBATCH --mail-type=FAIL,END,ABORT
#SBATCH --account=salemi
#SBATCH --qos=salemi
#SBATCH --output=out_weka_log_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=20gb
#SBATCH --time=90:00:00
##SBATCH --partition=gpu
##SBATCH --gpus=geforce:2


module load java/1.8.0_31
module load weka/3.8


java weka.filters.supervised.attribute.AttributeSelection \
 -i SIVE_missing.arff \
 -o SIVE_filtered.arff \
 -E "weka.attributeSelection.WrapperSubsetEval \
 -B weka.classifiers.trees.J48 \
 -F 5 -T 0.01 -R 1 -E DEFAULT --" \
 -S "weka.attributeSelection.BestFirst -D 1 -N 5"
 