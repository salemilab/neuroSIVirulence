#!/bin/bash
#SBATCH --job-name=Weka_aatribute_selection
#SBATCH --mail-user= <user@user.com>
#SBATCH --mail-type=FAIL,END,ABORT
#SBATCH --account=<account name>
#SBATCH -o ./logs/Report.weka.%A_%a.out
#SBATCH --qos=salemi-b
#SBATCH --output=out_weka_log_%j
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=20gb
#SBATCH --time=90:00:00
#SBATCH --partition=<partition name>

module load java/1.8.0_31
module load weka/3.8

java weka.filters.supervised.attribute.AttributeSelection -b -i <name of input testing .arff file> -o <name of output testing .arff file> -r <name of input training .arff file> -s <name of output training .arff file> -c last -E "weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.trees.J48 -F 5 -T 0.01 -R 1 -E DEFAULT --" -S "weka.attributeSelection.BestFirst -D 1 -N 5"
