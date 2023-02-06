
prefix="train_"

file=$(ls train*.arff | sed -n ${SLURM_ARRAY_TASK_ID}p)
model=${file#"$prefix"}
model=${model%.arff}

java weka.classifiers.rules.PART \
 -C 0.25 -M 2 -no-cv -t $file \
 -d $model