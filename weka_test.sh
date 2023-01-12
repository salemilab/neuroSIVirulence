 
prefix="test_"

file=$(ls test*.arff | sed -n ${SLURM_ARRAY_TASK_ID}p)
model=${file#"$prefix"}
model=${model%.arff}_out


java weka.classifiers.rules.PART \
 -l $model \
 -T $file
 
echo "$file"
