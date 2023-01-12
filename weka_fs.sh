

java weka.filters.supervised.attribute.AttributeSelection \
 -i SIVE_missing.arff \
 -o SIVE_filtered.arff \
 -E "weka.attributeSelection.WrapperSubsetEval \
 -B weka.classifiers.trees.J48 \
 -F 5 -T 0.01 -R 1 -E DEFAULT --" \
 -S "weka.attributeSelection.BestFirst -D 1 -N 5"
 