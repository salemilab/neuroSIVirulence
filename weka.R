library('dplyr')
library('tidyverse')
library('stringr')
require('parallel')

numCores=detectCores()


# Feature selection
system(paste('java --add-opens \
                           java.base/java.lang=ALL-UNNAMED \
                           -Xmx1024m \
                           -cp /Applications/weka-3-8-6/weka.jar \
                           weka.filters.supervised.attribute.AttributeSelection \
                           -i SIVE_missing.arff \
                           -o SIVE_filtered.arff \
                           -E "weka.attributeSelection.WrapperSubsetEval \
                           -B weka.classifiers.trees.J48 \
                           -F 5 -T 0.01 -R 1 -E DEFAULT --" \
                           -S "weka.attributeSelection.BestFirst -D 1 -N 5"'), 
                     intern = TRUE)
                     
                     

#train the model with PART from Weka
file_list=list.files[pattern="train*.arff")
prefix="train_"

mclapply(file_list, function(x) {
model=gsub("$prefix(.+).arff", "\\1", x)
system(paste('java --add-opens \
                           java.base/java.lang=ALL-UNNAMED \
                           -Xmx1024m \
                           -cp /Applications/weka-3-8-6/weka.jar \
                           weka.classifiers.rules.PART \
                           -C 0.25 -M 2 -no-cv -t x \
                           -d $model'), 
                     intern = TRUE)
                     }, mc.cores=numCores)

# C calculated as follows:
# p = f +- z*sqrt( f*(1-f) / N )
# given f is number of iterations, z is confidence interval (say, 90%=1.64)
# and f is error rate (say, 5%), so
# 1.96*sqrt(0.05*(1-0.05)/100)



#command used for Validation and Application. Each separate validation and application set 
# (per monkey and per tissue and time point was individually entered in -T)
file_list=list.files[pattern="test*.arff")
prefix="test_"

mclapply(file_list, function(x) {
model=gsub("$prefix(.+).arff", "\\1", x)
system(paste('java --add-opens \
                                                 java.base/java.lang=ALL-UNNAMED \
                                                 -Xmx1024m \
                                                 -cp /Applications/weka-3-8-6/weka.jar \
                                                 weka.classifiers.rules.PART \
                                                 -l $model \
                                                 -T x'), 
                                           intern = TRUE)
                                           
