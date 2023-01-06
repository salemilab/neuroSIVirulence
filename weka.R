library('dplyr')
library('tidyverse')
library('stringr')


# Feature selection
feature.selection = system(paste('java --add-opens \
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
train.model = system(paste('java --add-opens \
                           java.base/java.lang=ALL-UNNAMED \
                           -Xmx1024m \
                           -cp /Applications/weka-3-8-6/weka.jar \
                           weka.classifiers.rules.PART \
                           -C 0.10 -M 2 -no-cv -t /Applications/weka-3-8-6/data/vote.arff \
                           -d SIVE'), 
                     intern = TRUE)

# C calculated as follows:
# p = f +- z*sqrt( f*(1-f) / N )
# given f is number of iterations, z is confidence interval (say, 90%=1.64)
# and f is error rate (say, 5%), so
# 1.96*sqrt(0.05*(1-0.05)/100)



#command used for Validation and Application. Each separate validation and application set 
# (per monkey and per tissue and time point was individually entered in -T)
validate.model = system(paste('java --add-opens \
                                                 java.base/java.lang=ALL-UNNAMED \
                                                 -Xmx1024m \
                                                 -cp /Applications/weka-3-8-6/weka.jar \
                                                 weka.classifiers.rules.PART \
                                                 -l SIVE \
                                                 -T <.arff_file_for_testing_the_model>'), 
                                           intern = TRUE)
