library('dplyr')
library('tidyverse')
library('stringr')

#train the model with PART from Weka
train_model = system(paste('java --add-opens java.base/java.lang=ALL-UNNAMED -Xmx1024m -cp <path_to_weka.jar> weka.classifiers.rules.PART -C 0.25 -M 2 -no-cv -t <.arff_file_for_trining_the_model> -d <name_for_the_model>'), intern = TRUE)

#line used for Validation and Application. Each separate validation and application set (per monkey and per tissue and time point was individually entered in -T)
validation_or_application_dataset = system(paste('java --add-opens java.base/java.lang=ALL-UNNAMED -Xmx1024m -cp /Users/andrea.ramirez/Desktop/weka-3-8-5/weka.jar weka.classifiers.rules.PART -l <name_given_to_the_model> -T <.arff_file_for_testing_the_model>'), intern = TRUE)
