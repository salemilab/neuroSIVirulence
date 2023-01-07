require(parallel)
require(dplyr)

numCores=detectCores()

names_file="CNS_name_order"
arff_file="SIVE_CNS_filtered.arff"
skip_lines=16
#skip_lines=2463
prefix="train_"

names = read.delim(names_file, header=F, sep='\t', col.names="id") %>%
  dplyr::mutate(animal = gsub('.*(D03|D04|D05|D06|H880|H882|H886|DT1|DT2|TP1|TP2|N01|N02|N03|N04|N05|N09|N10|N12|JA41).*', "\\1", id))

animals = unique(names$animal)

system2(command="head", args = c("-n ", skip_lines, arff_file), stdout = "arff_header")

arff = read.delim(arff_file, sep=',', skip=skip_lines, header=F) %>%
  mutate(animal=names$animal) %>%
  group_by(animal) %>%
  group_split()

names(arff) = animals

if (prefix=="train_") {
  train_set <- lapply(animals, function(i) {
    loo = dplyr::select(do.call(rbind, arff[names(arff) != i]), -animal)
    return(loo)
  })
  for (i in 1:length(train_set)) {
    names(train_set)[i] = paste0(prefix, animals[i], "_out")
  }
  mclapply(1:length(train_set), function(x) {
    write.table(train_set[[x]], paste0(names(train_set)[x]), sep=',', quote=F, row.names=F, col.names=F)
  }, mc.cores=numCores)
  
  files = list.files(pattern=paste0(prefix))
  
  lapply(files, function(x) {
    system2(command="cat", args = c("arff_header", x), stdout = paste0(x, ".arff"))
  })
  
  system2(command="rm", args = c("train*out"))
  
  
  test_set = lapply(arff, function(i) {
    dplyr::select(i, -animal)})
  
  for (i in 1:length(test_set)) {
    names(test_set)[i] = paste0("test_", animals[i])
  }
  
  mclapply(1:length(test_set), function(x) {
    write.table(test_set[[x]], paste0("../testing/", names(test_set)[x]), sep=',', quote=F, row.names=F, col.names=F)
  }, mc.cores=numCores)
  
  files = list.files(path="../testing", pattern="test", full.names=T)
  
  lapply(files, function(x) {
    system2(command="cat", args = c("arff_header", x), stdout = paste0(x, ".arff"))
  })
  
  system2(command="rm", args = c("../testing/test*"))
  
  
} else {
  app_set = lapply(arff, function(i) {
    dplyr::select(i, -animal)})
  
  mclapply(1:length(app_set), function(x) {
    write.table(app_set[[x]], paste0("../application/", names(app_set)[x]), sep=',', quote=F, row.names=F, col.names=F)
  }, mc.cores=numCores)
  
  files = list.files(path="../application", pattern="app", full.names=T)
  
  lapply(files, function(x) {
    system2(command="cat", args = c("arff_header", x), stdout = paste0(x, ".arff"))
  })
  
  system2(command="rm", args = c("app*"))
  
}








