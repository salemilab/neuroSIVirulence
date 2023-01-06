require(parallel)
require(dplyr)

numCores=detectCores()

names_file="periphery_name_order"
arff_file="SIVE_periphery_missing.arff"
#skip_lines=16
skip_lines=2463
prefix="app_"

names = read.delim(names_file, header=F, sep='\t', col.names="id") %>%
  dplyr::mutate(animal = gsub('.*(D03|D04|D05|D06|H880|H882|H886|DT1|DT2|TP1|TP2|N01|N02|N03|N04|N05|N09|N10|N12|JA41).*', "\\1", id))

animals = unique(names$animal)

system2(command="head", args = c("-n ", skip_lines, arff_file), stdout = "arff_header")

arff = read.delim(arff_file, sep=',', skip=skip_lines, header=F) %>%
  mutate(animal=names$animal) %>%
  group_by(animal) %>%
  group_split()

names(arff) = animals

if (prefix=="test_") {
  results <- lapply(animals, function(i) {
    loo = dplyr::select(do.call(rbind, arff[names(arff) != i]), -animal)
    return(loo)
  })
  
} else {
  results = lapply(arff, function(i) {
    dplyr::select(i, -animal)})
}

for (i in 1:length(results)) {
  names(results)[i] = paste0(prefix, animals[i], "_out")
}

mclapply(1:length(results), function(x) {
  write.table(results[[x]], paste0(names(results)[x]), sep=',', quote=F, row.names=F, col.names=F)
}, mc.cores=numCores)

files = list.files(pattern=paste0(prefix))

lapply(files, function(x) {
  system2(command="cat", args = c("arff_header", x), stdout = paste0(x, ".arff"))
})

system2(command="rm", args = c("test*out"))
system2(command="rm", args = c("app*out"))




