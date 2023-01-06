require(parallel)
require(dplyr)

numCores=detectCores()

names = read.delim("name_order", header=F, sep='\t', col.names="id") %>%
  mutate(animal = gsub('.*(D03|D04|D05|D06|H880|H882|H886|DT1|DT2|TP1|TP2|N01|N02|N03|N04|N05|N09|N10|N12).*', "\\1", id))

animals = unique(names$animal)

system2(command="head", args = c("-n 16", "SIVE_filtered.arff"), stdout = "arff_header")

arff = read.delim("SIVE_filtered.arff", sep=',', skip=16, header=F) %>%
  mutate(animal=names$animal) %>%
  group_by(animal) %>%
  group_split()

names(arff) = animals

results <- lapply(animals, function(i) {
  loo = dplyr::select(do.call(rbind, arff[names(arff) != i]), -animal)
  return(loo)
})

for (i in 1:length(results)) {
  names(results)[i] = paste0("test_", animals[i], "_out")
}

mclapply(1:length(results), function(x) {
  write.table(results[[x]], paste0(names(results)[x]), sep=',', quote=F, row.names=F, col.names=F)
}, mc.cores=numCores)

files = list.files(pattern="test_")

lapply(files, function(x) {
  system2(command="cat", args = c("arff_header", x), stdout = paste0(x, ".arff"))
})

system2(command="rm", args = c("test*out"))




