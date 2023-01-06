setwd('/Users/andrea.ramirez/OneDrive - University of Florida/EPI-HomeDrive/Documents/PhD/ML_PAPER/NEW_paper/checking_rules')
library('Biostrings')
library('dplyr')
library('tidyverse')
library('stringr')
require(parallel)

numCores=detectCores()

seqs = readAAStringSet('all_monkeys.aa.uniqueseq.fasta')

# Transforming sequence data into character vectors
seqs_v = mclapply(1:length(seqs), function(i) {
  t(unlist(str_split(as.character(seqs[i]), pattern = '')))
}, mc.cores=numCores)
names(seqs_v) = names(seqs)

# Initiating a data frame that will be populated later with metadata
df = data.frame(id=names(seqs)) %>%
  dplyr::mutate(class = ifelse(grepl( 'D03|D04|D05|N10|H880|H882|H886', id),
                              "SIVE", "SIVnoE"),
                training = ifelse(grepl('H880|H882|H886|TP1|TP2|DT1|DT2|Zc|ZR|AR|Ac|FR|Fc|TR|Tc', id),
                                                     "training", "application"))

# Initiating list of vectors with biochemical/biophysical data for each amino acid
AA_index=list()
AA_index[['A']] = '-0.591,-1.302,-0.733,-0.146'
AA_index[['C']] = '-1.343,0.465,-0.862,-0.255'
AA_index[['D']] = '1.050,0.302,-3.656,-3.242'
AA_index[['E']] = '1.357,-1.453,1.477,-0.837'
AA_index[['F']] = '-1.006,-0.590,1.891,0.412'
AA_index[['G']] = '-0.384,1.652,1.330,2.064'
AA_index[['H']] = '0.336,-0.417,-1.673,-0.078'
AA_index[['I']] = '-1.239,-0.547,2.131,0.816'
AA_index[['K']] = '1.831,-0.561,0.533,1.648'
AA_index[['L']] = '-1.019,-0.987,-1.505,-0.912'
AA_index[['M']] = '-0.663,-1.524,2.219,1.212'
AA_index[['N']] = '0.945,0.828,1.299,0.933'
AA_index[['P']] = '0.189,2.081,1.628,1.392'
AA_index[['Q']] = '0.931,0.179,3.005,1.853'
AA_index[['R']] = '1.538,0.055,1.502,2.897'
AA_index[['S']] = '-0.228,1.399,-4.760,-2.647'
AA_index[['T']] = '-0.032,0.326,2.213,1.313'
AA_index[['V']] = '-1.337,-0.279,-0.544,-1.262'
AA_index[['W']] = '-0.595,0.009,0.672,-0.184'
AA_index[['Y']] = '0.260,0.830,3.097,1.512'
AA_index[['-']] = '0,0,0,0'
AA_index[['X']] = '0,0,0,0'

AA_index = lapply(AA_index, function(x) {
  y= str_split(x, ",")[[1]]
  return(y)
})

# Transforming AA_index table into a more usable data frame
feats = mclapply(seqs_v, function(seq) {
  P = unlist(lapply(1:length(seq), function(aa) {
    as.numeric(AA_index[names(AA_index) == seq[aa]][[1]][1])
  }))
  SS = unlist(lapply(1:length(seq), function(aa) {
    as.numeric(AA_index[names(AA_index) == seq[aa]][[1]][2])
  }))
  MS = unlist(lapply(1:length(seq), function(aa) {
    as.numeric(AA_index[names(AA_index) == seq[aa]][[1]][3])
  }))
  EC = unlist(lapply(1:length(seq), function(aa) {
    as.numeric(AA_index[names(AA_index) == seq[aa]][[1]][4])
  }))
  return(data.frame(P=P, SS=SS, MS=MS, EC=EC))
}, mc.cores=numCores)

# Setting rules and site number variables
r1s1=415
r2s1=415
r2s2=99
r3s1=99

# Assigning rules to each sequence based on identity (from seq_v) and feature (feats)
rules = do.call(rbind, mclapply(1:length(feats), function(x) {
  rule = data.frame(id=names(seqs_v)[x], rule=NA)
  if (isTRUE(seqs_v[[x]][r1s1]=='W' & feats[[x]]$P[r1s1]<=-0.279)){
    rule$rule="1_01"
  }
  if (isTRUE(is.na(rule$rule) &
      feats[[x]]$SS[r2s1]>-0.987 & feats[[x]]$SS[r2s1]>-0.987)) {
    rule$rule="1_02"
  }
  if (isTRUE(is.na(rule$rule) &
             feats[[x]]$SS[r2s1]>-0.987 & feats[[x]]$SS[r2s1]>-0.987)) {
    rule$rule="1_03"
  }
  return(rule)
}, mc.cores=numCores)) 

# Merging data into single data frame and saving  as csv file.
final_df = left_join(df, rules, by="id")
write.csv(final_df, "rules_by_seq.csv", quote=F, row.names=F)



