library('Biostrings')
library('dplyr')
library('tidyverse')
library('stringr')
require(parallel)
require(ggplot2)
require(ggpubr)
<<<<<<< HEAD

=======
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435


rm(list=ls())
numCores=detectCores()

<<<<<<< HEAD
seqs = readAAStringSet('../FASTA/all_monkeys.aa.fasta.uniqueseq.fasta')
=======
setwd("/Users/macbook/Dropbox (UFL)/SIV/SIV_Machine_Learning/NEW_rules/application")

seqs = readAAStringSet('../../FASTA/all_monkeys.aa.fasta.uniqueseq.fasta')
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435

# Transforming sequence data into character vectors
seqs_v = mclapply(1:length(seqs), function(i) {
  t(unlist(str_split(as.character(seqs[i]), pattern = '')))
}, mc.cores=numCores)
names(seqs_v) = names(seqs)
rm(seqs)

# Initiating a data frame that will be populated later with metadata
df = data.frame(isolate=names(seqs_v)) %>%
  dplyr::mutate(class = ifelse(grepl( 'D03|D04|D05|N10|H880|H882|H886', isolate),
                              "SIVE", "SIVnoE"),
                training = ifelse(grepl('H880|H882|H886|TP1|TP2|DT1|DT2|Zc|ZR|AR|Ac|FR|Fc|TR|Tc', isolate),
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
AA_index[['-']] = 'NA,NA,NA,NA'
AA_index[['X']] = 'NA,NA,NA,NA'

AA_index = lapply(AA_index, function(x) {
  y= str_split(x, ",")[[1]]
  return(y)
})

# Transforming AA_index table into a more usable data frame
feats = mclapply(seqs_v, function(seq) {
  Site = 1:length(seq)
  ID = as.character(seq)
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
  return(data.frame(Site=Site, ID=ID, P=P, SS=SS, MS=MS, EC=EC))
}, mc.cores=numCores)

# Setting rules and site number variables

<<<<<<< HEAD
rules = read.delim("rules_sites.txt", sep='\t', header=T)
=======
rules = read.delim("../rules_sites.txt", sep='\t', header=T)
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
#  mutate(Feature = ifelse(Feature=="polarity", "P",
#                         ifelse(Feature=="molecular_size", "MS", Feature)))
#    spread(Feature, Value)

# rules_split = rules %>%
#   dplyr::group_by(Rule) %>%
#   group_split() %>%
#   setNames(unique(rules$Rule))



# No order of importance here, so need to figure out which rules occupy most sequences
# df = cbind(df,
#            data.frame(matrix(ncol = length(unique(rules$Rule)), 
#                              nrow = nrow(df),
#                              dimnames=list(NULL, unique(rules$Rule)))))

# Assigning rules to each sequence based on identity (from seq_v) and feature (feats)
# rules_found = do.call(rbind, mclapply(feats, function(seq) {
#   mclapply(rules_split, function(rule) {
#     for (seq$Site %in% rule$Site)
  
# Rule	Feature	Site	Value
# 1_01	ID	325	W
r1s1=325
# 1_01	ID	197	Q
r1s2=197
# 1_01	ID	317	E
r1s3=317
# 1_01	molecular_size	28 	> -3.656
r1s4=28

# 1_02	ID	325	W
# 1_02	ID	317	E
# 1_02	molecular_size	28 	> -3.656

# 1_03	ID	325	W
# 1_03	ID	317	E

# 1_04	ID	325	W
# 1_04	ID	197	Q
# 1_04	molecular_size	28 	> -3.656

# 1_05	ID	325	W
# 1_05	ID	197	Q

# 1_06	ID	197	Q
# 1_06	molecular_size	28 	<= -3.656

# 1_07	ID	317	K

# 2_01	ID	325	W
r2s1=325
# 2_01	ID	410	P
r2s2=410
# 2_01	ID	462	I
r2s3=462

# 3_01	ID	410	P
r3s1=410
# 3_01	ID	317	K
r3s2=317

#4_01	polarity	407 	> -0.032
r4s1=407
# 4_01	ID	197	Q
r4s2=197
# 4_01	ID	462	I
r4s3=462

#4_02	polarity	407 	> -0.032
#4_02	ID	197	Q

#4_03	polarity	407 	> -0.032
#4_03	ID	462	I

#5_01	ID	325	W
r5s1=325
# 5_01	ID	415	R
r5s2=415

# 5_02	ID	415	R

# 6_01	ID	317	E
r6s1=317
# 6_01	ID	462	I
r6s2=462


tbl.manip = function(new_df, df, rule_start, rule_end) {
<<<<<<< HEAD
  left_join(new_df, df) %>%
    gather(rule, value, rule_start:rule_end) %>%
    mutate(time = as.numeric(ifelse(grepl("JA41", isolate), 
=======
  tbl=dplyr::left_join(new_df, df) %>%
    dplyr::filter(training=="application") %>%
    gather(rule, value, rule_start:rule_end) %>%
    dplyr::mutate(time = as.numeric(ifelse(grepl("JA41", isolate), 
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
                                    gsub("JA41-M\\d_[A-Z]+_.+_(\\d+)_.+", "\\1", isolate),
                                    gsub("(\\d+).+", "\\1", isolate))),
           tissue = ifelse(grepl("JA41", isolate), 
                           gsub("JA41-M\\d_([A-Z]+)_.+_\\d+_.+", "\\1", isolate),
                           gsub("\\d+[A-Z]\\d+([A-Z]).+", "\\1", isolate)),
           animal = ifelse(grepl("JA41", isolate), "JA41",
                           gsub("\\d+([A-Z]\\d+)[A-Z].+", "\\1", isolate))) %>%
<<<<<<< HEAD
    group_by(animal) %>%
    mutate(time_nec = max(time, na.rm=T)) %>%
    mutate(time_nec = ifelse(time_nec==-Inf, Inf, time_nec)) %>%
    mutate(time_disc = ifelse(time<=21, "Early",
                              ifelse(time>21 & time < (time_nec-21), "Chronic",
                                     "Late-stage"))) %>%
    mutate(time_disc = factor(time_disc,levels=c("Early","Chronic","Late-stage"))) %>%
    ungroup() %>%  
    mutate(tissue=ifelse(tissue=="LN", "J", tissue)) %>%
    filter(tissue != "LPL", tissue!="PB") %>%
    mutate(tissue=factor(tissue, levels=c("P", "J", "K",
                                          "O", "B", "U")))
=======
    dplyr::group_by(animal) %>%
    dplyr::mutate(time_nec = max(time)) %>%
    dplyr::mutate(time_disc = ifelse(time<=21, "Early",
                              ifelse(time>21 & time < (time_nec-21), "Chronic",
                                     "Late-stage"))) %>%
    dplyr::mutate(time_disc = factor(time_disc,levels=c("Early","Chronic","Late-stage"))) %>%
    ungroup() %>%  
    dplyr::mutate(tissue=ifelse(tissue=="LN", "J", tissue)) %>%
    dplyr::filter(tissue != "LPL", tissue!="PB") %>%
    dplyr::mutate(tissue=factor(tissue, levels=c("P", "J", "K",
                                          "O", "B", "U")))
  return(tbl)
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
}

# old, when feats was a list and list of rules was short
df_all_rules = do.call(rbind, mclapply(feats, function(x) {
  y=data.frame(`1_01`=NA,`1_02`=NA,`1_03`=NA,`1_04`=NA,`1_05`=NA,`1_06`=NA,`1_07`=NA,
               `2_01`=NA,
               `3_01`=NA,
               `4_01`=NA,`4_02`=NA,`4_03`=NA,
               `5_01`=NA,`5_02`=NA,
               `6_01`=NA)
  names(y) <- sub("^X", "", names(y))
  
  if(isTRUE(x$ID[r1s1] == "W" & 
            x$ID[r1s2] == "Q" &
            x$ID[r1s3] == "E" &
            x$MS[r1s4] > -3.656)){
    y$`1_01`=1
    # 1_01	ID	325	W
    # 1_01	ID	197	Q
    # 1_01	ID	317	E
    # 1_01	molecular_size	28 	> -3.656
<<<<<<< HEAD
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s3] == "E" &
             x$MS[r1s4] > -3.656)) {
    y$`1_02`=1
    # 1_02	ID	325	W
    # 1_02	ID	317	E
    # 1_02	molecular_size	28 	> -3.656
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s3] == "E")) {
    y$`1_03`=1
    # 1_03	ID	325	W
    # 1_03	ID	317	E
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s2] == "Q" &
             x$MS[r1s4] > -3.656)) {
    y$`1_04`=1
    # 1_04	ID	325	W
    # 1_04	ID	197	Q
    # 1_04	molecular_size	28 	> -3.656
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
=======
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s3] == "E" &
             x$MS[r1s4] > -3.656)) {
    y$`1_02`=1
    # 1_02	ID	325	W
    # 1_02	ID	317	E
    # 1_02	molecular_size	28 	> -3.656
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s3] == "E")) {
    y$`1_03`=1
    # 1_03	ID	325	W
    # 1_03	ID	317	E
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
             x$ID[r1s2] == "Q" &
             x$MS[r1s4] > -3.656)) {
    y$`1_04`=1
    # 1_04	ID	325	W
    # 1_04	ID	197	Q
    # 1_04	molecular_size	28 	> -3.656
  }
  if (isTRUE(x$ID[r1s1] == "W" & 
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
             x$ID[r1s2] == "Q" )) {
    y$`1_05`=1
    # 1_05	ID	325	W
    # 1_05	ID	197	Q
  }
  if (isTRUE(x$ID[r1s2] == "Q" & 
             x$MS[r1s4] <= -3.656)) {
    y$`1_06`=1
    # 1_06	ID	197	Q
    # 1_06	molecular_size	28 	<= -3.656
  }
  if (isTRUE(x$ID[r1s3] == "K")) {
    y$`1_07`=1
    # 1_07	ID	317	K
  }
  if (isTRUE(x$ID[r2s1] == "W" &
             x$ID[r2s2] == "P" &
             x$ID[r2s3] == "I")) {
    y$`2_01`=1
    # 2_01	ID	325	W
    # 2_01	ID	410	P
    # 2_01	ID	462	I
  }
  if (isTRUE(x$ID[r3s1] == "P" &
             x$ID[r3s2] == "K")) {
    y$`3_01`=1
    # 3_01	ID	410	P
    # 3_01	ID	317	K
  }
  if (isTRUE(x$P[r4s1] > -0.032 &
             x$ID[r4s2] == "Q" &
             x$ID[r4s3] == "I")) {
    y$`4_01`=1
    #4_01	polarity	407 	> -0.032
    # 4_01	ID	197	Q
    # 4_01	ID	462	I
  }
  if (isTRUE(x$P[r4s1] > -0.032 &
             x$ID[r4s2] == "Q")) {
    y$`4_02`=1
    #4_02	polarity	407 	> -0.032
    #4_02	ID	197	Q
  }
  if (isTRUE(x$P[r4s1] > -0.032 &
             x$ID[r4s3] == "I")) {
    y$`4_03`=1
    #4_03	polarity	407 	> -0.032
    #4_03	ID	462	I
  }
  if (isTRUE(x$ID[r5s1] == "W" &
             x$ID[r5s2] == "R")) {
    y$`5_01`=1
    #5_01	ID	325	W
    # 5_01	ID	415	R
  }
  if (isTRUE(x$ID[r5s2] == "R")) {
    y$`5_02`=1
    # 5_02	ID	415	R
  }
  if (isTRUE(x$ID[r6s1] == "E" &
             x$ID[r6s2] == "I")) {
    y$`6_01`=1
    # 6_01	ID	317	E
    # 6_01	ID	462	I
  }
  
  return(y)
}, mc.cores=numCores)) 
df_all_rules$isolate=names(feats)


df_all = tbl.manip(df_all_rules, df, "1_01", "6_01")

<<<<<<< HEAD
# Performance for reduced rules ##############################
# df_red_rules = do.call(rbind, mclapply(feats, function(x) {
#   y=data.frame(`1_03`=NA,`4_02`=NA,`4_03`=NA,`6_01`=NA)
#   names(y) <- sub("^X", "", names(y))
#   if (isTRUE(x$ID[r1s1] == "W" & 
#              x$ID[r1s3] == "E")) {
#     y$`1_03`=1
#     # 1_03	ID	325	W
#     # 1_03	ID	317	E
#   }
#   if (isTRUE(x$P[r4s1] > -0.032 &
#              x$ID[r4s2] == "Q")) {
#     y$`4_02`=1
#     #4_02	polarity	407 	> -0.032
#     #4_02	ID	197	Q
#   }
#   if (isTRUE(x$P[r4s1] > -0.032 &
#              x$ID[r4s3] == "I")) {
#     y$`4_03`=1
#     #4_03	polarity	407 	> -0.032
#     #4_03	ID	462	I
#   }
#   if (isTRUE(x$ID[r6s1] == "E" &
#              x$ID[r6s2] == "I")) {
#     y$`6_01`=1
#     # 6_01	ID	317	E
#     # 6_01	ID	462	I
#   }
#   
#   return(y)
# }, mc.cores=numCores)) 
# df_red_rules$isolate=names(feats)
# 
# df_red = tbl.manip(df_red_rules, df, "1_03", "6_01")

=======
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435

colors=c("P"="indianred", "J"='midnightblue', "K" = "lightblue",
         "O" = "lightgreen", "B" = "mediumpurple", "U" = "pink")

<<<<<<< HEAD

p1=dplyr::select(df_all, isolate, class, training, time_disc, tissue) %>%
  distinct() %>%
  filter(training=="application") %>%
  ggplot() +
  geom_bar(aes(x=time_disc, y=..count.., fill=tissue), position="stack") +
=======
totals = dplyr::select(df_all, isolate, class, time_disc, tissue) %>%
  dplyr::distinct() %>%
  dplyr::group_by(class, time_disc, tissue) %>%
  dplyr::summarize(total=n())

p1=ggplot(totals) +
  geom_col(aes(x=time_disc, y=total, fill=tissue), position="stack") +
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
  facet_wrap(~class, ncol=2) +
  theme_minimal() +
  scale_fill_manual(name="Tissue", labels=c("Plasma", "Lymph Node", "CD3+ PBMCs",
                                            "CD14+ PBMCs", "Bone Marrow", "BAL"),
                    values=colors) +
  labs(x="Time point", y="Number of sequences") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        text=element_text(size=14),
        legend.position="bottom")

<<<<<<< HEAD

p2=filter(df_all, training=="application") %>%
  mutate(rule_group=ifelse(grepl("(1_).+", rule), "Group 1 Rules",
                           ifelse(grepl("(2_).+", rule), "Group 2 Rules", 
                                  ifelse(grepl("(3_).+", rule), "Group 3 Rules",
                                         ifelse(grepl("(4_).+", rule), "Group 4 Rules",
                                                ifelse(grepl("(5_).+", rule), "Group 5 Rules",
                                                       "Group 6 Rules")))))) %>%
  group_by(rule_group, time_disc, tissue, class) %>%
  summarize(tissue_total=sum(value, na.rm=T))%>%
  ggplot() +
  geom_bar(aes(x=time_disc, y=tissue_total, fill=tissue), stat="identity") +
=======
grouped = dplyr::filter(df_all, value==1) %>%
  dplyr::mutate(rule_group=gsub("(\\d)_.+", "Group \\1 Rules", rule)) %>%
  dplyr::select(isolate, class, rule_group, time_disc, tissue) %>%
  distinct() %>% # Necessary because counted more than once in a rule group
  dplyr::group_by(rule_group, class, time_disc, tissue) %>%
  dplyr::summarize(n=n())

p2= ggplot(grouped) +
  geom_col(aes(x=time_disc, y=n, fill=tissue), position="stack") +
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
  facet_wrap(rule_group~class, scales="free_y", ncol=2) +
  theme_minimal() +
  scale_fill_manual(name="Tissue", labels=c("Plasma", "Lymph Node", "CD3+ PBMCs",
                                            "CD14+ PBMCs", "Bone Marrow", "BAL"),
                    values=colors) +
  labs(x="Time point", y="Number of sequences") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        text=element_text(size=14),
        legend.position="none")

<<<<<<< HEAD
=======
freq = dplyr::left_join(grouped, totals) %>%
  dplyr::mutate(perc = n/total)

p3=ggplot(freq) +
  geom_col(aes(x=time_disc, y=perc*100, fill=tissue), position="stack") +
  facet_wrap(rule_group~class, ncol=2) +
  theme_minimal() +
  scale_fill_manual(name="Tissue", labels=c("Plasma", "Lymph Node", "CD3+ PBMCs",
                                            "CD14+ PBMCs", "Bone Marrow", "BAL"),
                    values=colors) +
  labs(x="Time point", y="Number of sequences") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        text=element_text(size=14),
        legend.position="none")
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435
#ggarrange(p1,p2, ncol=2)
ggsave(plot=p1, "total_tissue.png", width=4, height=11, units="in")
ggsave(plot=p2, "rules_application.png", width=4, height=11, units="in")
# Copy above into assign_rules
<<<<<<< HEAD

# Now determine percentage of each tissue classified correctly
# and incorrectly as SIVE

filter(df_all, training=="application") %>%
  dplyr::select(isolate, tissue, value) %>%
  group_by(isolate) %>%
  mutate(rule_count = ifelse(sum(value, na.rm=T)>0, 1, 0)) %>%
  dplyr::select(-value) %>%
  distinct() %>%
  group_by(tissue) %>%
  mutate(n=n()) %>%
  summarize(perc=sum(rule_count, na.rm=T)/n) %>%
  distinct()
=======

# Now determine percentage of each tissue classified correctly
# and incorrectly as SIVE
>>>>>>> 1fd09ad7e4a38a352116ccfa341d1ea728adb435

filter(df_all, training=="application") %>%
  dplyr::select(isolate, tissue, value) %>%
  group_by(isolate) %>%
  mutate(rule_count = ifelse(sum(value, na.rm=T)>0, 1, 0)) %>%
  dplyr::select(-value) %>%
  distinct() %>%
  group_by(tissue) %>%
  mutate(n=n()) %>%
  summarize(perc=sum(rule_count, na.rm=T)/n) %>%
  distinct()

dplyr::filter(training=="application") %>%
  
  
rule_counts = dplyr::left_join(df_all_rules, df) %>%
  dplyr::filter(training=="training", class=="SIVE") %>%
  dplyr::mutate(animal = ifelse(grepl("JA41", isolate), "JA41",
                                ifelse(grepl("^[A-Z]+", isolate),
                                       gsub("^([A-Z]+\\d+).+", "\\1", isolate),
                                       gsub("\\d+([A-Z]\\d+)[A-Z].+", "\\1", isolate)))) %>%
  gather(rule, value, "1_01":"6_01") %>%
  group_by(animal) %>%
  dplyr::mutate(total_seqs=length(unique(isolate))) %>%
  dplyr::filter(value==1) %>%
  dplyr::group_by(rule, animal) %>%
  dplyr::summarize(seqs=length(isolate),
                   perc_seqs = round(seqs/total_seqs*100)) %>%
  dplyr::group_by(rule) %>%
  dplyr::summarize(seqs=sum(seqs),
            animals=length(unique(animal[perc_seqs>85])))

write.csv(rule_counts, "rule_counts_85.csv", quote=F, row.names=F)
