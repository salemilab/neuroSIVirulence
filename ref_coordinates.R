require(seqinr)


setwd("/Users/macbook/Dropbox (UFL)/SIV/SIV_Machine_Learning/NEW_rules")

ref = read.fasta("../FASTA/all_monkeys.aa.fasta")[1][[1]]
#ref=read.fasta("../FASTA/all_monkeys.aa.fasta")

ref_length = length(ref)
# Resolve insertions in reference sequence (re-configure to work with code below later)
Site=1:ref_length
ref_site=c(35)
x=2
while (x <= ref_length) {
  last_site=last(ref_site)
  if (ref[x]!="-") {
    ref_site=c(ref_site, trunc(last_site+1))
  } else {
    ref_site=c(ref_site, last_site+0.1)
  }
  x=x+1
}

ref_site_df = data.frame(Site=Site,
                         ref_site=ref_site)

# ref_site=c(37)
# Site1=1
# Site2 = 1:length(ref[[1]])
# x=2
# while (x <= ref_length) {
#   last_site=last(Site1)
#   if (ref$data[x]=="-") {
#     Site1=c(Site1, last_site+0.01)
#   } else {
#     Site1=c(Site1, trunc(last_site+1))
#   }
#   x=x+1
# }
# ref_site_df = data.frame(Site=Site1,
#                          ref_site=Site2+ref_site-1) %>%
#   dplyr::filter(Site %% 1 == 0)



write.csv(ref_site_df, "Reference_site_numbers.csv", quote=F, row.names=F)
