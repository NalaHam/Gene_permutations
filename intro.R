library(dplyr)
?complete.cases

unique(gene_mutations$Project) #gene_mutations is just nonreproductive cancers

#remove na's from gender. 
gene_mutations <- gene_mutations[complete.cases(gene_mutations[, 18]), ] 

sum(is.na(gene_mutations$gender)) #11621 used to know how many is removed


#goal: sum total mutations for each unique gene based on sex. 

#make easier df for loop
df_for_perm <- gene_mutations[,c(3,15,18)]



