#test on smaller df

#make small df
small_gene_mut <- gene_mutations[c(1,2,3,10,50,387,13,165,155,62,4793), c(1,2,3,15,18)]

small_gene_mut_count <- data.frame(tapply(small_gene_mut$Total, INDEX = list(small_gene_mut$Gene, small_gene_mut$gender),
       FUN = sum, default = 0))

#big papa
#sum total mutations by gene and gender
gene_mut_count <- data.frame(tapply(gene_mutations$Total, INDEX = list(gene_mutations$Gene, gene_mutations$gender),
                                    FUN = sum, default = 0))

length(unique(gene_mutations$Gene)) #check to see that gene_mut_count has same number of genes as og df

gene_mut_count$Gene <- row.names(gene_mut_count) #make a row with the genes

gene_mut_count$total <- rowSums(gene_mut_count) #sum mutations for total mutations

gene_mut_count$male_ratio <- gene_mut_count$male/gene_mut_count$total

gene_mut_count$female_ratio <- gene_mut_count$female/gene_mut_count$total

gene_mut_count <- gene_mut_count[, c(5,1,2,3,4, 6)] #organize columns

mean(gene_mut_count$male_ratio) #0.617
mean(gene_mut_count$female_ratio) #0.383

write.csv(gene_mut_count, file = "gene_mut_count_sex.csv")

sum(gene_mut_count$male) / sum(gene_mut_count$total) #0.617, this is what dunford did to get a base prob

fm_muts <- gene_mut_count$female
m_muts <- gene_mut_count$male

boxplot(fm_muts) #outliers push plot 
boxplot(m_muts) #outliers push plot 












