# lets calculate the absolute diff in means
test.stat1 <- mean(gene_mut_count$male_ratio) 
test.stat1 #0.617

n <- length(gene_mut_count$Gene)  
# the number of permutation samples to take
P <- 100000 
