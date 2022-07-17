# Gene_permutations

#test for just one cancer to see if the loop works
project <- subset(gene_mutations, gene_mutations$Project == "TCGA-XXXX") #put what ever cancer you want to test in the XXXX

df_for_perm <- project[, c(2,14,17)]#condense df to be just gene, total, gender (these might be differ coln numbers for you)

gene_list <- unique(project$Gene) #list of genes that are mutated in cancer type (this is done because some cancers don't have mutations in all the genes)

#permutation loop 
for (x in gene_list) {
  data <- subset(df_for_perm, df_for_perm$Gene == x)
  k <- length(data$gender)
  row.names(data) <- c(1:k)
  test.stat1 <- abs((sum(data[which(data$gender=='male'),2 ])/ sum(data$Total)) - 
                      expected) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it
  #values
  n <- length(data$gender) 
  
  P <- 500000 #new permutation count
  
  variable <- data$Total
  
  #make empty df
  PermSamples <- matrix(0, nrow=n, ncol=P)
  
  for(i in 1:P){
    PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
  }
  
  #make empty vector
  Perm.test.stat1 <- rep(0, P)
  
  # loop thru, and calculate the test-stats
  for (j in 1:P){
    # calculate the perm-test-stat1 and save it
    Perm.test.stat1[j] <- abs((sum(PermSamples[which(data$gender=='male'),j ])/ sum(data$Total)) - 
                                expected)
  }
  
  #calculate the p-value, for all P
  c <- mean(Perm.test.stat1 >= test.stat1)
  y <- which(grepl( x , gene_mut_half$Gene)) #says what row the gene is in
  
  gene_mut_half[ y , XXX] <- c #Make XXX the column number the cancer you are testing is ex: TCGA-BLCA is the cancer project 
                                      #you are testing and it is column 10 in gene_mut_half
}
}

#After this is done check the column in the gene_mut_half to see if there are any numbers there for any genes. 
#remember that for some cancers there isn't data for every gene.

#to test you can subset the df into just the genes that the cancer has with:

XXXX_df <- gene_mut_half[gene_mut_half$Gene %in% gene_list,] #this should work...

