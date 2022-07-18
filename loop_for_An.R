# Gene_permutations
#loop for An
library(dplyr) #install if not already installed

expected <- 0.61654709279636 

projects <- unique(gene_mutations$Project) #get list of cancer projects to use in loop

projects <- projects[15:25] #cancer projects that An is testing for

for(f in projects) {                        #make a loop that goes through each cancer 
  
  data.1 <- filter(gene_mutations, gene_mutations$Project == f) #make mutation df for each cancer 
  
  df_for_perm <- data.1[, c(2,14,17)] #condense df to be just gene, total, gender
  
  gene_list <- unique(data.1$Gene) #list of genes that are mutated in cancer type (this is done because some cancers don't have mutations in all the genes)
  
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
    
    for(b in 8:18) #the row numbers with the cancers that you want, ex: the start of the cancer columns is col 8 and they go to 18
      c <- mean(Perm.test.stat1 >= test.stat1)
      y <- which(grepl( x , gene_mut_half$Gene)) #says what row the gene is in
    
      gene_mut_half[ y , b] <- c #adds c into row of the gene and column of the cancer 
  }
}

write.csv(gene_mut_half, file = "gene_mut_half.csv")

#good Luck!
