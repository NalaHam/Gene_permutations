#Permutation test

perm_gene_list <- gene_list[1:19515]

expected <- 0.61654709279636


for (x in perm_gene_list) {
  data <- subset(df_for_perm, df_for_perm$Gene == x)
  k <- length(data$gender)
  row.names(data) <- c(1:k)
  test.stat1 <- abs((sum(data[which(data$gender=='male'),2 ])/ sum(data$Total)) - 
                      expected) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it
  #values
  n <- length(data$gender) 
  
  P <- 10000
  
  variable <- data$Total
  
  #make empty matrix
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
  y <- which(grepl( x , gene_mut_count_sex$Gene))
  
  gene_mut_count_sex[ y , 7] <- c
}


write.csv(gene_mut_count_sex, file = "gene_mut_count_sex.csv")
