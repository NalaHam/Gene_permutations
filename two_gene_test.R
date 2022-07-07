#for loop test with two genes

#make test data frame to pull from 
K_data <- data[11:29, ]

small_data <- subset(df_for_perm, Gene == "HOXB1")

row.names(small_data) <- c(1:60)

small_data <- small_data[4:20, ]

small_data <- rbind(small_data,K_data)
#small_data contains observations for two genes


gene_list <- unique(small_data$Gene)

for (x in gene_list) {
  data <- subset(small_data, small_data$Gene == x)
  k <- length(data$gender)
  row.names(data) <- c(1:k)
  test.stat1 <- abs((sum(data[which(data$gender=='male'),2 ])/ sum(data$Total)) - 
                    expected) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it
#values
  n <- length(data$gender) 

  P <- 100

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
                              (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total)))
      }

#calculate the p-value, for all P
  c <- mean(Perm.test.stat1 >= test.stat1)
  y <- which(grepl( x , gene_mut_count_sex$Gene))

  gene_mut_count_sex[ y , 7] <- c
}


