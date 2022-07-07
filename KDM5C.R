#Gene: KDM5C
set.seed(1979)  

#make data frame
data <- subset(df_for_perm, df_for_perm$Gene == "KDM5C")
k <- length(data$gender)
row.names(data) <- c(1:k)

#test statistic
test.stat1 <- abs((sum(data[which(data$gender=='male'),2 ])/ sum(data$Total)) - 
                    (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total))) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it
#values
n <- length(data$gender) 

P <- 1000000 

variable <- data$Total

#make empty df
PermSamples <- matrix(0, nrow=n, ncol=P)

for(i in 1:P){
  PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
}

#make empty vector
Perm.test.stat1 <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1[i] <- abs((sum(PermSamples[which(small_data$gender=='male'),i ])/ sum(small_data$Total)) - 
                              (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total)))
}

#calculate the p-value, for all P
c <- mean(Perm.test.stat1 >= test.stat1)

gene_mut_count_sex["ATRX", 7] <- c
