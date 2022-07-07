
gene_mut_count_sex$p_value <- 0 #make row for p values 

myDataFrame["rowName", "columnName"] <- value #command to add into a cell in df

#Gene: ATRX

data <- subset(gene_mutations, gene_mutations$Gene == "ATRX")
data <- data[, c(3,15,18)]
row.names(data) <- c(1:468)


boxplot(data$Total~data$gender, las=1, ylab="number of mutations", 
        xlab="sex",main="ATRX Mutations")


sum(data$Total)
sum(data[which(data$gender=='male'),2])

test.stat1 <- abs((sum(data[which(data$gender=='male'),2 ])/ sum(data$Total)) - 
                    (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total))) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it


test.stat1

set.seed(1979)  

n <- length(data$gender) 

P <- 1000000 

variable <- data$Total

PermSamples <- matrix(0, nrow=n, ncol=P)

for(i in 1:P){
  PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
}

# we can take a quick look at the first 5 columns of PermSamples
PermSamples[, 1:5]


Perm.test.stat1 <- rep(0, P)

# loop thru, and calculate the test-stats
for (i in 1:P){
  # calculate the perm-test-stat1 and save it
  Perm.test.stat1[i] <- abs((sum(PermSamples[which(small_data$gender=='male'),i ])/ sum(small_data$Total)) - 
                              (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total)))
}

test.stat1 #reminder that the test statistic is 0.16

round(Perm.test.stat1[1:15], 1) #all are over or equal to test.stat1 

# and, let's calculate the permutation p-value...
# notice how we can ask R a true/false question...(for the first 15)
(Perm.test.stat1 >= test.stat1)[1:15]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
mean((Perm.test.stat1 >= test.stat1)[1:15]) #p-value for the first 15 permutations

#...calculate the p-value, for all P=100,000
c <- mean(Perm.test.stat1 >= test.stat1)

row.names(gene_mut_count_sex) <- gene_mut_count_sex$Gene

gene_mut_count_sex <- subset(gene_mut_count_sex, select = -1)

gene_mut_count_sex["ATRX", "p_value"] <- c






