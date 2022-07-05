#choose better example of a gene to test
#Gene: TTN

data <- subset(gene_mutations, gene_mutations$Gene == "TTN")
data <- data[, c(3,15,18)]
row.names(data) <- c(1:2207)

#lets make it smaller to look at 

small_data <- data[1:23,]

boxplot(small_data$Total~small_data$gender, las=1, ylab="number of mutations", 
        xlab="sex",main="TTN Mutations")


sum(small_data$Total)

test.stat1 <- (sum(small_data[which(small_data$gender=='male'),2 ])/ sum(small_data$Total)) - 
                    (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total) #expect that this should be 0 or close to it. if it is greater than or equal, lets look into it
  

test.stat1

set.seed(1979)  

n <- length(small_data$gender) 

P <- 100000 

variable <- small_data$Total

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
  Perm.test.stat1[i] <- (sum(PermSamples[which(small_data$gender=='male'),i ])/ sum(small_data$Total)) - 
                              (sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total))
}

test.stat1 #reminder that the test statistic is 0.0447

round(Perm.test.stat1[1:15], 1) #none of the first 15 permutations are close to 0.0447 or more (all are 0)

# and, let's calculate the permutation p-value...
# notice how we can ask R a true/false question...(for the first 15)
(Perm.test.stat1 >= test.stat1)[1:15]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
mean((Perm.test.stat1 >= test.stat1)[1:15]) #p-value for the first 15 permutations

#...calculate the p-value, for all P=100,000
mean(Perm.test.stat1 >= test.stat1)



