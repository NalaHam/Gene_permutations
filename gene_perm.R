# lets calculate the absolute diff in means
test.stat1 <- mean(gene_mut_count$male_ratio) 
test.stat1 #0.617

# the number of permutation samples to take

#make test df for just one gene
expected <- sum(gene_mut_count_sex$male) / sum(gene_mut_count_sex$total)
observed <- gene_mut_count_sex[1, 6]
n <- gene_mut_count_sex[1,5]
Gene <- 'A1BG'

df <- data.frame(Gene, n, observed, expected)

#try again
df.1 <- data.frame(Gene,Mutations,Sex)

set.seed(1979)  

n <- sum(df.1$Mutations)

P <- 100000 

variable <- df.1$Mutations  

PermSamples <- matrix(0, nrow=n, ncol=P)


#try again
gene.1 <- filter(gene_mutations, Gene == "A1BG")
gene.1 <- gene.1[, c(3,15,18)]

gene.1$gender <- factor(gene.1$gender)

is.factor(gene.1$gender)

levels(gene.1$gender)

table(gene.1$Total)

boxplot(gene.1$Total~gene.1$gender, las=1, ylab="mutations", 
        xlab="sex",main="mutations for A1BG")

?rep()
female <- rep(1, 12)
male <- rep(1, 26)

df.2 <- data.frame(Gene, female, male)

max.length <- max(c(length(female), length(male)))    # Find out maximum length
max.length #26

data <- data.frame(female = c(female,                 # Create data frame with unequal vectors
                              rep(NA, max.length - length(female))),
                     male = c(male,
                              rep(NA, max.length - length(male)))) #used https://statisticsglobe.com/create-data-frame-of-unequal-lengths-in-r for reference
data$gene <- "A1BG"

data[is.na(data)] = 0 #make NA 0s




for(i in 1:P){
  PermSamples[,i] <- sample(variable, size= n, replace=FALSE)
}























