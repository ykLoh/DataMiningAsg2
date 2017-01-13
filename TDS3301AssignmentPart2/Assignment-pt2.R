#start counting the run time(seconds) for experiment
ptm <- proc.time()

setwd("/Users/natalie/Desktop")
bakery <- read.csv('1000-out2.csv',header = FALSE,stringsAsFactors = FALSE)
bakery$V1 <- NULL
bakery <- as.matrix(bakery)

productname <- read.csv('productname.csv',header = FALSE,stringsAsFactors = FALSE)
dimnames(bakery) <-  list(NULL,productname$V2)

library(ggplot2)
#get each column sum
qty <- as.integer(colSums(bakery != 0))
items <- data.frame(productname$V2,qty)
colnames(items) <- c("ProductName","Qty")
items <- items[with(items, order(qty)),]

#highest 10 sales
items <- tail(items,10) 
p <- ggplot(data=items, aes(x=reorder(ProductName,-Qty), y=Qty, fill=ProductName)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = Qty), color="white", hjust=1, size=3) +
  labs(title="Top 10 Highest Sales Products", x="Product Names", y = "Number of Sales")
p + coord_flip()

#rerun this first
as.integer(colSums(bakery != 0))
qty <- as.integer(colSums(bakery != 0))
items <- data.frame(productname$V2,qty)
colnames(items) <- c("ProductName","Qty")
items <- items[with(items, order(qty)),]

#top 5 lowest sales
items <- head(items,5) 
p <- ggplot(data=items, aes(x=reorder(ProductName,-Qty), y=Qty, fill=ProductName)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = Qty), color="white", hjust=1, size=4) +
  labs(title="Top 5 Lowest Sales Products", x="Product Names", y = "Number of Sales")
p + coord_flip()

library(arules)
library(arulesViz)
#the data converted into a set of transactions where each column is translated into items.
trans <- as(bakery, "transactions")
summary(trans)
#view few transactions as a matrix. True indicates the presence of an item.
as(trans, "matrix")[1:5,]
#view the transactions as sets of items
inspect(trans[1:10])
#view the relative frequency (=support) of items in the data set. 
itemFrequencyPlot(trans, type="absolute")
#here we look at the 15 most frequent items.
itemFrequencyPlot(trans, topN=15, type="absolute")

rules <- apriori(trans, parameter = list(supp = 0.002, conf = 0.8, minlen = 2))
inspect(rules[1:10])
summary(rules)

rules.sorted<-sort(rules, by="confidence", decreasing=TRUE)
rules.sorted
inspect(rules.sorted[1:10])

#remove redundancy
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
rules.pruned <- rules[!redundant]
summary(rules.pruned) #166
inspect(rules.pruned[1:10])

#sorted pruned rules with maximum support
rules.pruned.sorted<-sort(rules.pruned, by="support", decreasing = TRUE)
inspect(rules.pruned.sorted[1:10])

#sorted pruned rules with minimum support
rules.pruned.sorted<-sort(rules.pruned, by="support", decreasing = FALSE)
inspect(rules.pruned.sorted[1:5])

#sorted pruned rules with maximum confidence
rules.pruned.sorted<-sort(rules.pruned, by="confidence", decreasing = TRUE)
inspect(rules.pruned.sorted[1:10])

#sorted pruned rules with minimum confidence
rules.pruned.sorted<-sort(rules.pruned, by="confidence", decreasing = FALSE)
inspect(rules.pruned.sorted[1:5])

#sorted pruned rules with maximum lift
rules.pruned.sorted<-sort(rules.pruned, by="lift", decreasing = TRUE)
inspect(rules.pruned.sorted[1:10])

#sorted pruned rules with minimum lift
rules.pruned.sorted<-sort(rules.pruned, by="lift", decreasing = FALSE)
inspect(rules.pruned.sorted[1:5])

#what are customers likely to buy when they purchase "Truffle Cake"
rules<-apriori(trans, parameter=list(supp = 0.002,conf = 0.8), 
               appearance = list(default="lhs",rhs="Truffle Cake"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#stop the time and get know the result
proc.time() - ptm