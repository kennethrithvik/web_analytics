setwd("~/development/mtech/web_analytics")

library("arules");
library("future.apply")
plan(multiprocess, workers = 7)

#build rules
trainegs = read.transactions(file='./RecSys/data_cleaned/train.csv',format="single", sep=",", cols=c("sessionID","itemID"));
rules <- apriori(trainegs, parameter = list(supp=0.00211, conf=0.33324, minlen=2))
summary(rules)
inspect(rules)

# a useful plot of training data
itemFrequencyPlot(trainegs,topN=20,type="absolute")

#read the test data
testegs = read.csv(file="./RecSys/data_cleaned/test.csv");

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(itemID ~ sessionID, data = testegs, paste, collapse=","))
baskets$itemID = apply(baskets,1,function(X) uniqueitems(X["itemID"]))
baskets<-baskets[1:10000,]

#execute rules against test data
rulesDF = as(rules,"data.frame")
rulesDF$lhs<-as(lhs(rules), "list")
rulesDF$rhs<-as(rhs(rules), "list")
rulesDF$lhs_length<-apply(rulesDF,1,function(x)length(x["lhs"][[1]]))

#make predictions
baskets$preds= future_apply(baskets,1,function(X) makepreds(X["itemID"], rulesDF))


#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(baskets,1,function(X) checkpreds(X["preds"],X["itemID"],X["sessionID"])))

# count total number of unique predictions made
totalpreds = sum(apply(baskets,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds)



#######################################################################
# the supporting functions
#######################################################################

#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  l<-NULL
  if(itemstrg==""){
    l<-''
  }
  else{
    l<-unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
    if(length(l)==1&&l==""){
      l<-""
    }
    else
    {
      l<- l[nchar(l)>0]
    }
  }
  l
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  temp<-apply(rulesDF,1,function(x)length(intersect(x["lhs"][[1]],item[[1]])))
  rhs<-rulesDF[temp==rulesDF$lhs_length,]$rhs
  rhs<-unlist(rhs)
  unique(rhs)
}

# count how many predictions are in the basket of items already seen by that user 
checkpreds <- function(preds, items, baskID) {
  plist = preds[[1]]
  blist = items[[1]]
  cnt = 0 
  for (p in plist) {
    if (p %in% blist) cnt = cnt+1
  }
  cnt
}

# count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len > 0 && (predlist[[1]] == "")) 0 # avoid counting an empty list
  else len
}

###########################################################################
# rule visualisation
# also see (for example) http://www.rdatamining.com/examples/association-rules
###########################################################################

library(arulesViz)
plot(rules)
plot(rules, method="graph")
plot(rules, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1)
plot(rules, method="matrix")
plot(rules, method="paracoord", control=list(reorder=TRUE))


