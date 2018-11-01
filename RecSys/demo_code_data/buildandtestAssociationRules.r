#######################################################################
# simple demo of testing a set of association rules against a separate test set
#######################################################################

library("arules");

#build rules
trainegs = read.transactions(file="C:\\Users\\issbas\\Documents\\EBAC\\EBAC-electives\\webanalytics\\workshops\\simplebasket.csv",rm.duplicates=TRUE, format="single", sep=",", cols=c("basketID","item"));
rules <- apriori(trainegs, parameter = list(supp=0.1, conf=0.1, minlen=2))
summary(rules)
inspect(rules)

# a useful plot of training data
itemFrequencyPlot(trainegs,topN=20,type="absolute")

#read the test data
testegs = read.csv(file="C:\\Users\\issbas\\Documents\\EBAC\\EBAC-electives\\webanalytics\\workshops\\simplebasket-test.csv");
colnames(testegs) <- c("basketID","items")  # set standard names, in case they are different in the data file

#execute rules against test data
rulesDF = as(rules,"data.frame")
testegs$preds = apply(testegs,1,function(X) makepreds(X["items"], rulesDF))

# extract unique predictions for each test user
userpreds = as.data.frame(aggregate(preds ~ basketID, data = testegs, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(items ~ basketID, data = testegs, paste, collapse=","))
baskets$items = apply(baskets,1,function(X) uniqueitems(X["items"]))

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["basketID"])))

# count total number of unique predictions made
totalpreds = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds)

#######################################################################
# the supporting functions
#######################################################################

#remove duplicate items from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}

# execute ruleset using item as rule antecedent (handles single item antecedents only)
makepreds <- function(item, rulesDF) {
  antecedent = paste("{",item,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

# count how many predictions are in the basket of items already seen by that user 
# Caution : refers to "baskets" as a global
checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$basketID == baskID,"items"][[1]]
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


