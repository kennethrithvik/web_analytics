setwd("~/development/mtech/web_analytics")

library("arules");
library("future.apply")
library("dplyr")
library("readr")
library("Metrics")
plan(multiprocess, workers = 4)

##load training data and modify
train_clicks <- read_csv("RecSys/data_cleaned/train.csv", 
                    col_types = cols(X1 = col_skip(), cat = col_character(), 
                    itemID = col_character(), sessionID = col_character(), 
                    status = col_logical(), ts = col_datetime(format = "%Y-%m-%dT%H:%M:%OS%Z")))
train_clicks$day<-weekdays(train_clicks$ts)
train_clicks$hour<- format(train_clicks$ts,"%H")
train_clicks$cat<-paste0("cat=",train_clicks$cat)
train_clicks$itemID<-paste0("itemID=",train_clicks$itemID)
train_clicks$status<-paste0("BUY=",train_clicks$status)
train_clicks$day<-paste0("day=",train_clicks$day)
train_clicks$hour<-paste0("hour=",train_clicks$hour)
train_clicks$ts<-NULL
#train_clicks$status<-NULL
train_clicks$cat<-NULL
readr::write_csv(train_clicks,"./RecSys/data_cleaned/train1.csv")

#build rules
trainegs = read.transactions(file='./RecSys/data_cleaned/train1.csv',format="basket", sep=",", cols=1)
rhsTerms <- grep("^BUY=TRUE", itemLabels(trainegs), value = TRUE)
lhsTerms <- grep("^itemID=|day=|hour=", itemLabels(trainegs), value = TRUE)
rules <- apriori(trainegs, parameter = list(supp=0.0005, conf=0.15, minlen=2)
                 ,appearance = list(rhs = rhsTerms,lhs=lhsTerms,default="none")
                 )
summary(rules)
inspect(rules)

#execute rules against test data
rulesDF = as(rules,"data.frame")
rulesDF$lhs<-as(lhs(rules), "list")
rulesDF$rhs<-as(rhs(rules), "list")
rulesDF$lhs_length<-apply(rulesDF,1,function(x)length(x["lhs"][[1]]))
#readr::write_csv(rulesDF,"./RecSys/data_cleaned/seq_rules.csv")

# a useful plot of training data
itemFrequencyPlot(trainegs,topN=20,type="absolute")

#read the test data
testegs <- read_csv("RecSys/data_cleaned/test.csv", 
                    col_types = cols(X1 = col_skip(), cat = col_character(), 
                    itemID = col_character(), sessionID = col_character(), 
                    status = col_logical(), ts = col_datetime(format = "%Y-%m-%dT%H:%M:%OS%Z")))
testegs$day<-weekdays(testegs$ts)
testegs$hour<- format(testegs$ts,"%H")
testegs$cat<-paste0("cat=",testegs$cat)
testegs$itemID<-paste0("itemID=",testegs$itemID)
testegs$status<-paste0("BUY=",testegs$status)
testegs$day<-paste0("day=",testegs$day)
testegs$hour<-paste0("hour=",testegs$hour)
testegs$ts<-NULL
#testegs$status<-NULL
testegs$cat<-NULL

# extract unique items bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(. ~ sessionID, data = testegs, paste, collapse=","))
baskets$itemID = apply(baskets,1,function(X) uniqueitems(X["itemID"]))
baskets$status = apply(baskets,1,function(X) uniqueitems(X["status"]))
baskets$day = apply(baskets,1,function(X) uniqueitems(X["day"]))
baskets$hour = apply(baskets,1,function(X) uniqueitems(X["hour"]))
baskets$item_cat = apply(baskets,1,function(X) c(unlist(X["itemID"]),unlist(X["day"]),unlist(X["hour"])))
baskets<-baskets[1:10000,]

#make predictions
baskets$preds= future_apply(baskets,1,function(X) makepreds(X["item_cat"], rulesDF))


#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(baskets,1,function(X) checkpreds(X["preds"],X["status"],X["sessionID"])))

# count total number of unique predictions made
totalpreds = sum(apply(baskets,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds)

predict<-ifelse(predict %in% c("BUY=FALSE"), FALSE, TRUE)
actual<-ifelse(actual %in% c("BUY=FALSE"), FALSE, TRUE)
accuracy(actual,predict)
recall(actual,predict)
precision(actual,predict)
f1(actual,predict)
#######################################################################
# the supporting functions
#######################################################################

#remove dupliitemIDe items from a basket (itemstrg)
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
  #as.list(unique(rhs))
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
plot(rules, method="graph",max=20)
plot(rules, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1,max=20)
plot(rules, method="matrix")
plot(rules, method="paracoord", control=list(reorder=TRUE))


