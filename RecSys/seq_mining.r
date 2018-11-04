##########################################
# simple demo of building sequence rules 
##########################################

library(arulesSequences)
library("readr")
library(dplyr)

##load training data and modify
##load training data and modify
train_clicks <- read_csv("RecSys/data_cleaned/train.csv", 
                         col_types = cols(X1 = col_skip(), cat = col_character(), 
                                     itemID = col_character(), sessionID = col_character(), 
                                     status = col_logical(), ts = col_datetime(format = "%Y-%m-%dT%H:%M:%OS%Z")))

train_clicks<-train_clicks[with(train_clicks,order(sessionID,ts)),]
train_clicks<-train_clicks %>% group_by(sessionID) %>% mutate(eventid = row_number(sessionID))
train_clicks$ts<-train_clicks$eventid
train_clicks$itemID<-paste0("itemID=",train_clicks$itemID)
#train_clicks$itemID<-paste(train_clicks$itemID,"-",train_clicks$cat)
train_clicks$eventid<-NULL
train_clicks$status<-NULL
train_clicks$cat<-NULL
readr::write_csv(train_clicks,"./RecSys/data_cleaned/seq.csv",col_names = FALSE)

#read sequences
data <- read_baskets(con = "./RecSys/data_cleaned/seq.csv",sep = ",", info = c("sequenceID","eventID"))

as(head(data), "data.frame") # view first few rows of the data

seqs <- cspade(data, parameter = list(support = 0.001), control = list(verbose = TRUE))
as(seqs,"data.frame")  # view the sequences
summary(seqs)

rules <- ruleInduction(seqs, confidence = 0.01,control = list(verbose = TRUE))
as(rules,"data.frame")  # view the rules

#form rules dataframe
rulesDF = as(rules,"data.frame")
rulesDF$lhs<-as(lhs(rules), "list")
rulesDF$rhs<-as(rhs(rules), "list")
rulesDF$lhs_length<-apply(rulesDF,1,function(x)length(x["lhs"][[1]]))
#readr::write_csv(rulesDF,"./RecSys/data_cleaned/seq_rules.csv")
