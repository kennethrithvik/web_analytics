##########################################
# simple demo of building sequence rules 
##########################################

library(arulesSequences)
library("readr")


##load training data and modify
train_clicks <- read_csv("RecSys/data_cleaned/train.csv", 
                         col_types = cols(X1 = col_skip(), cat = col_character(), 
                                          itemID = col_character(), sessionID = col_character(), 
                                          status = col_logical(), ts = col_datetime(format = "%Y-%m-%dT%H:%M:%OS%Z")))

data <- read_baskets(con = "./"RecSys/data_cleaned/train.csv", info = c("sequenceID","eventID","SIZE"))

as(head(data), "data.frame") # view first few rows of the data

seqs <- cspade(data, parameter = list(support = 0.1), control = list(verbose = TRUE))
as(seqs,"data.frame")  # view the sequences

rules <- ruleInduction(seqs, confidence = 0.5,control = list(verbose = TRUE))
as(rules,"data.frame")  # view the rules
