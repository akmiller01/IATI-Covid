list.of.packages <- c("data.table", "anytime", "Hmisc","reshape2","splitstackshape", "stringdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

comm = c("C", "2")
disb = c("E", "D", "3", "4")
incom = c("11")

single_vocabulary = function(row){
  codes = as.character(row$generic_sector_code)
  percentages = as.character(row$generic_sector_percentage)
  vocabularies = as.character(row$generic_sector_vocabulary)

  code_split = str_split(codes,",")[[1]]
  if(length(code_split)==1 & length(percentages)==0){
    percentages = "100"
  }
  perc_split = str_split(percentages,",")[[1]]
  vocab_split = str_split(vocabularies,",")[[1]]
  if(length(code_split)!=length(perc_split) |
     length(perc_split)!=length(vocab_split) |
     length(vocab_split)!=length(code_split)
  ){
    row$generic_sector_code = ""
    row$generic_sector_percentage = ""
    row$generic_sector_vocabulary = ""
    return(row)
  }
  row_df = data.frame(code=code_split,percent=perc_split,vocab=vocab_split)
  if("1" %in% vocab_split){
    row_df = subset(row_df,vocab=="1")
  }else if("2" %in% vocab_split){
    row_df = subset(row_df,vocab=="2")
  }else if("DAC" %in% vocab_split){
    row_df = subset(row_df,vocab=="DAC")
  }else{
    row_df = subset(row_df,is.na(vocab))
  }
  row$generic_sector_code = paste0(row_df$code,collapse=",")
  row$generic_sector_percentage = paste0(row_df$percent,collapse=",")
  row$generic_sector_vocabulary = paste0(row_df$vocab,collapse=",")
  return(row)
}

# dportal = fread("dportal_ug_ke_covid.csv")
# dportal_acts = unique(dportal[,"iati-identifier",with=F][[1]])
# dportal_acts = sapply(dportal_acts,URLdecode)
# 
# agg <- fread("iati_unfiltered_agg.csv",na.strings="")
# covid = subset(agg, iati_identifier %in% dportal_acts)
# transactions = subset(covid, budget_or_transaction == "Transaction")
# 
# save(transactions,file="ke_ug_covid.RData")
load("ke_ug_covid.RData")

# Create unique ID
transactions$unique_transaction_id = c(1:nrow(transactions))
transactions$transaction_value = as.numeric(transactions$usd_disbursement)
transaction$usd_disbursement = NULL

# Country split
transactions$generic_recipient_country_code = transactions[,"recipient_country_codes",with=F]
transactions$generic_recipient_country_percentage = transactions[,"recipient_country_percentages",with=F]
transactions$recipient_country_codes = NULL
transactions$recipient_country_percentages = NULL

transactions$transaction.id = c(1:nrow(transactions))
names(transactions) = gsub("_",".",names(transactions))
original_names = names(transactions)
transactions.split = cSplit(transactions,c("generic.recipient.country.code", "generic.recipient.country.percentage"),",")
new_names = setdiff(names(transactions.split),original_names)
transactions.split.long =reshape(transactions.split, varying=new_names, direction="long", sep="_")
transactions.split.long$generic.recipient.country.percentage = as.numeric(transactions.split.long$generic.recipient.country.percentage)
transactions.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(generic.recipient.country.percentage, na.rm=T) ) , by = transaction.id ]
transactions.split.long=subset(transactions.split.long, !is.na(generic.recipient.country.code) | max_count==1 | count==1)
transactions.split.long$generic.recipient.country.code = as.character(transactions.split.long$generic.recipient.country.code)
transactions.split.long$generic.recipient.country.code[which(is.na(transactions.split.long$generic.recipient.country.code))] = "Missing"
transactions.split.long$sum_percent[which(transactions.split.long$sum_percent==0)] = 100
transactions.split.long$generic.recipient.country.percentage[which(is.na(transactions.split.long$generic.recipient.country.percentage))] = transactions.split.long$sum_percent[which(is.na(transactions.split.long$generic.recipient.country.percentage))]
transactions.split.long$country.split.transaction.value=(transactions.split.long$generic.recipient.country.percentage/transactions.split.long$sum_percent)*transactions.split.long$transaction.value
transactions.split.long$country.split.transaction.value[which(is.na(transactions.split.long$country.split.transaction.value))] = transactions.split.long$transaction.value[which(is.na(transactions.split.long$country.split.transaction.value))]
transactions.split.long[,c("max_count", "count", "transaction.id", "id", "time", "sum_percent")] = NULL

transactions = transactions.split.long

# Sector split
transactions$generic_sector_code = transactions[,"transaction.sector.code",with=F]
transactions$generic_sector_percentage = transactions[,"transaction.sector.percentage",with=F]
transactions$generic_sector_vocabulary = transactions[,"transaction.sector.vocabulary",with=F]

for(i in 1:nrow(transactions)){
  transactions[i,] = single_vocabulary(transactions[i,])
}

transactions$transaction.id = c(1:nrow(transactions))
names(transactions) = gsub("_",".",names(transactions))
original_names = names(transactions)
transactions.split = cSplit(transactions,c("generic.sector.code", "generic.sector.percentage", "generic.sector.vocabulary"),",")
new_names = setdiff(names(transactions.split),original_names)
transactions.split.long =reshape(transactions.split, varying=new_names, direction="long", sep="_")
transactions.split.long$generic.sector.percentage = as.numeric(transactions.split.long$generic.sector.percentage)
transactions.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = transaction.id ]
transactions.split.long=subset(transactions.split.long, !is.na(generic.sector.code) | max_count==1 | count==1)
transactions.split.long$generic.sector.code = as.character(transactions.split.long$generic.sector.code)
transactions.split.long$generic.sector.vocabulary[which(is.na(transactions.split.long$generic.sector.vocabulary) & !is.na(transactions.split.long$generic.sector.code))] = "99"
transactions.split.long$generic.sector.code[which(is.na(transactions.split.long$generic.sector.code))] = "99810"
transactions.split.long$generic.sector.vocabulary[which(is.na(transactions.split.long$generic.sector.vocabulary))] = "1"
transactions.split.long$generic.sector.percentage[which(is.na(transactions.split.long$generic.sector.percentage))] = 100
transactions.split.long$sector.country.split.transaction.value=(transactions.split.long$generic.sector.percentage/100)*transactions.split.long$country.split.transaction.value
transactions.split.long$sector.country.split.transaction.value[which(is.na(transactions.split.long$sector.country.split.transaction.value))] = transactions.split.long$country.split.transaction.value[which(is.na(transactions.split.long$sector.country.split.transaction.value))]
transactions.split.long[,c("max_count", "count", "transaction.id", "id", "time")] = NULL

transactions = transactions.split.long 
names(transactions) = gsub(".","_",names(transactions),fixed=T)
transactions = subset(transactions, generic_recipient_country_code %in% c("UG","KE"))
fwrite(transactions,"ke_ug_covid.csv")