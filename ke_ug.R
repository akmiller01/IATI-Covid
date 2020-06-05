list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/IATI-Covid/output")

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

# activity_url = paste0(
#   "https://iati.cloud/search/activity?q=(recipient_country_code:(KE UG) OR transaction_recipient_country_code:(KE UG))",
#   " AND transaction_date_iso_date:[2020-01-01T00:00:00Z TO *]",
#   "&fl=",
#   "default_currency,humanitarian,iati_identifier,reporting_org_*,title_*,",
#   "description_*,participating_org_*,activity_scope_code,recipient_country_*,",
#   "recipient_region_*,sector_*,tag_*,humanitarian_scope_*,policy_marker_*,collaboration_type_code,",
#   "default_flow_type_code,default_finance_type_code,default_aid_type_*,default_tied_status_code,capital_spend_percentage",
#   "&wt=csv&rows=5000000"
# )
# activities = fread(URLencode(activity_url))
# 
# ids = activities$iati_identifier
# ids[which(ids == "NL-KVK-61172863-NL-KVK-61172863-DGGF I&P Afrique Enterpreneurs II")] = "NL-KVK-61172863-NL-KVK-61172863-DGGF I"
# activity_filter_vec = paste0('"',ids,'"')
# 
# transaction_list = list()
# transaction_index = 1
# i = 1
# while(i < length(activity_filter_vec)){
#   start_index = i
#   end_index = min(i + 99, length(activity_filter_vec))
#   activity_filter = paste0(activity_filter_vec[start_index:end_index],collapse=" ")
# 
#   transaction_url = paste0(
#     "https://iati.cloud/search/transaction?q=",
#     "iati_identifier: (",activity_filter,")",
#     " AND transaction_date_iso_date:[2020-01-01T00:00:00Z TO *]",
#     "&fl=",
#     "iati_identifier,transaction_ref,transaction_humanitarian,transaction_type,",
#     "transaction_date_iso_date,transaction_valu*,transaction_provider_org_*,",
#     "transaction_receiver_org_*,transaction_disbursement_channel_code,",
#     "transaction_sector_*,transaction_recipient_*,transaction_flow_type_code,",
#     "transaction_finance_type_code,transaction_aid_type_*,transaction_tied_status_code",
#     "&wt=csv&rows=5000000"
#   )
#   transactions_tmp = fread(URLencode(transaction_url))
#   transaction_list[[transaction_index]] = transactions_tmp
#   transaction_index = transaction_index + 1
# 
#   i = i + 100
# }
# 
# transactions = rbindlist(transaction_list)
# 
# save(activities,transactions,file="ke_ug.RData")
load("ke_ug.RData")

# Join activity_level_data
transactions = merge(transactions,activities,by="iati_identifier")

# Country split
transactions$generic_recipient_country_code = transactions[,"transaction_recipient_country_code",with=F]
transactions$generic_recipient_country_percentage = "100"
transactions$generic_recipient_country_percentage[which(is.na(transactions$generic_recipient_country_code))] = transactions$recipient_country_percentage[which(is.na(transactions$generic_recipient_country_code))]
transactions$generic_recipient_country_code[which(is.na(transactions$generic_recipient_country_code))] = transactions$recipient_country_code[which(is.na(transactions$generic_recipient_country_code))]

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
transactions.split.long$country.split.transaction.value=(transactions.split.long$generic.recipient.country.percentage/transactions.split.long$sum_percent)*transactions.split.long$transaction.value
transactions.split.long$country.split.transaction.value[which(is.na(transactions.split.long$country.split.transaction.value))] = transactions.split.long$transaction.value[which(is.na(transactions.split.long$country.split.transaction.value))]
transactions.split.long[,c("max_count", "count", "transaction.id", "id", "time", "sum_percent")] = NULL

transactions = transactions.split.long

# Sector split
transactions$generic_sector_code = transactions[,"transaction.sector.code",with=F]
transactions$generic_sector_percentage = "100"
transactions$generic_sector_vocabulary = "1"
transactions$generic_sector_percentage[which(is.na(transactions$generic_sector_code))] = transactions$sector.percentage[which(is.na(transactions$generic_sector_code))]
transactions$generic_sector_vocabulary[which(is.na(transactions$generic_sector_code))] = transactions$sector.vocabulary[which(is.na(transactions$generic_sector_code))]
transactions$generic_sector_code[which(is.na(transactions$generic_sector_code))] = transactions$sector.code[which(is.na(transactions$generic_sector_code))]

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
transactions.split.long[ , `:=`( max_count = .N , count = 1:.N , sum_percent=sum(generic.sector.percentage, na.rm=T)) , by = transaction.id ]
transactions.split.long=subset(transactions.split.long, !is.na(generic.sector.code) | max_count==1 | count==1)
transactions.split.long$generic.sector.code = as.character(transactions.split.long$generic.sector.code)
transactions.split.long$generic.sector.code[which(is.na(transactions.split.long$generic.sector.code))] = "99810"
transactions.split.long$sum_percent[which(transactions.split.long$sum_percent==0)] = 100
transactions.split.long$sector.country.split.transaction.value=(transactions.split.long$generic.sector.percentage/transactions.split.long$sum_percent)*transactions.split.long$country.split.transaction.value
transactions.split.long$sector.country.split.transaction.value[which(is.na(transactions.split.long$sector.country.split.transaction.value))] = transactions.split.long$country.split.transaction.value[which(is.na(transactions.split.long$sector.country.split.transaction.value))]
transactions.split.long[,c("max_count", "count", "transaction.id", "id", "time", "sum_percent")] = NULL

transactions = transactions.split.long 
names(transactions) = gsub(".","_",names(transactions),fixed=T)
transactions = subset(transactions, generic_recipient_country_code %in% c("UG","KE"))
fwrite(transactions,"ke_ug.csv")
