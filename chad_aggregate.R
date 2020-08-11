options(java.parameters = "- Xmx4096m")
list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr","openxlsx","xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

implementers = function(row){
  org_roles = as.character(row$participating_org_role)
  org_narratives = as.character(row$participating_org_narrative)
  org_types = as.character(row$participating_org_type)
  org_refs = as.character(row$participating_org_ref)
  
  role_split = str_split(org_roles,",")[[1]]
  narr_split = str_split(org_narratives,",")[[1]]
  type_split = str_split(org_types,",")[[1]]
  ref_split = str_split(org_refs,",")[[1]]
  max_len = max(length(role_split),length(narr_split),length(type_split),length(ref_split))
  if(length(role_split)<max_len){
    lendiff = max_len - length(role_split)
    role_split = c(role_split, rep("",lendiff))
  }
  if(length(narr_split)<max_len){
    lendiff = max_len - length(narr_split)
    narr_split = c(narr_split, rep("",lendiff))
  }
  if(length(type_split)<max_len){
    lendiff = max_len - length(type_split)
    type_split = c(type_split, rep("",lendiff))
  }
  if(length(ref_split)<max_len){
    lendiff = max_len - length(ref_split)
    ref_split = c(ref_split, rep("",lendiff))
  }
  row_df = data.frame(role=role_split,narr=narr_split,type=type_split,ref=ref_split)
  row_df = subset(row_df,role=="4")
  row$implementing_narrative = paste0(row_df$narr,collapse=",")
  row$implementing_type = paste0(row_df$type,collapse=",")
  row$implementing_ref = paste0(row_df$ref,collapse=",")
  return(row)
}

dagg =  xlsx::read.xlsx("Past Spending_Chad_split_t_split_sector_edited.xlsx", sheetIndex = 1, encoding="UTF-8")

org_type = fread("../OrganisationType.csv")
org_type = org_type[,c("code","name")]
names(org_type) = c("reporting_org_type_code","reporting_org_type_name")
dagg$reporting_org_type_code = as.numeric(as.character(dagg$reporting_org_type_code))
dagg = merge(dagg,org_type,all.x=T)

dagg$x_currency = dagg$transaction_value_currency
dagg$x_currency[which(is.na(dagg$x_currency))] = dagg$default_currency[which(is.na(dagg$x_currency))] 

dagg$x_aid_type_code = dagg$transaction_aid_type_code
dagg$x_aid_type_vocabulary = dagg$transaction_aid_type_vocabulary
dagg$x_aid_type_vocabulary[which(is.na(dagg$x_aid_type_code))] = dagg$default_aid_type_vocabulary[which(is.na(dagg$x_aid_type_code))]
dagg$x_aid_type_code[which(is.na(dagg$x_aid_type_code))] = dagg$default_aid_type_code[which(is.na(dagg$x_aid_type_code))]

dagg$x_finance_type_code = dagg$transaction_finance_type_code
dagg$x_finance_type_code[which(is.na(dagg$x_finance_type_code))] = dagg$default_finance_type_code[which(is.na(dagg$x_finance_type_code))]

dagg_implementing = dagg[,c("participating_org_role","participating_org_narrative","participating_org_type","participating_org_ref")]
dagg_implementing$implementing_narrative = NA
dagg_implementing$implementing_type = NA
dagg_implementing$implementing_ref = NA
for(i in 1:nrow(dagg_implementing)){
  dagg_implementing[i,] = implementers(dagg_implementing[i,])
}
dagg_implementing = cSplit(dagg_implementing,c("implementing_narrative", "implementing_type", "implementing_ref"),",")
dagg_implementing[,c("participating_org_role","participating_org_narrative","participating_org_type","participating_org_ref")] = NULL
dagg = cbind(dagg,dagg_implementing)

dagg$x_transaction_provider_org = dagg$transaction_provider_org_narrative

org_id = fread("../IATIOrganisationIdentifier.csv")
org_id = org_id[,c("code","name")]
names(org_id) = c("transaction_provider_org_ref","x_transaction_provider_org_recode")
dagg = merge(dagg,org_id,all.x=T)

dagg$x_transaction_provider_org[which(is.na(dagg$x_transaction_provider_org))] = dagg$x_transaction_provider_org_recode[which(is.na(dagg$x_transaction_provider_org))]
dagg$x_transaction_provider_org_recode = NULL

# Split recipient country
dagg$transaction_value = as.numeric(dagg$transaction_value)
pre = sum(dagg$transaction_value,na.rm=T)
dagg$transaction.id = c(1:nrow(dagg))
names(dagg) = gsub("_",".",names(dagg))
original_names = names(dagg)
agg.split = cSplit(dagg,c("recipient.country.code", "recipient.country.percentage"),",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long$transaction.value = as.numeric(agg.split.long$transaction.value)
agg.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(recipient.country.percentage, na.rm=T) ) , by = transaction.id ]
agg.split.long=subset(agg.split.long, !is.na(recipient.country.code) | max_count==1 | count==1)
agg.split.long$transaction.value.split=(agg.split.long$recipient.country.percentage/agg.split.long$sum_percent)*agg.split.long$transaction.value
agg.split.long$transaction.value.split[which(is.na(agg.split.long$transaction.value.split))] = agg.split.long$transaction.value[which(is.na(agg.split.long$transaction.value.split))]
agg.split.long$chad.transaction.value = agg.split.long$transaction.value.split
agg.split.long[,c("transaction.value.split", "max_count", "count", "transaction.id", "id", "time", "sum_percent")] = NULL
post = sum(agg.split.long$chad.transaction.value,na.rm=T)
pre == post
dagg = subset(agg.split.long,recipient.country.code %in% c("TD","TCD"))
names(dagg) = gsub(".","_",names(dagg),fixed=T)
fwrite(dagg,"Past Spending_Chad_split_t_split_sector_edited_recode.csv")
