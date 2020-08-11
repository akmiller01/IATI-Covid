list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

single_vocabulary = function(row){
  codes = as.character(row$x_sector_code)
  percentages = as.character(row$x_sector_percentage)
  vocabularies = as.character(row$x_sector_vocabulary)
  
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
    row$x_sector_code = ""
    row$x_sector_percentage = ""
    row$x_sector_vocabulary = ""
    return(row)
  }
  row_df = data.frame(code=code_split,percent=perc_split,vocab=vocab_split)
  if("1" %in% vocab_split){
    row_df = subset(row_df,vocab=="1")
  }else if("2" %in% vocab_split){
    row_df = subset(row_df,vocab=="2")
  }else if("98" %in% vocab_split){
    row_df = subset(row_df,vocab=="98")
  }else if("99" %in% vocab_split){
    row_df = subset(row_df,vocab=="99")
  }else if("DAC" %in% vocab_split){
    row_df = subset(row_df,vocab=="DAC")
  }else{
    row_df = subset(row_df,is.na(vocab))
  }
  row$x_sector_code = paste0(row_df$code,collapse=",")
  row$x_sector_percentage = paste0(row_df$percent,collapse=",")
  row$x_sector_vocabulary = paste0(row_df$vocab,collapse=",")
  return(row)
}

# data_file = download.file(
#   "https://iatidatastore.iatistandard.org/search/activity?q=recipient_country_code:(TD)&wt=xslt&tr=activity-csv.xsl&rows=5000",
#   destfile="td_activities.csv"
# )

agg <- read.table(
  "td_activities.csv",
  header=T,
  sep=",",
  quote=c("\""),
  na.strings="",
  stringsAsFactors=FALSE,
  flush=T
)
write_excel_csv(agg,"td_activities_utf8.csv",na="")


# Split transactions
t_names = c("transaction.type.code","transaction.date.iso.date","transaction.value.currency","transaction.value.date","transaction.value","transaction.provider.org.provider.activity.id","transaction.provider.org.type","transaction.provider.org.ref","transaction.provider.org.narrative","transaction.receiver.org.receiver.activity.id","transaction.receiver.org.type","transaction.receiver.org.ref","transaction.receiver.org.narrative","transaction.disburstment.channel.code","transaction.sector.vocabulary","transaction.sector.code","transaction.recipient.country.code","transaction.recipient.region.code","transaction.recipient.region.vocabulary","transaction.flow.type.code","transaction.finance.type.code","transaction.aid.type.code","transaction.aid.type.vocabulary","transaction.tied.status.code")
agg$transaction.id = c(1:nrow(agg))
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,t_names,",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, (!is.na(transaction.type.code) & !is.na(transaction.value)) | max_count==1 | count==1)
agg.split.long[,c("max_count", "count", "transaction.id", "id", "time")] = NULL

agg = agg.split.long
names(agg) = gsub(".","_",names(agg),fixed=T)
write_excel_csv(agg,"Past Spending_Chad_split_t_qb.csv", na="")

agg$transaction_value_date = anydate(agg$transaction_value_date)
agg = subset(agg,transaction_value_date >= as.Date("2016-01-01"))

agg$x_sector_code = as.character(agg$transaction_sector_code)
agg$x_sector_vocabulary = agg$transaction_sector_vocabulary
agg$x_sector_percentage = "100"
agg$x_sector_vocabulary = as.character(agg$x_sector_vocabulary)
agg$x_sector_vocabulary[which(is.na(agg$x_sector_code))] = agg$sector_vocabulary[which(is.na(agg$x_sector_code))]
agg$x_sector_percentage[which(is.na(agg$x_sector_code))] = agg$sector_percentage[which(is.na(agg$x_sector_code))]
agg$x_sector_code[which(is.na(agg$x_sector_code))] = agg$sector_code[which(is.na(agg$x_sector_code))]
agg$transaction_value = as.numeric(agg$transaction_value)
pre = sum(agg$transaction_value,na.rm=T)
# Split by sector
agg.sector = data.table(agg[,c("x_sector_code","x_sector_vocabulary","x_sector_percentage")])
for(i in 1:nrow(agg.sector)){
  agg.sector[i,] = single_vocabulary(agg.sector[i,])
}
agg$x_sector_code = agg.sector$x_sector_code
agg$x_sector_percentage = agg.sector$x_sector_percentage
agg$x_sector_vocabulary = agg.sector$x_sector_vocabulary
agg$transaction.id = c(1:nrow(agg))
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,c("x.sector.code", "x.sector.percentage", "x.sector.vocabulary"),",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long$x.sector.percentage = as.numeric(agg.split.long$x.sector.percentage)
agg.split.long$x.sector.percentage[which(is.na(agg.split.long$x.sector.percentage))] = 100
agg.split.long$x.sector.percentage[which(is.na(agg.split.long$x.sector.code))] = NA
agg.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(x.sector.percentage, na.rm=T)) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, !is.na(x.sector.code) | max_count==1 | count==1)

agg.split.long$transaction.value.split=(agg.split.long$x.sector.percentage/agg.split.long$sum_percent)*agg.split.long$transaction.value
agg.split.long$transaction.value.split[which(is.na(agg.split.long$transaction.value.split))] = agg.split.long$transaction.value[which(is.na(agg.split.long$transaction.value.split))]
agg.split.long$transaction.value = agg.split.long$transaction.value.split
setdiff(unique(agg.split.long$transaction.id),c(1:nrow(agg)))
agg.split.long[,c("max_count", "count", "transaction.id", "id", "time", "transaction.value.split" ,"sum_percent")] = NULL

agg = agg.split.long
names(agg) = gsub(".","_",names(agg),fixed=T)
post = sum(agg$transaction_value,na.rm=T)
pre == post

sectors = fread("../Sector.csv")
sectors = sectors[,c("code","name")]
names(sectors) = c("x_sector_code","x_sector_name")
agg$x_sector_code = as.numeric(as.character(agg$x_sector_code))
agg$x_sector_vocabulary[which(is.na(agg$x_sector_code))] = 1
agg$x_sector_percentage[which(is.na(agg$x_sector_code))] = 100
agg$x_sector_code[which(is.na(agg$x_sector_code))] = 99810
agg = merge(agg,sectors,all.x=T)

sector_cats = fread("../SectorCategory.csv")
sector_cats = sector_cats[,c("code","name")]
names(sector_cats) = c("x_sector_cat_code","x_sector_cat_name")
agg$x_sector_cat_code = as.numeric(substr(as.character(agg$x_sector_code),1,3))
agg = merge(agg,sector_cats,by="x_sector_cat_code",all.x=T)

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

dagg =  agg

org_type = fread("../OrganisationType.csv")
org_type = org_type[,c("code","name")]
names(org_type) = c("reporting_org_type_code","reporting_org_type_name")
dagg$reporting_org_type_code = as.numeric(as.character(dagg$reporting_org_type_code))
dagg = merge(dagg,org_type,by="reporting_org_type_code",all.x=T)

dagg$x_currency = dagg$transaction_value_currency
dagg$x_currency[which(is.na(dagg$x_currency))] = dagg$default_currency[which(is.na(dagg$x_currency))] 

dagg$x_aid_type_code = dagg$transaction_aid_type_code
dagg$x_aid_type_code = as.character(dagg$x_aid_type_code)
dagg$x_aid_type_vocabulary = dagg$transaction_aid_type_vocabulary
dagg$x_aid_type_vocabulary = as.character(dagg$x_aid_type_vocabulary)
dagg$x_aid_type_vocabulary[which(is.na(dagg$x_aid_type_code))] = dagg$default_aid_type_vocabulary[which(is.na(dagg$x_aid_type_code))]
dagg$x_aid_type_code[which(is.na(dagg$x_aid_type_code))] = dagg$default_aid_type_code[which(is.na(dagg$x_aid_type_code))]

dagg$x_finance_type_code = dagg$transaction_finance_type_code
dagg$x_finance_type_code[which(is.na(dagg$x_finance_type_code))] = dagg$default_finance_type_code[which(is.na(dagg$x_finance_type_code))]

dagg_implementing = dagg[,c("participating_org_role","participating_org_narrative","participating_org_type","participating_org_ref")]
dagg_implementing$implementing_narrative = NA
dagg_implementing$implementing_type = NA
dagg_implementing$implementing_ref = NA
dagg_implementing = data.frame(dagg_implementing)
for(i in 1:nrow(dagg_implementing)){
  dagg_implementing[i,] = implementers(dagg_implementing[i,])
}
dagg_implementing = cSplit(dagg_implementing,c("implementing_narrative", "implementing_type", "implementing_ref"),",")
dagg_implementing[,c("participating_org_role","participating_org_narrative","participating_org_type","participating_org_ref")] = NULL
dagg = cbind(dagg,dagg_implementing)

dagg$x_transaction_provider_org = dagg$transaction_provider_org_narrative
dagg$x_transaction_provider_org = as.character(dagg$x_transaction_provider_org)

org_id = fread("../IATIOrganisationIdentifier.csv")
org_id = org_id[,c("code","name")]
names(org_id) = c("transaction_provider_org_ref","x_transaction_provider_org_recode")
dagg = merge(dagg,org_id,by="transaction_provider_org_ref",all.x=T)

dagg$x_transaction_provider_org[which(is.na(dagg$x_transaction_provider_org))] = dagg$x_transaction_provider_org_recode[which(is.na(dagg$x_transaction_provider_org))]
dagg$x_transaction_provider_org_recode = NULL

# Split recipient country
dagg$transaction_value = as.numeric(dagg$transaction_value)
# Fix for massive recipient activity
dagg$transaction_value[which(dagg$iati_identifier=="NO-BRC-980997278-970000")] = dagg$transaction_value[which(dagg$iati_identifier=="NO-BRC-980997278-970000")] * (0.699301/100)
dagg$recipient_country_code[which(dagg$iati_identifier=="NO-BRC-980997278-970000")] = "TD"
dagg$recipient_country_percentage[which(dagg$iati_identifier=="NO-BRC-980997278-970000")] = 100
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
write_excel_csv(dagg,"Past Spending_Chad_split_t_split_sector_edited_recode_qb.csv",na="")
