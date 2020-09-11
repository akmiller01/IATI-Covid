list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

org_id_imp = fread("../IATIOrganisationIdentifier.csv")
org_id_imp = org_id_imp[,c("code","name")]
names(org_id_imp) = c("ref","recode")

implementers = function(row){
  org_roles = as.character(row$participating_org_role)
  org_narratives = as.character(row$participating_org_narrative)
  org_refs = as.character(row$participating_org_ref)
  
  role_split = str_split(org_roles,",")[[1]]
  narr_split = str_split(org_narratives,",")[[1]]
  ref_split = str_split(org_refs,",")[[1]]
  max_len = max(length(role_split),length(narr_split),length(ref_split))
  if(length(role_split)<max_len){
    lendiff = max_len - length(role_split)
    role_split = c(role_split, rep("",lendiff))
  }
  if(length(narr_split)<max_len){
    lendiff = max_len - length(narr_split)
    narr_split = c(narr_split, rep("",lendiff))
  }
  if(length(ref_split)<max_len){
    lendiff = max_len - length(ref_split)
    ref_split = c(ref_split, rep("",lendiff))
  }
  row_df = data.frame(role=role_split,narr=narr_split,ref=ref_split)
  row_df = subset(row_df,role=="4")
  row_df = merge(row_df,org_id_imp,by="ref",all.x=T)
  row_df$narr[which(is.na(row_df$narr))] = row_df$recode[which(is.na(row_df$narr))]
  row$implementing_narrative = paste0(row_df$narr,collapse=",")
  return(row)
}

# download.file(
#   "https://iatidatastore.iatistandard.org/search/transaction?q=transaction_recipient_country_code:(TD)%20AND%20transaction_type:(3)&wt=xslt&tr=transaction-csv.xsl&rows=2500",
#   destfile="td_transactions.csv"
# )

trans <- read.table(
  "td_transactions.csv",
  header=T,
  sep=",",
  quote=c("\""),
  na.strings="",
  stringsAsFactors=FALSE,
  flush=T
)
trans$transaction_date_iso_date = anydate(trans$transaction_date_iso_date)
trans = subset(trans,transaction_date_iso_date >= as.Date("2016-01-01"))
write_excel_csv(trans,"td_transactions_utf8.csv",na="")

search_terms = unique(trans$iati_identifier)

search_fields = c(
  "iati_identifier"
)

search_query = ""
search_grid = expand.grid(search_fields, search_terms, stringsAsFactors = F)
for(i in 1:nrow(search_grid)){
  row = search_grid[i,]
  query = paste0(row[1], ':"', row[2], '"')
  if(i != nrow(search_grid)){
    query = paste0(query, " OR ")
  }
  search_query = paste0(search_query, query)
}

search_url = paste0(
  'https://iatidatastore.iatistandard.org/search/activity?q=(',
  search_query,
  ')&wt=xslt&tr=activity-csv.xsl&rows=2500'
)

# download.file(
#   URLencode(search_url),
#   destfile="td_transaction_activities.csv"
# )

act <- read.table(
  "td_transaction_activities.csv",
  header=T,
  sep=",",
  quote=c("\""),
  na.strings="",
  stringsAsFactors=FALSE,
  flush=T
)
write_excel_csv(act,"td_transaction_activities_utf8.csv",na="")

overlapping_names = intersect(names(act),names(trans))
overlapping_names = overlapping_names[which(overlapping_names!="iati_identifier")]
act[,overlapping_names] = NULL

agg = merge(trans,act,by="iati_identifier")

agg_implementing = agg[,c("participating_org_role","participating_org_narrative","participating_org_ref")]
agg_implementing$implementing_narrative = NA
agg_implementing = data.frame(agg_implementing)
for(i in 1:nrow(agg_implementing)){
  agg_implementing[i,] = implementers(agg_implementing[i,])
}
agg_implementing = cSplit(agg_implementing,c("implementing_narrative"),",")
agg_implementing[,c("participating_org_role","participating_org_narrative","participating_org_ref")] = NULL
agg = cbind(agg,agg_implementing)

agg$x_currency = agg$transaction_value_currency
agg$x_currency[which(is.na(agg$x_currency))] = agg$default_currency[which(is.na(agg$x_currency))] 

agg$x_aid_type_code = agg$transaction_aid_type_code
agg$x_aid_type_code = as.character(agg$x_aid_type_code)
agg$x_aid_type_vocabulary = agg$transaction_aid_type_vocabulary
agg$x_aid_type_vocabulary = as.character(agg$x_aid_type_vocabulary)
agg$x_aid_type_vocabulary[which(is.na(agg$x_aid_type_code))] = agg$default_aid_type_vocabulary[which(is.na(agg$x_aid_type_code))]
agg$x_aid_type_code[which(is.na(agg$x_aid_type_code))] = agg$default_aid_type_code[which(is.na(agg$x_aid_type_code))]

agg$x_finance_type_code = agg$transaction_finance_type_code
agg$x_finance_type_code[which(is.na(agg$x_finance_type_code))] = agg$default_finance_type_code[which(is.na(agg$x_finance_type_code))]


finance_types = fread("../FinanceType.csv")
finance_types = finance_types[,c("code","name")]
names(finance_types) = c("x_finance_type_code","x_finance_type_name")
agg$x_finance_type_code = as.numeric(agg$x_finance_type_code)
agg = merge(agg,finance_types,by="x_finance_type_code",all.x=T)

aid_types = fread("../AidType.csv")
aid_types = aid_types[,c("code","name")]
names(aid_types) = c("x_aid_type_code","x_aid_type_name")
agg$x_aid_type_code = as.character(gsub(",","",agg$x_aid_type_code))
agg = merge(agg,aid_types,by="x_aid_type_code",all.x=T)

sectors = fread("../Sector.csv")
sectors = sectors[,c("code","name")]
names(sectors) = c("transaction_sector_code","transaction_sector_name")
agg$transaction_sector_code = as.numeric(gsub(",","",as.character(agg$transaction_sector_code)))
agg$transaction_sector_vocabulary[which(is.na(agg$transaction_sector_code))] = 1
agg$transaction_sector_code[which(is.na(agg$transaction_sector_code))] = 99810
agg = merge(agg,sectors,all.x=T)

ex_rates = fread("../ex_rates.csv")
agg$year = as.numeric(substr(as.character(agg$transaction_date_iso_date),1,4))
names(ex_rates) = c("year","x_currency","ex_rate")
agg = merge(agg,ex_rates,by=c("year","x_currency"), all.x=T)
agg$transaction_value_usd = agg$transaction_value * agg$ex_rate

write_excel_csv(agg,"td_transactions_recode.csv",na="")
