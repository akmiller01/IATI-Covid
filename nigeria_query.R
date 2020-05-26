list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

single_vocabulary = function(row){
  codes = as.character(row$transaction.sector.code)
  percentages = as.character(row$transaction.sector.percentage)
  vocabularies = as.character(row$transaction.sector.vocabulary)
  
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
    row$transaction.sector.code = ""
    row$transaction.sector.percentage = ""
    row$transaction.sector.vocabulary = ""
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
  row$transaction.sector.code = paste0(row_df$code,collapse=",")
  row$transaction.sector.percentage = paste0(row_df$percent,collapse=",")
  row$transaction.sector.vocabulary = paste0(row_df$vocab,collapse=",")
  return(row)
}

agg <- fread("iati_unfiltered_agg.csv")
agg = subset(
  agg,
  secondary_reporter %in% c("0","false") &
  budget_or_transaction=="Transaction" &
  year>=2016 &
  transaction_type %in% c("E", "D", "3", "4") & (
    grepl("NG",recipient_country_codes) |
      grepl("NGA", recipient_country_codes)
  )
)
agg[,c("budget_period_start", "budget_period_end", "budget_type", "budget_or_transaction")] = NULL

# Split recipient country
agg$transaction.id = c(1:nrow(agg))
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,c("recipient.country.codes", "recipient.country.percentages"),",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(recipient.country.percentages, na.rm=T) ) , by = transaction.id ]
agg.split.long=subset(agg.split.long, !is.na(recipient.country.codes) | max_count==1 | count==1)
agg.split.long$usd.value=(agg.split.long$recipient.country.percentages/agg.split.long$sum_percent)*agg.split.long$usd.disbursement
agg.split.long$usd.value[which(is.na(agg.split.long$usd.value))] = agg.split.long$usd.disbursement[which(is.na(agg.split.long$usd.value))]
agg.split.long[,c("usd.disbursement", "max_count", "count", "transaction.id", "id", "time", "sum_percent","recipient.country.percentages")] = NULL

agg = subset(agg.split.long,recipient.country.codes %in% c("NG","NGA"))
setnames(agg, "usd.value", "usd.disbursement")

# Split by sector
for(i in 1:nrow(agg)){
  agg[i,] = single_vocabulary(agg[i,])
}

agg$transaction.id = c(1:nrow(agg))
original_names = names(agg)
agg.split = cSplit(agg,c("transaction.sector.code", "transaction.sector.percentage", "transaction.sector.vocabulary"),",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(transaction.sector.percentage, na.rm=T) ) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, !is.na(transaction.sector.code) | max_count==1 | count==1)
agg.split.long$usd.value=(agg.split.long$transaction.sector.percentage/agg.split.long$sum_percent)*agg.split.long$usd.disbursement
agg.split.long$usd.value[which(is.na(agg.split.long$usd.value))] = agg.split.long$usd.disbursement[which(is.na(agg.split.long$usd.value))]
agg.split.long[,c("usd.disbursement", "max_count", "count", "transaction.id", "id", "time", "sum_percent","transaction.sector.percentage")] = NULL

agg = agg.split.long

sectors = fread("../Sector.csv")
sectors = sectors[,c("code","name")]
names(sectors) = c("transaction.sector.code","transaction.sector.name")

sector_cats = fread("../SectorCategory.csv")
sector_cats = sector_cats[,c("code","name")]
names(sector_cats) = c("transaction.sector.cat.code","transaction.sector.category.name")

org_types = fread("../OrganisationType.csv")
org_types = org_types[,c("code","name")]
names(org_types) = c("reporting.org.type","reporting.org.type.name")

finance_types = fread("../FinanceType.csv")
finance_types = finance_types[,c("code","name")]
names(finance_types) = c("finance.type.code","finance.type.name")

agg$transaction.sector.code = as.numeric(as.character(agg$transaction.sector.code))
agg$transaction.sector.code[which(is.na(agg$transaction.sector.code))] = 99810
agg = merge(agg,sectors,all.x=T)
agg$transaction.sector.cat.code = as.numeric(substr(agg$transaction.sector.code,1,3))
agg = merge(agg,sector_cats,by="transaction.sector.cat.code",all.x=T)
agg$reporting.org.type = as.numeric(agg$reporting.org.type)
agg = merge(agg,org_types,by="reporting.org.type",all.x=T)
agg$finance.type.code = as.numeric(agg$finance.type.code)
agg = merge(agg,finance_types,by="finance.type.code",all.x=T)

remove = c("finance.type.code", "reporting.org.type","secondary.reporter","humanitarian")
agg[,remove]=NULL

fwrite(agg,"nigeria_query_2016.csv")
