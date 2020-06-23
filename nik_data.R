list.of.packages <- c("data.table", "openxlsx", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

single_vocabulary = function(row){
  codes = as.character(row$transaction_sector_code)
  percentages = as.character(row$transaction_sector_percentage)
  vocabularies = as.character(row$transaction_sector_vocabulary)
  
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
    row$transaction_sector_code = ""
    row$transaction_sector_percentage = ""
    row$transaction_sector_vocabulary = ""
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
  row$transaction_sector_code = paste0(row_df$code,collapse=",")
  row$transaction_sector_percentage = paste0(row_df$percent,collapse=",")
  row$transaction_sector_vocabulary = paste0(row_df$vocab,collapse=",")
  return(row)
}

keep = c("iati_identifier", "reporting_org_name", "reporting_org_ref",
         "reporting_org_type", "value_usd", "transaction_date",
         "transaction_type", "publisher", "humanitarian", "humanitarian_scope_code",
         "humanitarian_scope_narrative", "finance_type_code", "transaction_sector_code",
         "transaction_sector_percentage", "transaction_sector_vocabulary", "recipient_country_codes",
         "recipient_country_percentages")

agg <- fread("iati_unfiltered_agg.csv",na.strings="")

ids = unique(read.xlsx("/home/alex/git/IATI-Covid/output/C-19 IATI identifiers.xlsx")$iati_identifier)

ids[which(!ids %in% agg$iati_identifier)]

agg = subset(agg, budget_or_transaction == "Transaction" & iati_identifier %in% ids)
agg$value_usd = as.numeric(agg$usd_disbursement)

transactions = agg[,keep,with=F]

for(i in 1:nrow(transactions)){
  transactions[i,] = single_vocabulary(transactions[i,])
}

fwrite(transactions, "nik_transactions.csv")

