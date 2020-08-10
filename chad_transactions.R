list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr","openxlsx")
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

agg <- read.xlsx("/home/alex/git/IATI-Covid/output/Past Spending_Chad_to_split_transactions.xlsx", sheet=1)
agg[,c(paste0("X",221:231))] = NULL


# Split transactions
t_names = c("transaction.type.code","transaction.date.iso.date","transaction.value.currency","transaction.value.date","transaction.value","transaction.provider.org.provider.activity.id","transaction.provider.org.type","transaction.provider.org.ref","transaction.provider.org.narrative","transaction.receiver.org.receiver.activity.id","transaction.receiver.org.type","transaction.receiver.org.ref","transaction.receiver.org.narrative","transaction.disburstment.channel.code","transaction.sector.vocabulary","transaction.sector.code","transaction.recipient.country.code","transaction.recipient.region.code","transaction.recipient.region.vocabulary","transaction.flow.type.code","transaction.finance.type.code","transaction.aid.type.code","transaction.aid.type.vocabulary","transaction.tied.status.code")
agg$transaction.id = c(1:nrow(agg))
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,t_names,",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, !is.na(transaction.type.code) | max_count==1 | count==1)
agg.split.long[,c("max_count", "count", "transaction.id", "id", "time")] = NULL

agg = agg.split.long
names(agg) = gsub(".","_",names(agg),fixed=T)
fwrite(agg,"Past Spending_Chad_split_t.csv")

agg$x_sector_code = as.character(agg$transaction_sector_code)
agg$x_sector_vocabulary = agg$transaction_sector_vocabulary
agg$x_sector_percentage = "100"
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

fwrite(agg,"Past Spending_Chad_split_t_split_sector.csv")

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

fwrite(agg,"Past Spending_Chad_split_t_split_sector.csv")
