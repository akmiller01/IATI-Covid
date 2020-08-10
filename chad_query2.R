list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

single_vocabulary = function(row){
  codes = as.character(row$sector_code)
  percentages = as.character(row$sector_percentage)
  vocabularies = as.character(row$sector_vocabulary)
  
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
    row$sector_code = ""
    row$sector_percentage = ""
    row$sector_vocabulary = ""
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
  row$sector_code = paste0(row_df$code,collapse=",")
  row$sector_percentage = paste0(row_df$percent,collapse=",")
  row$sector_vocabulary = paste0(row_df$vocab,collapse=",")
  return(row)
}

agg <- read.xlsx("/home/alex/git/IATI-Covid/output/Chad Budget Data_waggregated quarters.xlsx", sheet=1)
pre_total = sum(agg$Budget.Q1.2021,na.rm=T)

# Split by sector
for(i in 1:nrow(agg)){
  agg[i,] = single_vocabulary(agg[i,])
}

agg$transaction.id = c(1:nrow(agg))
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,c("sector.code", "sector.percentage", "sector.vocabulary"),",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N, sum_percent=sum(sector.percentage, na.rm=T) ) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, !is.na(sector.code) | max_count==1 | count==1)
agg.split.long[,c("max_count", "count", "transaction.id", "id", "time", "sum_percent")] = NULL

agg = agg.split.long
names(agg) = gsub(".","_",names(agg),fixed=T)
agg$sector_vocabulary[which(is.na(agg$sector_code))] = 1
agg$sector_percentage[which(is.na(agg$sector_code))] = 100
agg$sector_code[which(is.na(agg$sector_code))] = 99810

budget_cols = c(
  "Budget_Q1_2021",
  "Budget_Q2_2021",                                   
  "Budget_Q3_2021",
  "Budget_Q4_2021",                                   
  "Budget_Q1_2022",
  "Budget_Q2_2022",                                   
  "Budget_Q3_2022",
  "Budget_Q4_2022",                                   
  "Budget_Q1_2023",
  "Budget_Q2_2023",                                   
  "Budget_Q3_2023",
  "Budget_Q4_2023"
)
for(budget_col in budget_cols){
  agg[,budget_col] = agg[,budget_col,with=F] * (agg$sector_percentage/100)
  setnames(agg,budget_col,gsub("_",".",budget_col,fixed=T)) 
}

post_total = sum(agg$Budget.Q1.2021,na.rm=T)

sectors = fread("../Sector.csv")
sectors = sectors[,c("code","name")]
names(sectors) = c("sector_code","sector_name")
agg$sector_code = as.numeric(as.character(agg$sector_code))
agg = merge(agg,sectors,all.x=T)

pre_total == post_total

fwrite(agg,"Chad Budget Data_waggregated quarters_sectorsplit.csv")
