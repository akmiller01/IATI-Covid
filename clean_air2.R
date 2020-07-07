list.of.packages <- c("data.table", "openxlsx","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

clean_air_file = "Clean_Air_Fund_IATI_Activity_Data.xlsx"
clean_air = read.xlsx(clean_air_file)
clean_air = subset(clean_air,Selected=="Y")
clean_air_ids = unique(clean_air$iati_identifier)

header = names(read.csv("../sep/000header.csv"))
csvs = list.files(path="../sep",pattern="*.csv",full.names=T)
csvs = csvs[which(csvs!="../sep/000header.csv")]

agg_list = list()
agg_index = 1
pb = txtProgressBar(max=length(csvs),style=3)
file_count = 1

for(csv in csvs){
  tmp = read.csv(csv,header=F,col.names=header,na.strings="")
  tmp = subset(tmp,iati_identifier %in% clean_air_ids)
  if(nrow(tmp)>0){
    agg_list[[agg_index]] = tmp
    agg_index = agg_index + 1
  }
  file_count = file_count + 1
  setTxtProgressBar(pb, file_count)
}

agg = rbindlist(agg_list)
# save(agg,file="clean_air_agg.R")
load("clean_air_agg.R")

setdiff(clean_air_ids,unique(agg$iati_identifier))
disb = c("E", "D", "3", "4")

agg = subset(agg, transaction_type_code %in% disb & x_transaction_year %in% c(2018,2019))
agg = subset(agg,x_sector_vocabulary==x_default_vocabulary)
keep = c("iati_identifier","x_finance_type_code","x_flow_type_code","x_transaction_year","x_transaction_value_usd")
agg = agg[,keep,with=F]

agg.tab = agg[,.(sum_usd_disbursement=sum(x_transaction_value_usd,na.rm=T)),by=.(iati_identifier,x_transaction_year,x_finance_type_code,x_flow_type_code)]

finance_types = fread("../FinanceType.csv")
finance_types = finance_types[,c("code","name")]
names(finance_types) = c("x_finance_type_code","finance_type")
agg.tab$x_finance_type_code = as.numeric(agg.tab$x_finance_type_code)
agg.tab = merge(agg.tab,finance_types,by="x_finance_type_code",all.x=T)

flow_types = fread("../FlowType.csv")
flow_types = flow_types[,c("code","name")]
names(flow_types) = c("x_flow_type_code","flow_type")
agg.tab$x_flow_type_code = as.numeric(agg.tab$x_flow_type_code)
agg.tab = merge(agg.tab,flow_types,by="x_flow_type_code",all.x=T)

agg.tab[,c("x_finance_type_code","x_flow_type_code")] = NULL

agg_tab_m = melt(agg.tab,id.vars=c("iati_identifier","x_transaction_year","flow_type","finance_type"))
agg_tab_wide = dcast(agg_tab_m,iati_identifier+flow_type+finance_type~variable+x_transaction_year)
agg_tab_wide = agg_tab_wide[order(agg_tab_wide$iati_identifier),]
length(setdiff(clean_air_ids,unique(agg_tab_wide$iati_identifier)))
fwrite(agg_tab_wide,"clean_air_sum2.csv")
