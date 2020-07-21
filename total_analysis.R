list.of.packages <- c("data.table", "openxlsx","reshape2","anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

header = names(read.csv("../sep/000header.csv"))
csvs = list.files(path="../sep",pattern="*.csv",full.names=T)
csvs = csvs[which(csvs!="../sep/000header.csv")]

agg_list = list()
agg_index = 1
pb = txtProgressBar(max=length(csvs),style=3)
file_count = 1

for(csv in csvs){
  tmp = read.csv(csv,header=F,col.names=header,na.strings="")
  tmp$x_sector_vocabulary = as.character(tmp$x_sector_vocabulary)
  tmp$x_default_vocabulary = as.character(tmp$x_default_vocabulary)
  tmp = subset(tmp,x_transaction_year==2019 & (transaction_type_code %in% c("E", "D", "3", "4")) & (x_sector_vocabulary==x_default_vocabulary))
  if(nrow(tmp)>0){
    tmp_tab = data.table(tmp)[,.(total_disb_exp=sum(x_transaction_value_usd,na.rm=T)),by=.(reporting_org_ref)]
    agg_list[[agg_index]] = tmp_tab
    agg_index = agg_index + 1
  }
  file_count = file_count + 1
  setTxtProgressBar(pb, file_count)
}

dat = rbindlist(agg_list)
dat_tab = dat[,.(total_disb_exp=sum(total_disb_exp,na.rm=T)),by=.(reporting_org_ref)]


fwrite(dat_tab,"total_disb_exp_2019.csv")
