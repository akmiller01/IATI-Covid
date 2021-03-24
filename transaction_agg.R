list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)


dat = fread("https://docs.google.com/spreadsheets/d/1hBCnc6w4Th_yAuM66FQdfRAZcWxveTVodYBHkzMg0Y4/export?format=csv&id=1hBCnc6w4Th_yAuM66FQdfRAZcWxveTVodYBHkzMg0Y4&gid=434569659",na.strings="")

dat = subset(dat,year>=2020 & transaction_type=="3")
dat_pub = dat[,.(usd_disbursement=sum(usd_disbursement,na.rm=T)),by=.(reporting_org_name)]
dat_pub = dat_pub[order(-dat_pub$usd_disbursement),]
fwrite(dat_pub,"feb_2021_covid_related_transactions_by_publisher.csv")

agg = dat
agg$transaction_id = c(1:nrow(agg))
t_names = c("recipient.country.codes","recipient.country.percentages")
names(agg) = gsub("_",".",names(agg))
original_names = names(agg)
agg.split = cSplit(agg,t_names,",")
new_names = setdiff(names(agg.split),original_names)
agg.split.long = reshape(agg.split, varying=new_names, direction="long", sep="_")
agg.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = .(transaction.id) ]
agg.split.long=subset(agg.split.long, !is.na(recipient.country.codes) | max_count==1 | count==1)
agg.split.long[,c("max_count", "count", "transaction.id", "id", "time")] = NULL
agg.split.long$recipient.country.percentages[which(is.na(agg.split.long$recipient.country.percentages))] = 100
agg.split.long$usd.disbursement = agg.split.long$usd.disbursement * (agg.split.long$recipient.country.percentages/100)
dat_recip = agg.split.long[,.(usd_disbursement=sum(usd.disbursement,na.rm=T)),by=.(recipient.country.codes)]
dat_recip = dat_recip[order(-dat_recip$usd_disbursement),]
fwrite(dat_recip,"feb_2021_covid_related_transactions_by_recipient.csv")
