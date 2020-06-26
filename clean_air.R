list.of.packages <- c("data.table", "openxlsx","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

disb = c("E", "D", "3", "4")

agg <- fread("iati_unfiltered_agg.csv",na.strings="")

clean_air_file = "/home/alex/git/IATI-Covid/output/Clean_Air_Fund_IATI_Activity_Data.xlsx"
clean_air = read.xlsx(clean_air_file)
clean_air = subset(clean_air,Selected=="Y")

unique(clean_air$iati_identifier)[which(!unique(clean_air$iati_identifier) %in% agg$iati_identifier)]

agg = subset(agg, transaction_type %in% disb & iati_identifier %in% unique(clean_air$iati_identifier) & year %in% c(2018,2019))
agg$usd_disbursement = as.numeric(agg$usd_disbursement)
agg_tab = agg[,.(sum_usd_disbursement=sum(usd_disbursement,na.rm=T)),by=.(iati_identifier,year)]
agg_tab_m = melt(agg_tab,id.vars=c("iati_identifier","year"))
agg_tab_wide = dcast(agg_tab_m,iati_identifier~variable+year)
fwrite(agg_tab_wide,"clean_air_sum.csv")
