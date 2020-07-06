list.of.packages <- c("data.table", "openxlsx","reshape2","anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

header = names(read.csv("../sep/000header.csv"))
csvs = list.files(path="../sep",pattern="*.csv",full.names=T)
csvs = csvs[which(csvs!="../sep/000header.csv")]

agg_list_global = list()
agg_index_global = 1
agg_list_local = list()
agg_index_local = 1
agg_list_oda = list()
agg_index_oda = 1
pb = txtProgressBar(max=length(csvs),style=3)
file_count = 1

for(csv in csvs){
  tmp = read.csv(csv,header=F,col.names=header,na.strings="")
  tmp$x_transaction_date = anydate(tmp$x_transaction_date)
  tmp_global = subset(tmp,
   (grepl("covid-19|coronavirus", title_narrative, ignore.case=T) |
     grepl("covid-19|coronavirus", description_narrative, ignore.case=T) |
     grepl("covid-19|coronavirus", transaction_description_narrative, ignore.case=T) |
     humanitarian_scope_code == "EP-2020-000012-001" |
     humanitarian_scope_code == "HCOVD20" |
     tag_code == "COVID-19") &
   (x_transaction_date > as.Date("2020-02-29"))
  )
  if(nrow(tmp_global)>0){
    agg_list_global[[agg_index_global]] = tmp_global
    agg_index_global = agg_index_global + 1
  }
  tmp_local = subset(tmp_global,
    x_country_code %in% c("KE", "UG")
  )
  if(nrow(tmp_local)>0){
    agg_list_local[[agg_index_local]] = tmp_local
    agg_index_local = agg_index_local + 1
  }
  tmp_oda = subset(tmp,
   x_sector_vocabulary == 1 &
     reporting_org_type_code %in% c(10,15,40) &
     x_transaction_date >= as.Date("2019-01-01") &
     transaction_type_code %in% c(2, 3, 4)
  )
  if(nrow(tmp_oda)>0){
    agg_list_oda[[agg_index_oda]] = tmp_oda
    agg_index_oda = agg_index_oda + 1
  }
  file_count = file_count + 1
  setTxtProgressBar(pb, file_count)
}

agg_global = rbindlist(agg_list_global)
agg_local = rbindlist(agg_list_local)
agg_oda = rbindlist(agg_list_oda)
save(agg_global,agg_local,agg_oda,file="covid_analysis2.RData")
# load("covid_analysis2.RData")

fwrite(agg_global,"global_covid_filter.csv")
fwrite(agg_local,"ke_ug_covid_filter.csv")
fwrite(agg_oda,"oda_filter.csv")
