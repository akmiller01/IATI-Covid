list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

test.ids = c(
  "XM-DAC-41122-REPUBLIC OF MONTENEGRO-8950/A0/04/001/001",
  "US-GOV-1-7200AA18C00070",
  "XM-DAC-41114-OUTPUT-00119509",
  "XI-IATI-EC_ECHO-ECHO/-AF/BUD/2019/91013",
  "44000-P127306",
  "XM-DAC-41121-2020",
  "XI-IATI-EC_DEVCO-2020/415-636",
  "XM-DAC-928-AF-2020-21-13.003.EM01.AFG02",
  "NL-KVK-27108436-A-05823-15:UG",
  "XM-DAC-5-7-6614286"
)

header = names(read.csv("../sep/000header.csv"))
csvs = list.files(path="../sep",pattern="*.csv",full.names=T)
csvs = csvs[which(csvs!="../sep/000header.csv")]

agg_list = list()
agg_index = 1
pb = txtProgressBar(max=length(csvs),style=3)
file_count = 1

for(csv in csvs){
  tmp = read.csv(csv,header=F,col.names=header,na.strings="")
  tmp = subset(tmp,toupper(iati_identifier) %in% test.ids)
  if(nrow(tmp)>0){
    agg_list[[agg_index]] = tmp
    agg_index = agg_index + 1
  }
  file_count = file_count + 1
  setTxtProgressBar(pb, file_count)
}

agg = rbindlist(agg_list)
save(agg,file="test_agg.R")
load("test_agg.R")

setdiff(test.ids,toupper(unique(agg$iati_identifier)))

fwrite(agg, "transaction_spec_test.csv")
