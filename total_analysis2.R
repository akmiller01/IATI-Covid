list.of.packages <- c("data.table", "openxlsx","reshape2","anytime")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

orgs = c(
	"41119",
	"41120",
	"44000",
	"46002",
	"46004",
	"47045",
	"47122",
	"41AAA",
	"AU-5",
	"CA-3",
	"CH-4",
	"DAC-1601",
	"DE-1",
	"ES-DIR3-E04585801",
	"ES-DIR3-EA0035768",
	"FI-3",
	"FR-3",
	"FR-6",
	"GB-COH-03877777",
	"GB-GOV-1",
	"GB-GOV-13",
	"GB-GOV-3",
	"GB-GOV-6",
	"KR-GOV-021",
	"NO-BRC-971277882",
	"NZ-1",
	"SE-0",
	"US-18",
	"US-GOV-1",
	"US-USAGOV",
	"XI-IATI-EBRD",
	"XI-IATI-EC_DEVCO",
	"XI-IATI-EC_ECHO",
	"XI-IATI-EC_FPI",
	"XI-IATI-EC_NEAR",
	"XI-IATI-IADB",
	"XI-IATI-OFID",
	"XI-IATI-UNPF",
	"XM-DAC-2-10",
	"XM-DAC-21-1",
	"XM-DAC-30010",
	"XM-DAC-3-1",
	"XM-DAC-41108",
	"XM-DAC-41110",
	"XM-DAC-41111",
	"XM-DAC-41114",
	"XM-DAC-41116",
	"XM-DAC-41121",
	"XM-DAC-41122",
	"XM-DAC-41123",
	"XM-DAC-41127",
	"XM-DAC-41130",
	"XM-DAC-41140",
	"XM-DAC-41146",
	"XM-DAC-41301",
	"XM-DAC-41302",
	"XM-DAC-41304",
	"XM-DAC-43000",
	"XM-DAC-47066",
	"XM-DAC-5-7",
	"XM-DAC-576",
	"XM-DAC-6-4",
	"XM-DAC-7",
	"XM-DAC-701-2",
	"XM-DAC-701-8",
	"XM-DAC-903",
	"XM-DAC-918-3",
	"XM-DAC-928",
	"XM-OCHA-CBPF",
	"XM-OCHA-CERF"
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
  tmp = subset(tmp,x_transaction_year==2020 & (x_sector_vocabulary==1) & (reporting_org_ref %in% orgs))
  if(nrow(tmp)>0){
    agg_list[[agg_index]] = tmp
    agg_index = agg_index + 1
  }
  file_count = file_count + 1
  setTxtProgressBar(pb, file_count)
}

dat = rbindlist(agg_list)

fwrite(dat,"transaction_row_subset_2020.csv")
