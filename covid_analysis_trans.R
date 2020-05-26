list.of.packages <- c("data.table", "anytime", "Hmisc","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

comm = c("C", "2")
disb = c("E", "D", "3", "4")

agg <- fread("iati_unfiltered_agg.csv")
agg = subset(agg, secondary_reporter %in% c("0","false"))
agg$tag_code = ""
agg$tag_narrative = ""

covid_related = subset(
  agg,
  grepl("covid-19", activity_title, ignore.case=T) |
    grepl("covid-19", activity_description, ignore.case=T) |
    grepl("covid-19", transaction_description_narrative, ignore.case=T) |
    humanitarian_scope_narrative == "EP-2020-000012-001" |
    humanitarian_scope_code == "EP-2020-000012-001" |
    humanitarian_scope_narrative == "HCOVD20" |
    humanitarian_scope_code == "HCOVD20" |
    tag_code == "COVID-19" |
    tag_narrative == "COVID-19"
)

covid_related$transaction_date = anydate(covid_related$transaction_date)
covid_related$using_transaction_description = grepl("covid-19", covid_related$transaction_description_narrative, ignore.case=T)
covid_related = subset(covid_related,transaction_date>=as.Date("2020-03-01") | using_transaction_description)

covid_related = subset(covid_related,transaction_type %in% comm)
keep = c("iati_identifier","activity_title","transaction_description_narrative","reporting_org_ref","publisher","humanitarian",
         "transaction_date", "usd_disbursement", "transaction_sector_code",
         "transaction_sector_percentage", "transaction_sector_vocabulary", "transaction_type")
covid_related = covid_related[,keep,with=F]
covid_related$transaction.id = c(1:nrow(covid_related))
names(covid_related) = gsub("_",".",names(covid_related))
original_names = names(covid_related)
covid_related.split = cSplit(covid_related,c("transaction.sector.code", "transaction.sector.percentage", "transaction.sector.vocabulary"),",")
new_names = setdiff(names(covid_related.split),original_names)
covid_related.split.long =reshape(covid_related.split, varying=new_names, direction="long", sep="_")
covid_related.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = transaction.id ]
covid_related.split.long=subset(covid_related.split.long, !is.na(transaction.sector.code) | max_count==1 | count==1)
covid_related.split.long$usd.value=(covid_related.split.long$transaction.sector.percentage/100)*covid_related.split.long$usd.disbursement
covid_related.split.long$usd.value[which(is.na(covid_related.split.long$usd.value))] = covid_related.split.long$usd.disbursement[which(is.na(covid_related.split.long$usd.value))]
covid_related.split.long[,c("usd.disbursement", "max_count", "count", "transaction.id", "id", "time")] = NULL

fwrite(covid_related.split.long,"covid_transaction_by_sector.csv")
