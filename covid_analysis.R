list.of.packages <- c("data.table", "anytime", "Hmisc","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

comm = c("C", "2")
disb = c("E", "D", "3", "4")
incom = c("11")

agg <- fread("iati_unfiltered_agg.csv",na.strings="")
agg = subset(agg, secondary_reporter %in% c("0","false"))

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

# BASIC QUESTIONS
# Which IATI publishers are currently publishing COVID-19 related activities/transactions? Which elements are they using?
covid_publishers = unique(covid_related$publisher)
covid_publishers = covid_publishers[order(covid_publishers)]
covid_publishers
#   Free text - activity title
# Free text - activity description
# Free text - transaction description
# Hum scope codes- GLIDE and HRP
# Tag element for development activities- vocab 99 with code= COVID-19
covid_related$using_title = grepl("covid-19", covid_related$activity_title, ignore.case=T)
covid_related$using_description = grepl("covid-19", covid_related$activity_description, ignore.case=T)
covid_related$using_transaction_description = grepl("covid-19", covid_related$transaction_description_narrative, ignore.case=T)
covid_related$using_glide = covid_related$humanitarian_scope_narrative == "EP-2020-000012-001" | covid_related$humanitarian_scope_code == "EP-2020-000012-001"
covid_related$using_appeal = covid_related$humanitarian_scope_narrative == "HCOVD20" | covid_related$humanitarian_scope_code == "HCOVD20"
covid_related$using_tag = covid_related$tag_code == "COVID-19" | covid_related$tag_narrative == "COVID-19"

covid_related$using_title[which(is.na(covid_related$using_title))] = FALSE
covid_related$using_description[which(is.na(covid_related$using_description))] = FALSE
covid_related$using_transaction_description[which(is.na(covid_related$using_transaction_description))] = FALSE
covid_related$using_glide[which(is.na(covid_related$using_glide))] = FALSE
covid_related$using_appeal[which(is.na(covid_related$using_appeal))] = FALSE
covid_related$using_tag[which(is.na(covid_related$using_tag))] = FALSE

using_tab = covid_related[,.(
  using_title = any(using_title),
  using_description = any(using_description),
  using_transaction_description = any(using_description),
  using_glide = any(using_glide),
  using_appeal = any(using_appeal),
  using_tag = any(using_tag)
),by=.(publisher)]
using_tab$using_any = T
fwrite(using_tab,"using_tab.csv")
# Which countries is COVID-19 funding going to? (assess number of activities per recipient country)
countries_tab = covid_related[,.(activity_count=length(unique(.SD$iati_identifier))),by=.(recipient_country_codes)]
fwrite(countries_tab,"countries_tab.csv",na="")
# From all IATI members, how many are publishing vs not publishing? Which members are publishing vs not publishing?
members = fread("../members.csv")
members = subset(members,publisher!="")
members = members[,c("Name","publisher")]
setnames(members,"Name","publisher_name")
members = merge(members,using_tab,all.x=T)
members$using_title[which(is.na(members$using_title))] = FALSE
members$using_description[which(is.na(members$using_description))] = FALSE
members$using_transaction_description[which(is.na(members$using_transaction_description))] = FALSE
members$using_glide[which(is.na(members$using_glide))] = FALSE
members$using_appeal[which(is.na(members$using_appeal))] = FALSE
members$using_tag[which(is.na(members$using_tag))] = FALSE
members$using_any[which(is.na(members$using_any))] = FALSE
fwrite(members,"members_using.csv",na="")

#   Pick one specific member. Use as an example of publishing good data?
#   Can we track progress over time for publishers? Run a query to show increase of publishers and activities over time.
# Number of activities by reporting organization type
org_type_tab = covid_related[,.(activity_count=length(unique(.SD$iati_identifier))),by=.(reporting_org_type)]
org_types = fread("../OrganisationType.csv")
org_types = org_types[,c("code","name")]
names(org_types) = c("reporting_org_type","reporting_org_type_name")
org_type_tab$reporting_org_type = as.numeric(org_type_tab$reporting_org_type)
org_type_tab = merge(org_type_tab,org_types)
fwrite(org_type_tab,"org_type_tab.csv")
# 
# MORE DETAILED QUESTIONS
# How much money has been allocated to COVID-19 up to now? Track progress over the year.
covid_related_trans = covid_related
covid_related_trans$transaction_date = anydate(covid_related_trans$transaction_date)
covid_related_trans$using_transaction_description = grepl("covid-19", covid_related_trans$transaction_description_narrative, ignore.case=T)
covid_related_trans = subset(covid_related_trans,transaction_date>=as.Date("2020-03-01") | using_transaction_description)
sum(subset(covid_related_trans, transaction_type %in% disb)$usd_disbursement,na.rm=T)
sum(subset(covid_related_trans, transaction_type %in% comm)$usd_disbursement,na.rm=T)
# Money allocated to COVID-19 = the money in transactions that are identified as COVID-19-related (can’t be assume that all transactions within an activity are COVID-19-related)
# Could assess total commitments/disbursements in COVID-19-related activities and transactions vs total commitments/disbursement for the transactions we can definitively say are COVID-19-related (because the specific transactions are identified as COVID-19-related)
# Which activities and who is involved? What are the implementing agencies and how many activities are being implemented by each? What are the implementing agencies by recipient country?
#   Which sectors are COVID-19-related activities/transactions targeting?
#   Qualitative analysis comparing the activities!
#   Try to assess which activities appear to be 100% COVID-19-related and which are clearly not (e.g. UNHCR activities)
# What transaction types have been used? 
#   Do COVID-19-related activities activities already have disbursements?
#   Are the COVID-19-related activities newly reported/published activities or were they previously reported and now the COVID-19-related values have been added? Or can we get a breakdown of each of these? 
#   Are there old activities which are now being included as having COVID-19-specifc values but had been published previously?
#   Who are the key receiver orgs? Run the check by identifying covid-19 transactions and the searching for provider and receiver-org.
