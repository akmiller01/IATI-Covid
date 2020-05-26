list.of.packages <- c("data.table", "anytime", "Hmisc","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

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
using_tab = covid_related[,.(
  using_title = any(using_title),
  using_description = any(using_description),
  using_transaction_description = any(using_description),
  using_glide = any(using_glide),
  using_appeal = any(using_appeal),
  using_tag = any(using_tag)
),by=.(publisher)]
# Which countries is COVID-19 funding going to? (assess number of activities per recipient country)
countries_tab = covid_related[,.(activity_count=length(unique(.SD$iati_identifier))),by=.(recipient_country_codes)]
# From all IATI members, how many are publishing vs not publishing? Which members are publishing vs not publishing?
#   Pick one specific member. Use as an example of publishing good data?
#   Can we track progress over time for publishers? Run a query to show increase of publishers and activities over time.
# Number of activities by reporting organization type
# 
# MORE DETAILED QUESTIONS
# How much money has been allocated to COVID-19 up to now? Track progress over the year. 
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
