list.of.packages <- c("data.table", "googledrive")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/IATI-Covid/output")

url = paste0(
  'https://iati.cloud/search/activity?q=',
  'reporting_org_ref:(AU-5 BE-BCE_KBO-0264814354 DAC-1601 DE-1 CA-3 XM-DAC-21-1 XM-DAC-6-4 XM-DAC-701-8 XM-DAC-3-1 XM-DAC-7 NO-BRC-971277882 SE-0 CH-4 GB-GOV-1 GB-GOV-10 US-USAGOV US-GOV-1 XI-IATI-EC_DEVCO XI-IATI-EC_ECHO XI-IATI-EC_NEAR)',
  ' AND transaction_date_iso_date:[2020-01-01T00:00:00Z TO *]',
  ' AND (',
    'title_narrative:("gavi" "amc" "iffim")',
    ' OR description_narrative:("gavi" "amc" "iffim")',
    ' OR (transaction_receiver_org_narrative:"gavi")',
    ' OR (participating_org_narrative:"gavi")',
    ' OR transaction_receiver_org_ref:(47122)',
    ' OR participating_org_ref:("47122")',
  ')',
  "&fl=",
  "iati_identifier,reporting_org_*,title*,description*,default_currency,transaction_receiver*,participating_org*,transaction_date*,transaction_type*,transaction_value*,",
  "transaction_provider*,transaction_receiver*",
  '&wt=csv&rows=5000000'
)
activities = read.csv(URLencode(url),as.is=T,na.strings="")

t_url = paste0(
  'https://iati.cloud/search/transaction?q=',
  'transaction_date_iso_date:[2020-01-01T00:00:00Z TO *]',
  ' AND iati_identifier:(',paste0(unique(activities$iati_identifier),collapse=" "),
  ')',
  "&fl=",
  "iati_identifier,reporting_org_*,title*,description*,default_currency,transaction_receiver*,participating_org*,transaction_date*,transaction_type*,transaction_value*,",
  "transaction_provider*,transaction_receiver*",
  '&wt=csv&rows=5000000'
)
transactions = read.csv(URLencode(t_url),as.is=T,na.strings="")

fwrite(transactions, "gavi_transactions_2020.csv")

# t_sheet <- drive_upload(
#   media="gavi_transactions_2020.csv",
#   path=as_id("1urT1sW4k7ngnQeVZfiZ5XDEk-PRtj_Fq"),
#   "GAVI Transactions 2020",
#   type = "spreadsheet"
# )

t_sheet <- drive_update(
  file=as_id("1LeVyfbTsqTeA_9mI08pNhE0HRzn2aj1NLwPn228W8y8s"),
  media="all_transactions_2020.csv"
)
