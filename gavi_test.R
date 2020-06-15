list.of.packages <- c("data.table", "googledrive")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/IATI-Covid/output")

url = paste0(
  'https://iati.cloud/search/activity?q=',
  '(title_narrative:"gavi") OR ',
  '(title_narrative:"vaccine") OR ',
  '(title_narrative:"amc") OR ',
  '(title_narrative:"iffim") OR ',
  '(description_narrative:"gavi") OR ',
  '(description_narrative:"vaccine") OR ',
  '(description_narrative:"amc") OR ',
  '(description_narrative:"iffim") OR ',
  '(transaction_receiver_org_narrative:"gavi") OR ',
  '(participating_org_narrative:"gavi") OR ',
  'transaction_receiver_org_ref:(47122) OR participating_org_ref:("47122")',
  "&fl=",
  "iati_identifier,reporting_org_*,title*,description*,transaction_receiver*,participating_org*",
  '&wt=csv&rows=5000000'
)
activities = read.csv(URLencode(url),as.is=T,na.strings="")
activities = subset(activities,reporting_org_ref!="47122")
fwrite(activities, "gavi_test.csv")

# gavi_sheet <- drive_upload(
#   media="gavi_test.csv",
#   path=as_id("1urT1sW4k7ngnQeVZfiZ5XDEk-PRtj_Fq"),
#   "GAVI Filter",
#   type = "spreadsheet"
# )

gavi_sheet <- drive_update(
  file=as_id("1pzq3zY7So2mjMl8a-F2X_i1ZLfPftUOqXa_mlUNbBYs"),
  media="gavi_test.csv"
)
