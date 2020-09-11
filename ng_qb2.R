list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

recipient_country = "NG"

# 1. Activities filtered by activity-level recipient country

url = paste0(
  "https://iatidatastore.iatistandard.org/search/activity?q=recipient_country_code:(",
  recipient_country,
  ")&wt=xslt&tr=activity-csv.xsl&rows=1000000"
)
afa_filename = paste0(recipient_country,"_activity_filtered_activities.csv")

if(!file.exists(afa_filename)){
  data_file = download.file(
    url,
    destfile=afa_filename
  )
}

afa <- read.table(
  afa_filename,
  header=T,
  sep=",",
  quote=c("\""),
  na.strings="",
  stringsAsFactors=FALSE,
  flush=T
)

# 2. Split by transaction