list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

url = "https://iatidatastore.iatistandard.org/search/activity?q=recipient_country_code:(NG)&wt=xslt&tr=activity-csv.xsl&rows=100000"

results = c()
for(i in 1:10){
  download.file(
    url,
    destfile="ng_test.csv"
  )
  
  test <- read.table(
    "ng_test.csv",
    header=T,
    sep=",",
    quote=c("\""),
    na.strings="",
    stringsAsFactors=FALSE,
    flush=T
  )
  
  results = c(results,length(unique(test$iati_identifier)))
  Sys.sleep(10)
}

