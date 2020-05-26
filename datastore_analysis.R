list.of.packages <- c("data.table","reshape2","splitstackshape","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Text search ####
search_terms = c(
  # "covid",
  # "corona",
  # "coronavirus",
  "covid-19"
)

search_fields = c(
  "title_narrative",
  "description_narrative",
  "iati_identifier"
)

search_query = ""
search_grid = expand.grid(search_fields, search_terms, stringsAsFactors = F)
for(i in 1:nrow(search_grid)){
  row = search_grid[i,]
  query = paste0(row[1], ':"', row[2], '"')
  if(i != nrow(search_grid)){
    query = paste0(query, " OR ")
  }
  search_query = paste0(search_query, query)
}

text_search_url = paste0(
  'https://iati.cloud/search/activity?q=(',
  search_query,
  ')&wt=xslt&tr=activity-csv.xsl&rows=5000000' 
)

text_search_activities = read.csv(URLencode(text_search_url),as.is=T)

# Glide search ####
glide_code = "\"EP-2020-000012-001\""
glide_url = paste0(
  'https://iati.cloud/search/activity?q=(',
  'humanitarian_scope_narrative:', glide_code, ' OR ',
  'humanitarian_scope_code:', glide_code,
  ')&wt=xslt&tr=activity-csv.xsl&rows=5000000' 
)

glide_activities = read.csv(URLencode(glide_url),as.is=T)

# Tag search ####
tag_code = "\"EP-2020-000012-001\""
tag_url = paste0(
  'https://iati.cloud/search/activity?q=(',
  'tag_narrative:', tag_code, ' OR ',
  'tag_code:', tag_code,
  ')&wt=xslt&tr=activity-csv.xsl&rows=5000000' 
)

tag_activities = read.csv(URLencode(tag_url),as.is=T)

# Appeal search ####
appeal_code = "\"HCOVD20\""
appeal_url = paste0(
  'https://iati.cloud/search/activity?q=(',
  'humanitarian_scope_code:', appeal_code,
  ')&wt=xslt&tr=activity-csv.xsl&rows=5000000' 
)

appeal_activities = read.csv(URLencode(appeal_url),as.is=T)

# Combine ####
all_activities = appeal_activities
all_activities = rbind(
  all_activities,
  glide_activities[which(!(glide_activities$iati_identifier %in% unique(all_activities$iati_identifier))),]
)
all_activities = rbind(
  all_activities,
  text_search_activities[which(!(text_search_activities$iati_identifier %in% unique(all_activities$iati_identifier))),]
)


# Transactions ####
names(all_activities) = gsub("_",".",names(all_activities))
original_names = names(all_activities)
transaction_names = original_names[which(substr(original_names,1,12)=="transaction.")]
all_activities.split = cSplit(all_activities,transaction_names,",")
new_names = setdiff(names(all_activities.split),original_names)
all_activities.split.long =reshape(all_activities.split, varying=new_names, direction="long", sep="_")
all_activities.split.long[ , `:=`( any.data = any(!is.na(.SD[,transaction_names,with=F])) ) , by = iati.identifier ]
all_activities.split.long=subset(all_activities.split.long, any.data==T)
all_activities.split.long$usd.value=(all_activities.split.long$transaction.sector.percentage/100)*all_activities.split.long$usd.disbursement
all_activities.split.long$usd.value[which(is.na(all_activities.split.long$usd.value))] = all_activities.split.long$usd.disbursement[which(is.na(all_activities.split.long$usd.value))]
all_activities.split.long[,c("any.data", "id", "time")] = NULL

all_transactions = all_activities.split.long
all_transactions$transaction.value.currency = as.character(all_transactions$transaction.value.currency)
all_transactions$transaction.value.currency[which(is.na(all_transactions$transaction.value.currency))] = all_transactions$default.currency[which(is.na(all_transactions$transaction.value.currency))]
all_transactions$transaction.year = substr(all_transactions$transaction.value.date,1,4)
all_transactions$transaction.year[which(is.na(all_transactions$transaction.year))] = substr(all_transactions$activity.date.iso.date,1,4)[which(is.na(all_transactions$transaction.year))]

exchange_rates = fread("/home/alex/git/IATI-annual-report-2020/ex_rates.csv")
setnames(exchange_rates,"cc","transaction.value.currency")
setnames(exchange_rates,"year","transaction.year")
all_transactions$transaction.year = as.numeric(all_transactions$transaction.year)
all_transactions = merge(all_transactions,exchange_rates,all=T)
all_transactions$transaction.value.usd = all_transactions$transaction.value * all_transactions$ex.rate
comm = c("C", "2")
disb = c("E", "D", "3", "4")
sum(subset(all_transactions,transaction.type.code %in% comm)$transaction.value.usd,na.rm=T)
sum(subset(all_transactions,transaction.type.code %in% disb)$transaction.value.usd,na.rm=T)
# BASIC QUESTIONS
# Which IATI publishers are currently publishing COVID-19 related activities/transactions? Which elements are they using?
#   Free text - activity title
# Free text - activity description
# Free text - transaction description
# Hum scope codes- GLIDE and HRP
# Tag element for development activities- vocab 99 with code= COVID-19
# Which countries is COVID-19 funding going to? (assess number of activities per recipient country)
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
