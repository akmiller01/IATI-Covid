list.of.packages <- c("data.table","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# commit_2020_url = "https://iati.cloud/search/activity?q=(activity_date_start_actual_f:[2020-01-01T00:00:00Z TO *] OR (-activity_date_start_actual_f:[* TO *]
#               AND activity_date_start_planned_f:[2020-01-01T00:00:00Z TO *])) AND transaction_type:(2)&wt=xslt&tr=activity-csv.xsl&rows=5000000"
# 
# data=read.csv(URLencode(commit_2020_url))
prefix = "transaction_"
data = read.csv("~/git/IATI-Covid/output/ds_test.csv")
transaction_names = names(data)[which(substr(names(data),1,nchar(prefix))==prefix)]
original_names = names(data)
for(transaction_name in transaction_names){
  new_transaction_name = gsub("_", ".", transaction_name)
  setnames(data,transaction_name,new_transaction_name)
  data = cSplit(data,new_transaction_name,",")
  if(ncol(data) == length(original_names)){
    next
  }
  original_names = original_names[which(original_names!=transaction_name)]
}
new_names = setdiff(names(data),original_names)
times = as.numeric(unlist(regmatches(new_names, gregexpr("[[:digit:]]+", new_names))))
timeless_names = gsub("[[:digit:]]+","",new_names)
times.dt = data.table(times,timeless_names)
times.dt[,max:=max(times),by=.(timeless_names)]
times.dt$non.zero.name = paste0(times.dt$timeless_names,times.dt$times)
setnames(data,new_names,times.dt$non.zero.name)
max_count = max(times.dt$max)
unique_timeless_names = unique(times.dt$timeless_names)
all_possible_names = expand.grid(unique_timeless_names,c(1:max_count))
all_possible_names = paste0(all_possible_names$Var1, all_possible_names$Var2)
non_existant_names = setdiff(all_possible_names,names(data))
data[,(non_existant_names):=NA]


data.long=reshape(data, varying=all_possible_names, direction="long", sep="_")