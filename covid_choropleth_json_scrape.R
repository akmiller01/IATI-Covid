library(jsonlite)
library(data.table)

covid_json = fromJSON("/home/alex/Downloads/covid_choropleth.json",simplifyVector=F)

covid.list = list()
covid.index = 1

for(i in 1:length(covid_json)){
  row = covid_json[[i]]
  country_name = row$name[[1]]
  ifr = row$value[[1]]
  metadata = row$metadata
  share_with_comorbidity = metadata[[1]]
  average_age = metadata[[2]]
  health_system_mult = metadata[[3]]
  tmp.df = data.frame(
    country_name,
    share_with_comorbidity,
    average_age,
    health_system_mult,
    ifr
  )
  covid.list[[covid.index]] = tmp.df
  covid.index = covid.index + 1
}

covid.df = rbindlist(covid.list)