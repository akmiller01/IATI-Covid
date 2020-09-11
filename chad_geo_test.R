list.of.packages <- c("data.table", "anytime", "dplyr", "reshape2","splitstackshape","stringr", "httr","XML","jsonlite","sp", "rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

# admin = fread("~/Downloads/chad_admin.csv")
# admin = subset(admin,location_administrative_code!="")
# names(admin) = gsub("_",".",names(admin))
# original_names = names(admin)
# admin.split = cSplit(admin,c("location.administrative.code", "location.administrative.level", "location.administrative.vocabulary"),",")
# new_names = setdiff(names(admin.split),original_names)
# admin.split.long = data.frame(reshape(admin.split, varying=new_names, direction="long", sep="_"))
# admin.split.long[c("time","id")] = NULL
# admin.split.long = subset(admin.split.long,!is.na(location.administrative.code))
# 
# geonames = subset(admin.split.long,location.administrative.vocabulary=="G1")
# osm = subset(admin.split.long,location.administrative.vocabulary=="G2")
# iso = subset(admin.split.long,location.administrative.vocabulary=="A4")
# 
# geonames$country = NA
# geonames$name = NA
# 
# unique_geonames = unique(subset(geonames,is.na(country))$location.administrative.code)
# 
# for(unique_geoname in unique_geonames){
#   geonames_id = as.integer(unique_geoname)
#   geonames_url = paste0("https://www.geonames.org/getJSON?style=gui&id=",geonames_id)
#   if(GET(geonames_url)$status==200){
#     geonames_contents = fromJSON(geonames_url)
#     name = geonames_contents$asciiName
#     country = geonames_contents$countryCode
#     geonames$name[which(geonames$location.administrative.code==unique_geoname)] = name
#     geonames$country[which(geonames$location.administrative.code==unique_geoname)] = country
#     Sys.sleep(5)
#   }
# }
# 
# # save(geonames,file="geonames.RData")
# td_geonames = subset(geonames,country=="TD")
# fwrite(td_geonames,"td_geonames.csv")

pos = fread("~/Downloads/chad_pos.csv")
pos = subset(pos,location_point_pos!="")
pos$location_point_pos = gsub("\\,"," ",pos$location_point_pos,fixed=T)
names(pos) = gsub("_",".",names(pos))
original_names = names(pos)
pos.split = cSplit(pos,c("location.name.narrative","location.point.pos", "location.exactness.code"),",")
new_names = setdiff(names(pos.split),original_names)
pos.split.long = data.frame(reshape(pos.split, varying=new_names, direction="long", sep="_"))
pos.split.long[c("time","id")] = NULL
pos.split.long = subset(pos.split.long,!is.na(location.point.pos))
pos.split.long$location.point.pos = gsub("(","",pos.split.long$location.point.pos,fixed=T)
pos.split.long$location.point.pos = gsub(")","",pos.split.long$location.point.pos,fixed=T)
coords = cSplit(pos.split.long, "location.point.pos", " ")
coordinates(coords)=~location.point.pos_1+location.point.pos_2

td = readOGR("/home/alex/Downloads/tcd_adm1/tcd_admbnda_adm1_ocha.shp")
proj4string(coords) = proj4string(td)
over_dat = over(coords,td)
coords$adm1 = over_dat$admin1Name

td_coords = subset(coords,!is.na(adm1))
fwrite(data.frame(td_coords),"td_coords.csv")
