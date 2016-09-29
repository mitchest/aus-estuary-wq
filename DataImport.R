library(data.table)
library(dplyr)
library(sp)
library(rgdal)
library(raster)

# source import functions
source("DataCubeWQ_functions.R")



# extract data from GA files ----------------------------------------------
setwd("B:/0_scratchProcessing/Bugnot_LandsatWQ")

# import estuary polygon to label points
estuaries = readOGR(dsn="estuary_polygon", layer="australian_estuaries", stringsAsFactors=F)
estuaries = spTransform(estuaries, CRS(" +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# choose folders to loop over
folders = dir("data/")

# loop over folders and add esuary information as you go
for (i in 1:length(folders)) {
  print(paste0("Processing folder: ",folders[i]))
  temp.points = combine.all.pixels(path=paste0("data/",folders[i],"/"), file.prefix = "LS578_NBAR", 
                                   lat.long.pos = c(6,5)) # make sure this is the correct order
  coords = SpatialPoints(data.frame(temp.points$lon, temp.points$lat))
  temp.spdf = SpatialPointsDataFrame(coords=coords, data=temp.points)
  proj4string(temp.spdf) = CRS(" +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  print("Extracting estuary names...")
  estuary.names = over(temp.spdf, estuaries)
  folder.out = data.frame(temp.points, estuary=estuary.names$EST_NAME, stringsAsFactors=F)
  save(folder.out, file=paste0(folders[i],"_out.RData"))
  writeLines("Successful!\n")
}
# clean up
rm(folder.out, estuary.names, temp.points, temp.spdf, coords, estuaries)
gc()

# load and combine data, then clean up
output.files = as.list(list.files(path="./", pattern="*_out.RData", full.names=TRUE))

get.files = function(X) {
  get(load(X))
}
model.data = rbindlist(lapply(output.files, get.files))

model.data = 
  model.data %>%
  #na.omit() %>%
  filter(TSS>0) %>%
  mutate(date=as.Date(ACQUISITION.DATE), estuary.fac=factor(estuary))

# save off
save(model.data, file="AllEstuaries.RData")



# remove land pixels ------------------------------------------------------
load("AllEstuaries.RData")

detach(package:raster) # fucks with dplry select

# hist(model.data$NEAR_INFRARED, breaks=100)
# abline(v=500, col="red")
# mean(model.data$NEAR_INFRARED, na.rm = T)
# sd(model.data$NEAR_INFRARED, na.rm = T)

# sum(model.data$NEAR_INFRARED < 500, na.rm = T)


############## -> maybe do it per estuary?
############## -> loop through, create threshold for each estuary, then loop through all rows, conditionally filtering

model.data.filter = 
  model.data %>%
  filter(as.numeric(NEAR_INFRARED) < 500)



# cut down lite versions --------------------------------------------------

# cut down the data size a little; remove un-needed variables
model.data.lite = 
  model.data %>%
  mutate(rbratio=as.numeric(BLUE)/as.numeric(RED)) %>%
  select(estuary.fac, date, rgratio, rbratio)

model.data.filter.lite = 
  model.data.filter %>%
  mutate(rbratio=as.numeric(BLUE)/as.numeric(RED)) %>%
  select(estuary.fac, date, rgratio, rbratio)
  


# estuary means -----------------------------------------------------------
# all data
model.data.lite = 
  model.data.lite %>%
  na.omit() %>%
  mutate(rbratio=replace(rbratio, which(rbratio<=0), NA)) %>% # special case because some blue values are negative
  group_by(estuary.fac, date) %>%
  summarise(mean.rgratio=mean(rgratio, na.rm=T), mean.rbratio=mean(rbratio, na.rm=T))
save(model.data.lite, file="AllEstuariesLite.RData")

# filtered data
model.data.filter.lite = 
  model.data.filter.lite %>%
  na.omit() %>%
  mutate(rbratio=replace(rbratio, which(rbratio<=0), NA)) %>% # special case because some blue values are negative
  group_by(estuary.fac, date) %>%
  summarise(mean.rgratio=mean(rgratio, na.rm=T), mean.rbratio=mean(rbratio, na.rm=T))
save(model.data.filter.lite, file="AllEstuariesFilterLite_means.RData")


# import rain data --------------------------------------------------------

#### --> should funbcitonalise this, but too lazy at the moment

## Full Landsat ratio data set
# load katana processed rain extractions
test.out = get(load("rainfall/metrics/out1.RData"))
nlags = length(test.out[[1]])
lagnames = names(test.out[[1]])
lagnames = paste0("rain",1:30) # in case you want to manually name lag columns
# how many jobs was it split into?
jobs = 199
# function to load data - check directory is right
load.rdata = function(X) {
  data.frame(matrix(unlist(get(load(paste0("rainfall/metrics/out",X,".RData")))),ncol=nlags,byrow=T))
}
# load all the chunks and combine, check it matches original data
rain.metrics = rbindlist(lapply(as.list(1:jobs), load.rdata))
setnames(rain.metrics, names(rain.metrics), lagnames)
if (nrow(rain.metrics)==nrow(model.data.filter.lite)) print("Imported data length matches") else print("Imported data do not match - check all chunks imported")

# append rain metrics to model data, final clean
model.data.filter.lite = data.frame(model.data.filter.lite, rain.metrics)
rm(rain.metrics)

save(model.data.filter.lite, file="AllEstuariesFilterLite.RData")


## In-situ data sites w/ coincident Landsat pixels
load("IS.RS.data.RData")

test.out = get(load("rainfall/metrics/out1.RData"))
nlags = length(test.out[[1]])
lagnames = paste0("rain",1:30) # in case you want to manually name lag columns
lagnames = names(test.out[[1]])
# how many jobs was it split into?
jobs = 20
# function to load data - check directory is right
load.rdata = function(X) {
  data.frame(matrix(unlist(get(load(paste0("rainfall/metrics/out",X,".RData")))),ncol=nlags,byrow=T))
}
# load all the chunks and combine, check it matches original data
rain.metrics = rbindlist(lapply(as.list(1:jobs), load.rdata))
setnames(rain.metrics, names(rain.metrics), lagnames)
if (nrow(rain.metrics)==nrow(IS.RS.data)) print("Imported data length matches") else print("Imported data do not match - check all chunks imported")

# append rain metrics to model data, final clean
IS.RS.rain.data = data.frame(IS.RS.data, rain.metrics)
rm(rain.metrics)

save(IS.RS.rain.data, file="IS.RS.rain.data.RData")



# import fractional cover data --------------------------------------------

# # get codes for SA2 -> FID
# yearly.fc = read.csv("fractional_cover/yearly_fc.csv", header=T, stringsAsFactors=F)
# yearly.fc = yearly.fc[!is.na(yearly.fc$SA2),c("FID","SA2")]
# fid.sa2 = yearly.fc %>%
#   group_by(FID) %>%
#   summarise(SA2x=mean(SA2, na.rm=T))
# 
# # get monthly FC for each FID, then translate to SA2
# # load data
# load.fc = function(X) {
#   read.csv(paste0("fractional_cover/FID_",X,"_result.csv"))
# }
# fc.all = rbindlist(lapply(as.list(fid.sa2$FID), load.fc))
# # get monthly fc
# fc.monthly = fc.all %>%
#   mutate(date=as.Date(Timestamp, format="%Y-%m-%d %H:%M:%S")) %>%
#   mutate(year=format(date,"%Y"), month=format(date,"%m")) %>%
#   select(FID,Band,Mean,year,month) %>%
#   na.omit() %>%
#   group_by(FID,year,month,Band) %>%
#   summarise(meanfc=mean(Mean, na.rm=T))
# # add SA2 codes
# fid.to.sa2 = function(X) {
#   fid.sa2$SA2x[fid.sa2$FID==fc.monthly$FID[X]]
# }
# fc.monthly$SA2 = unlist(lapply(as.list(1:nrow(fc.monthly)), fid.to.sa2))
# 
# # join fc data to model data
# join.fc = function(X) {
#   
# }


  




















