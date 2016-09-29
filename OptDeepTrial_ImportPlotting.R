library(data.table)
library(dplyr)
library(sp)
library(rgdal)

# source import functions
source("DataCubeWQ_functions.R")



# extract data from GA files ----------------------------------------------
setwd("B:/0_scratchProcessing/Bugnot_LandsatWQ")

# import estuary polygon to label points
estuaries = readOGR(dsn="estuary_polygon", layer="australian_estuaries", stringsAsFactors=F)
estuaries = spTransform(estuaries, CRS(" +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# choose folders to loop over
folders = dir("data_trial/", pattern="^trial_*")

# loop over folders and add esuary information as you go
for (i in 1:length(folders)) {
  print(paste0("Processing folder: ",folders[i]))
  temp.points = combine.all.pixels(path=paste0("data_trial/",folders[i],"/"), file.prefix = "LS578_NBAR", 
                                   lat.long.pos = c(6,5)) # make sure this is the correct order
  coords = SpatialPoints(data.frame(temp.points$lon, temp.points$lat))
  temp.spdf = SpatialPointsDataFrame(coords=coords, data=temp.points)
  proj4string(temp.spdf) = CRS(" +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  print("Extracting estuary names...")
  estuary.names = over(temp.spdf, estuaries)
  folder.out = data.frame(temp.points, estuary=estuary.names$EST_NAME, stringsAsFactors=F)
  folder.out$trial = rep(folders[i], nrow(folder.out))
  save(folder.out, file=paste0("data_trial/out_",folders[i],".RData"))
  writeLines("Successful!\n")
}

# clean up
rm(folder.out, estuary.names, temp.points, temp.spdf, coords, estuaries)
gc()

output.files = list.files(path="data_trial/", pattern="trial_*.*RData", full.names=TRUE)
folder.list = list()
for (i in 1:length(folders)) {
  load(output.files[i])
  folder.out = folder.out[folder.out$TSS>0,]
  folder.out = folder.out[!is.na(folder.out$TSS),]
  folder.list[[i]] = folder.out
}
  
# combine into final data frame for plotting
model.data.raw = rbindlist(folder.list)

model.data.filter = 
  model.data.raw %>%
  filter(trial=="trial_AllPoints" & NEAR_INFRARED<500) %>% # NIR filtering for land pixels
  mutate(trial="NIR_filtered")

model.data = rbind(model.data.raw, model.data.filter)

model.data = 
  model.data %>%
  na.omit() %>%
  mutate(date=as.Date(ACQUISITION.DATE), estuary.fac=factor(estuary), trial=factor(trial)) %>%
  mutate(rbratio=BLUE/RED) %>%
  mutate(rbratio=replace(rbratio, which(rbratio<=0), NA)) %>% # special case because some blue values are negative
  select(estuary.fac, date, trial, rgratio, rbratio) %>%
  group_by(estuary.fac, date, trial) %>%
  summarise(mean.rgratio=mean(rgratio, na.rm=T), mean.rbratio=mean(rbratio, na.rm=T))

# clean up
rm(folder.list, folder.out)

# save off
save(model.data, file="data_trial/OptDeepTrial.RData")



# plotting ---------------------------------------------------------------
load("data_trial/OptDeepTrial.RData")
table(model.data$estuary.fac)

# # moving averages
# temp.zoo = zoo(model.data$rgratio, model.data$date)
# mov.av = rollmean(x=temp.zoo, k=10, fill=list(NA,NULL,NA))
# model.data$rgratio.av = coredata(mov.av)
# 
# temp.zoo = zoo(model.data.filter$rgratio, model.data.filter$date)
# mov.av = rollmean(x=temp.zoo, k=10, fill=list(NA,NULL,NA))
# model.data.filter$rgratio.av = coredata(mov.av)

# GAM smoother plots
CairoPDF(file="OptDeepTrial_rgratio_gam", width=20, height=10)
ggplot(model.data, aes(y=mean.rgratio, x=date, colour=trial)) +
  ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  stat_smooth(method=gam, formula=y~s(x, k=29), se=F) +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
dev.off()

CairoPDF(file="OptDeepTrial_rbratio_gam", width=20, height=10)
ggplot(model.data, aes(y=mean.rbratio, x=date, colour=trial)) +
  ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  stat_smooth(method=gam, formula=y~s(x, k=29), se=F) +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
dev.off()

# LM smoother plots
CairoPDF(file="OptDeepTrial_rgratio_lm", width=20, height=10)
ggplot(model.data, aes(y=mean.rgratio, x=date, colour=trial)) +
  ggtitle("Linear term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  stat_smooth(method=lm, formula=y~x, se=F) +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
dev.off()

CairoPDF(file="OptDeepTrial_rbratio_lm", width=20, height=10)
ggplot(model.data, aes(y=mean.rbratio, x=date, colour=trial)) +
  ggtitle("Linear term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  stat_smooth(method=lm, formula=y~x, se=T) +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
dev.off()

# CairoPDF(file="OptDeepTrial_TSS_GAMsmooth", width=20, height=10)
# ggplot(model.data, aes(y=TSS, x=date, colour=trial)) + #geom_point(alpha=0.5) + 
#   stat_smooth(method=gam, formula=y~s(x), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
# dev.off()

# 'less smoothed' plots
for (i in seq(4,60,4)){
  CairoPDF(file=paste0("data_trial/OptDeepTrial_ratio_GAMk",i), width=20, height=10)
  print(
    ggplot(model.data, aes(y=rgratio, x=date, colour=trial)) + #geom_point(alpha=0.5) + 
    stat_smooth(method=gam, formula=y~s(x,k=i), se=T) +
    facet_wrap(facets=~estuary.fac, scales="free_y", nrow=2)
  )
  dev.off()
}