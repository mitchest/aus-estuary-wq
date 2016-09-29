library(rgdal)
library(raster)

model.data.filter.lite = get(load("IS.RS.data.RData"))

# get array ID and subset data
job = Sys.getenv("PBS_ARRAYID")
job = as.numeric(job)

datrows = round(seq(1, nrow(model.data.filter.lite)+1, length.out=21))

model.data.filter.lite = model.data.filter.lite[datrows[job]:(datrows[job+1]-1),]

# load catchment data
catchments = readOGR("catchments/catchments.shp", layer="catchments")

# create LUT so that rain is only extracted for catchmetns that are in both estuary and catchment vectors
unq.estuaries = as.character(unique(model.data.filter.lite$estuary.fac))
unq.catchments = as.character(unique(catchments@data$EST_NAME))
ext.estuaries = unq.estuaries[unq.estuaries %in% unq.catchments]

# specify rain data location (or put it in PBS script instead)
file.locs = "/srv/scratch/z3476264/rainfall/"

# function for summing rain 
get.lag.sums = function(X, daily.sums){
  sum(daily.sums[1:X])
}

# function to extract rain metrics
## X = the row iterator
## file.locs = full path to file locations including trailing "/"
## landsat.date = date of landsat acquisition
## date.lag = days to calculate sums over (e.g. rainfall in last c(1,3,5) days)
## estuary = estuary to calculate metrics for
## catchments shapefile with estuary name in the first column/variable
extract.rain.metrics = function(X, data, file.locs, date.lag=seq(1,30,1), catchments) {
  if(substr(file.locs,nchar(file.locs),nchar(file.locs))!="/") stop("Add trailing / to file location")
  landsat.date = data$date[X]
  estuary = as.character(data$estuary.fac[X])
  if (estuary %in% ext.estuaries) {
    # stack layers corresponding to date lag
    dates = seq.Date(landsat.date,landsat.date-max(date.lag),by="-1 days")
    files = paste0(file.locs,"r",as.character(format(dates,"%Y%m%d")),".txt")
    rain.stack = raster:::.quickStack(files)
    # crop/mask to estuary
    catchment.poly = catchments[catchments[[1]]==estuary,]
    rain.stack.values = extract(rain.stack, extent(catchment.poly))
    # calculate lag metrics
    lag.sums = unlist(lapply(as.list(date.lag), FUN=get.lag.sums, daily.sums=colSums(rain.stack.values)))
  } else {
    lag.sums = as.numeric(rep(NA, length(date.lag)))
  }
  names(lag.sums) = paste0("rain",date.lag)
  return(lag.sums)
}

# extract metrics and save object
rain.metrics = lapply(as.list(1:nrow(model.data.filter.lite)),
                      FUN=extract.rain.metrics, 
                      data=model.data.filter.lite,
                      file.locs=file.locs,
                      date.lag=seq(1,30,1),
                      catchments=catchments)

save(rain.metrics, file=paste0("out",job,".RData"))
