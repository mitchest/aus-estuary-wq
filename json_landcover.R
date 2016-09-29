library(jsonlite)

?jsonlite

coverdat <- fromJSON('http://vegcover.com/tsjson?wmsLayer=aus:ground_cover&geoJsonPoly={"type":"FeatureCollection","features":[{"type":"Feature","properties":{},"geometry":{"type":"Polygon","coordinates":[[[146.62628173828125,-30.42972957503159],[146.62628173828125,-30.42972957503159],[146.72515869140625,-30.315987718557853],[146.85699462890625,-30.401306519203573],[146.722412109375,-30.496017831341284],[146.722412109375,-30.496017831341284],[146.62628173828125,-30.42972957503159]]]}}]}&startDate=2014-01&endDate=2016-07&monthlyRainfall=shitYeah',
                     simplifyVector = T, flatten = F)

plot(coverdat$bare[,2] ~ coverdat$bare[,1], type='n', ylim=c(0,100), xlab="date", ylab="Cover Fraction (%)")
lines(coverdat$bare[,2] ~ coverdat$bare[,1], col="red")
lines(coverdat$green[,2] ~ coverdat$green[,1], col="green")
lines(coverdat$nongreen[,2] ~ coverdat$nongreen[,1], col="blue")
lines((coverdat$nongreen[,2]+coverdat$green[,2]) ~ coverdat$green[,1], col="black")

for (idx in 1:10) {#idx <- 2
  print(coverdat$green[,2][idx])
  print(coverdat$green[,1][idx])
  print(coverdat$greenHist[[idx]][[2]])
  print(coverdat$greenHist[[idx]][[1]])
  print(mean(coverdat$greenHist[[idx]][[2]][coverdat$greenHist[[idx]][[2]]>0])
  )
}


sum(coverdat$nongreenHist[[1]][[2]] * (0:100)) / sum(coverdat$nongreenHist[[1]][[2]])
coverdat$nongreen[1,2]

ms_convert <- function(x) {x/(1000 * 1 * 60 * 60 * 24)} # convert milliseconds to days
as.Date(ms_convert(coverdat$green[10,1]), origin = "1970-01-01")
