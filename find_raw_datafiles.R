library(dplyr)

load("AllEstuaries.RData")

model.data = model.data[model.data$estuary=="Hastings River",] # estuary(s) you want the raw files for
table(model.data$estuary)
model.data = data.frame(model.data) # in case there's data.table/dplyr/df mixups

unique.loc = 
  model.data %>%
  select(lon,lat)

unique.loc = unique.loc[!duplicated(unique.loc),]

# copy and paste - have to specify in and out dirs
for (i in 1:nrow(unique.loc)) {
  # (!file.exists(paste0("data/NSW_big2/LS578_NBAR_WITH_PQA_",unique.loc$lon[i],"_",unique.loc$lat[i],"_1987-01-01_2014-12-31.csv"))) print(paste0(i,": File doesn't exist"))
  file.copy(
    from = paste0("data/NSW_big2/LS578_NBAR_WITH_PQA_",unique.loc$lon[i],"_",unique.loc$lat[i],"_1987-01-01_2014-12-31.csv"),
    to = "data/hast/"
  )
}
