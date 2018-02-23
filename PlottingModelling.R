library(ggplot2)
#library(gplots)
library(Cairo)
library(mgcv)
library(dplyr)
library(tidyr)

# source functions
source("DataCubeWQ_functions.R")


# load main data set -> run this whole section! ---------------------------

# load data
setwd("B:/0_scratchProcessing/Bugnot_LandsatWQ")

load("AllEstuariesFilterLite.RData")
sort(table(model.data.filter.lite$estuary.fac), decreasing=T)
sum(is.na(model.data.filter.lite$mean.rgratio)); sum(is.na(model.data.filter.lite$mean.rbratio))

# further prep the filtered data for modelling
# subset to minimum smaple size
sum(sort(table(model.data.filter.lite$estuary.fac), decreasing=T)>30)

sample.sizes = table(model.data.filter.lite$estuary.fac)
min.size = names(sample.sizes)[sample.sizes>30]
model.data.filter.lite = model.data.filter.lite[model.data.filter.lite$estuary.fac %in% min.size,]
model.data.filter.lite$estuary.fac = factor(model.data.filter.lite$estuary.fac)


# add time variables for seasonal effects modelling
model.data.filter.lite = model.data.filter.lite %>%
  mutate(month = as.numeric(format(date, format="%m")),
         year = factor(format(date, format="%Y")),
         time = as.numeric(date))
#model.data.filter.lite = model.data.filter.lite[,c("mean.ratio","estuary.fac","month","time","year")]



# NIR filter compare / ratio compare -------------------------------------------------------
load("AllEstuariesLite.RData")
sort(table(model.data.lite$estuary.fac), decreasing=T)
sum(is.na(model.data.lite$mean.rgratio)); sum(is.na(model.data.lite$mean.rbratio))

# test difference between non-filtered data
model.data.filtered = model.data.filter.lite[,names(model.data.lite)]
model.data.lite$filtered = "All_Data"
model.data.filtered$filtered = "filtered"
full.data = rbind(model.data.lite, model.data.filtered)
full.data$filtered = factor(full.data$filtered)

CairoPDF(file="AllEstuaries_rgratio_gam_filter-compare", width=30, height=30)
ggplot(full.data, aes(y=mean.rgratio, x=date, colour=filtered)) + #geom_point(alpha=0.5) + 
  ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
  stat_smooth(method=gam, formula=y~s(x), se=T) +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=25)
dev.off()
gc()

# # trends - just a time smooth - in filtered data
# CairoPDF(file="AllEstuaries_TSS_GAMsmooth", width=30, height=30)
# ggplot(model.data.filter.lite, aes(y=mean.TSS, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=25)
# dev.off()

# # compare ratios
# ratio.data = 
#   model.data.filter.lite %>%
#   select(estuary.fac, date, mean.rgratio, mean.rbratio) %>%
#   gather(key=ratio.bands, value=ratio.value, mean.rgratio:mean.rbratio)
# 
# CairoPDF(file="AllEstuaries_gam_ratio-compare", width=30, height=30)
# ggplot(ratio.data, aes(y=ratio.value, x=date, colour=ratio.bands)) + #geom_point(alpha=0.5) + 
#   ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=25)
# dev.off()
# gc()
# 
# CairoPDF(file="AllEstuaries_lm_ratio-compare", width=30, height=30)
# ggplot(ratio.data, aes(y=ratio.value, x=date, colour=ratio.bands)) + #geom_point(alpha=0.5) + 
#   ggtitle("Linear term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=lm, se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=25)
# dev.off()
# gc()



# case study plots --------------------------------------------------------
# case.studies = c("Port Jackson","Nadgee Lake and Inlet")
# plot.data = model.data.filter.lite[model.data.filter.lite$estuary.fac %in% case.studies,]
# plot.data$estuary.fac = factor(plot.data$estuary.fac)
# plot.data$month = as.integer(format(plot.data$date, "%m"))
# 
# # CairoPDF(file="CaseStudy_TSS_GAMspikey", width=30, height=15)
# # ggplot(plot.data, aes(y=mean.TSS, x=date)) + #geom_point(alpha=0.5) + 
# #   ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
# #   stat_smooth(method=gam, formula=y~s(x, k=120), se=T) +
# #   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# # dev.off()
# # 
# # CairoPDF(file="CaseStudy_TSS_GAMsmooth", width=30, height=15)
# # ggplot(plot.data, aes(y=mean.TSS, x=date)) + #geom_point(alpha=0.5) + 
# #   ggtitle("Smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
# #   stat_smooth(method=gam, formula=y~s(x, k=30), se=T) +
# #   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# # dev.off()
# 
# # rgratio
# CairoPDF(file="CaseStudy_rgratio_GAMspikey", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rgratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Green/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x, k=120), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()
# 
# CairoPDF(file="CaseStudy_rgratio_GAMsmooth", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rgratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Green/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x, k=30), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()
# 
# CairoPDF(file="CaseStudy_rgratio_LMfit", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rgratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Green/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=lm, se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()
# 
# #rbratio
# CairoPDF(file="CaseStudy_rbratio_GAMspikey", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rbratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Blue/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x, k=120), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()
# 
# CairoPDF(file="CaseStudy_rbratio_GAMsmooth", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rbratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Blue/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=gam, formula=y~s(x, k=30), se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()
# 
# CairoPDF(file="CaseStudy_rbratio_LMfit", width=30, height=15)
# ggplot(plot.data, aes(y=mean.rbratio, x=date)) + #geom_point(alpha=0.5) + 
#   ggtitle("Blue/Red - smooth term for time only") + theme(plot.title=element_text(family="Times", face="bold", size=20)) +
#   stat_smooth(method=lm, se=T) +
#   facet_wrap(facets=~estuary.fac, scales="free_y", nrow=1)
# dev.off()



# compare rb and rg ratios ------------------------------------------------
# plot(model.data.filter.lite$mean.rgratio ~ model.data.filter.lite$mean.rbratio)
# boxplot(model.data.filter.lite$mean.rgratio, model.data.filter.lite$mean.rbratio)
# summary(model.data.filter.lite$mean.rgratio)
# summary(model.data.filter.lite$mean.rbratio)
# cor.test(model.data.filter.lite$mean.rgratio, model.data.filter.lite$mean.rbratio)
# summary(lm(model.data.filter.lite$mean.rgratio ~ model.data.filter.lite$mean.rbratio))



# sample distribuion --------------------------------------------------

# sample distribution in months
CairoPDF(file="AllEstuaries_SamplesPerMonth", width=30, height=30)
ggplot(model.data.filter.lite, aes(factor(month))) + geom_bar() + theme_classic() +
  facet_wrap(facets=~estuary.fac, scales="free_y", nrow=25)
dev.off()



# models - linear trends --------------------------------------------------

# # fit LM models
# linear.fits.rg = fit.lms(model.data.filter.lite, "mean.rgratio")
# linear.fits.rb = fit.lms(model.data.filter.lite, "mean.rbratio")
# fit GAM models
gamm.fits.rg = fit.gams(model.data.filter.lite, "mean.rgratio")
gamm.fits.rb = fit.gams(model.data.filter.lite, "mean.rbratio")

##################################
# # get estuary lat/longs
# library(data.table)
# load("AllEstuaries.RData")
# latlongs = data.frame(lat=model.data$lat, lon=model.data$lon, estuary=model.data$estuary)
# latlongs = data.table(latlongs, key="estuary")
# est.locs = latlongs[J(unique(estuary)),mult="first"] # fuck, this is fast!
# rm(model.data,latlongs)
# save(est.locs, file="est.locs.RData")
load("est.locs.RData")
cities = read.csv("CapCities.csv", header=T, stringsAsFactors=F)
##################################

# ## estuary location coef plot - linear term only models
# for (i in c("rg","rb")) {
#   
#   if (i=="rg") linear.fits = linear.fits.rg
#   if (i=="rb") linear.fits = linear.fits.rb
#   
#   # join model results
#   estuary.lm.fits = merge(x=est.locs, y=linear.fits, by="estuary")
#   # make some plotting vars
#   estuary.lm.fits$sig = "(P<0.05)"
#   estuary.lm.fits$sig[estuary.lm.fits$pvals>0.05] = "(P>0.05)"
#   estuary.lm.fits$sig = factor(estuary.lm.fits$sig)
#   estuary.lm.fits$trend = "Decreasing"
#   estuary.lm.fits$trend[estuary.lm.fits$slopes>0] = "Increasing"
#   estuary.lm.fits$trend = factor(estuary.lm.fits$trend)
#   estuary.lm.fits$plotvalues = paste0(estuary.lm.fits$trend,estuary.lm.fits$sig)
#   estuary.lm.fits$plotvalues = factor(estuary.lm.fits$plotvalues)
#   #save(estuary.lm.fits, file="lmfits.latlongs.RData")
#   # plot around Aus
#   #load("lmfits.latlongs.RData")
#   CairoPDF(paste0("AllEstuaries_",i,"ratio_TimeTerm"), width=12, height=7)
#   print(
#     ggplot(data=estuary.lm.fits, aes(x=lon, y=lat)) +
#     geom_point(size=2, aes(colour=plotvalues, shape=plotvalues)) + theme_bw() + 
#     ggtitle(paste0("Landsat water clarity index (",i,"ratio) - single linear term for time")) +
#     scale_colour_manual(name="Trend (95% significance)",
#                         labels=c("Decreasing(P<0.05)","Decreasing(P>0.05)","Increasing(P<0.05)","Increasing(P>0.05)"),
#                         values=c("red","black","green","black")) +
#     scale_shape_manual(name="Trend (95% significance)",
#                        labels=c("Decreasing(P<0.05)","Decreasing(P>0.05)","Increasing(P<0.05)","Increasing(P>0.05)"),
#                        values=c(16,95,16,3)) +
#     geom_text(data=cities, aes(x=lon, y=lat, label=city), nudge_x=-2)
#   )
#   dev.off()
# }

## estuary location coef plot - seasonal term + linear term models
for (i in c("rg","rb")) { 
  
  if (i=="rg") gamm.fits = gamm.fits.rg
  if (i=="rb") gamm.fits = gamm.fits.rb
  
  # join model results
  estuary.gamm.fits = merge(x=est.locs, y=gamm.fits, by="estuary")
  # make some plotting vars
  estuary.gamm.fits$sig = "(P<0.05)"
  estuary.gamm.fits$sig[estuary.gamm.fits$pvals>0.05] = "(P>0.05)"
  estuary.gamm.fits$sig = factor(estuary.gamm.fits$sig)
  estuary.gamm.fits$trend = "Decreasing"
  estuary.gamm.fits$trend[estuary.gamm.fits$slopes>0] = "Increasing"
  estuary.gamm.fits$trend = factor(estuary.gamm.fits$trend)
  estuary.gamm.fits$plotvalues = paste0(estuary.gamm.fits$trend,estuary.gamm.fits$sig)
  estuary.gamm.fits$plotvalues = factor(estuary.gamm.fits$plotvalues)
  estuary.gamm.fits$seasonality = "NotSeasonal"
  estuary.gamm.fits$seasonality[estuary.gamm.fits$s.pvals>0.05] = "Seasonal"
  estuary.gamm.fits$seasonality = factor(estuary.gamm.fits$seasonality)
  #save(estuary.gamm.fits, file="gammfits.latlongs.RData")
  # plot around Aus
  #load("gammfits.latlongs.RData")
  CairoPDF(paste0("AllEstuaries_",i,"ratio_SeasonaRainTimeTerms"), width=12, height=7)
  print(
    ggplot(data=estuary.gamm.fits, aes(x=lon, y=lat)) +
    geom_point(size=2, aes(colour=plotvalues, shape=plotvalues)) + theme_bw() + 
    ggtitle(paste0("Landsat water clarity index (",i,"ratio) - linear term for time, after effects of season and rainfall")) +
    scale_colour_manual(name="Trend (95% significance)",
                        labels=c("Decreasing(P<0.05)","Decreasing(P>0.05)","Increasing(P<0.05)","Increasing(P>0.05)"),
                        values=c("red","black","green","black")) +
    scale_shape_manual(name="Trend (95% significance)",
                       labels=c("Decreasing(P<0.05)","Decreasing(P>0.05)","Increasing(P<0.05)","Increasing(P>0.05)"),
                       values=c(16,95,16,3)) +
    geom_text(data=cities, aes(x=lon, y=lat, label=city), nudge_x=-2)
  )
  dev.off()
  
  # plot seasonal significance
  CairoPDF(paste0("AllEstuaries_",i,"ratio_SeasonalSig"), width=12, height=7)
  print(
    ggplot(data=estuary.gamm.fits, aes(x=lon, y=lat)) +
    geom_point(size=2, aes(colour=seasonality)) + theme_bw() + ggtitle("Seasonal term significance in [Lansdat clarity metric ~ Season + Rain + Time] models") +
    scale_colour_manual(name="95% significance of seasonality",
                        labels=c("No seasonality in clarity metric", "clarity metric is seasonal"),
                        values=c("black","blue")) +
    geom_text(data=cities, aes(x=lon, y=lat, label=city))
  )
  dev.off()
}

# plot whether time coefs have same sign
sign.test = function(X, data) {
  if (is.na(data[X,1]) | is.na(data[X,2])) { return("NoFit") }
  else if (data[X,1]>0 & data[X,2]>0) { return("Positive") }
  else if (data[X,1]<0 & data[X,2]<0) {return("Negative") }
  else { return("Different") }
}

trend.test = unlist(lapply(as.list(1:nrow(gamm.fits.rb)), FUN=sign.test, data=cbind(gamm.fits.rb$slopes, gamm.fits.rg$slopes)))
table(trend.test)
gam.trend.test = data.frame(estuary=gamm.fits.rb$estuary, 
                            trend.test=trend.test)
gam.trend.test = merge(x=est.locs, y=gam.trend.test, by="estuary")

CairoPDF("AllEstuaries_ratio_trend-test", width=12, height=7)
ggplot(data=gam.trend.test, aes(x=lon, y=lat)) +
  geom_point(size=2, aes(colour=trend.test)) + theme_bw() + ggtitle("Similarity in trend between R:G / R:B ratios [Model: Lansdat clarity metric ~ Season + Rain + Time]") +
  scale_colour_manual(name="Sign of 'time' coefficient",
                      labels=c("Different signs", "Both negative", "No Fit possible", "Both Positive"),
                      values=c("yellow","red","black","green")) +
  geom_text(data=cities, aes(x=lon, y=lat, label=city), nudge_x=-2)
dev.off()

# CairoPDF("POSTER/POSTER_ratio_trend-test", width=36, height=21)
# ggplot(data=gam.trend.test, aes(x=lon, y=lat)) +
#   geom_point(size=1.5, aes(colour=trend.test)) + theme_void() + ggtitle("Similarity in trend between R:G / R:B ratios [Model: Lansdat clarity metric ~ Season + Rain + Time]") +
#   scale_colour_manual(name="Sign of 'time' coefficient",
#                       labels=c("Different signs", "Both negative", "No Fit possible", "Both Positive"),
#                       values=c("yellow","red","black","green"))
# dev.off()

# write off coeff data
# write.csv(linear.fits.rb, file="linear.fits.rb.csv", row.names=F)
# write.csv(linear.fits.rg, file="linear.fits.rg.csv", row.names=F)
write.csv(gam.trend.test,file="trend-test_locs.csv", row.names=F)


# ## posthoc estuary covariates coef plots - seasonal term + linear term models
# ## this is done really clunkily, sorry...
# estcovars = read.csv("estuary_covars.csv", header=T)
# estcovars$CONDITION = factor(estcovars$CONDITION, levels=c("extensively modified", "modified", "largely unmodified", "near pristine", "no assessment"))
# estcov.rb = merge(x=gamm.fits.rb, y=estcovars, by="estuary")
# estcov.rg = merge(x=gamm.fits.rg, y=estcovars, by="estuary")
# 
# write.csv(estcov.rb, file="gamfits.covar.rb.csv", row.names=F)
# write.csv(estcov.rg, file="gamfits.covar.rg.csv", row.names=F)
#
# facs = c(7,10:12,39)# sapply(estcov.rg, is.factor) # don't really want to plot everyting...
# nums = c(13:38)
# 
# # plot numerics
# par(oma=c(2,2,2,2), mfrow=c(1,1))
# for (i in 1:length(nums)) {
#   # rb
#   y = as.numeric(estcov.rb$slopes[estcov.rb$pvals>=0.05])
#   x = as.numeric(estcov.rb[estcov.rb$pvals>=0.05,nums[i]])
#   if (sum(!is.na(x))>20) {
#     ft = gam(y~s(x))
#     summ = summary(ft)
#     if (summ$s.pv<=0.05) { # remove if wanting to see all fits
#       plot(ft, xlab=paste0(names(estcov.rb)[nums[i]]), ylab="smooth term", 
#            main=paste0("effect on b/r time coefficient\n(p=",round(summ$s.pv,3),"; dev expl = ",round(summ$dev.expl*100, 2),"%)"))
#     }
#   }
#   # rg
#   y = as.numeric(estcov.rg$slopes[estcov.rg$pvals>=0.05])
#   x = as.numeric(estcov.rg[estcov.rg$pvals>=0.05,nums[i]])
#   if (sum(!is.na(x))>20) {
#     ft = gam(y~s(x))
#     summ = summary(ft)
#     if (summ$s.pv<=0.05) { # remove if wanting to see all fits
#       plot(ft, xlab=paste0(names(estcov.rg)[nums[i]]), ylab="smooth term", 
#            main=paste0("effect on g/r time coefficient\n(p=",round(summ$s.pv,3),"; dev expl = ",round(summ$dev.expl*100, 2),"%)"))
#     }
#   }
# }
# 
# 
# par(oma=c(6,3,1,1), mgp=c(3,1,0), mfrow=c(1,2))
# for (i in 1:length(facs)) {
#   # rb
#   y = as.numeric(estcov.rb$slopes[estcov.rb$pvals>=0.05])
#   x = factor(estcov.rb[estcov.rb$pvals>=0.05,facs[i]])
#   if (sum(!is.na(x))>20) {
#     ft = lm(y~x)
#     summ = summary(ft)
#     anov = anova(ft)
#     boxplot(y~x, xlab="", ylab="", 
#          main=paste0("b/r time coefficient ~ ",names(estcov.rb)[facs[i]],"\n(p=",round(anov$`Pr(>F)`[1],3),"; R^2 = ",round(summ$r.squared, 2),")"),
#          outline=F, las=2)
#     abline(h=0, lty=2)
#   }
#   # rg
#   y = as.numeric(estcov.rg$slopes[estcov.rg$pvals>=0.05])
#   x = factor(estcov.rg[estcov.rg$pvals>=0.05,facs[i]])
#   if (sum(!is.na(x))>20) {
#     ft = lm(y~x)
#     summ = summary(ft)
#     anov = anova(ft)
#     boxplot(y~x, xlab="", ylab="", 
#             main=paste0("g/r time coefficient ~ ",names(estcov.rb)[facs[i]],"\n(p=",round(anov$`Pr(>F)`[1],3),"; R^2 = ",round(summ$r.squared, 2),")"),
#             outline=F, las=2)
#     abline(h=0, lty=2)
#   }
# }
# 
# # plot for poster
# 
# ylim=c(-0.000015,0.000015)
# 
# # numeric
# CairoPDF(file="POSTER/gam-posthoc_num.pdf", width=30, height=15)
# par(oma=c(2,2,2,2), mfrow=c(1,2))
# 
# y = as.numeric(estcov.rg$slopes[estcov.rg$pvals>=0.05])
# x = as.numeric(estcov.rg$MANGROVE[estcov.rg$pvals>=0.05])
# ft = gam(y~s(x))
# summ = summary(ft)
# plot(ft, xlab="Mangroves (Ha)", ylab="smooth term",  rug=F, ylim=ylim, shade=T, 
#      main=paste0("effect on g/r time coefficient\n(p=",round(summ$s.pv,3),"; dev expl = ",round(summ$dev.expl*100, 2),"%)"))
# abline(h=0, lty=1)
# 
# y = as.numeric(estcov.rb$slopes[estcov.rb$pvals>=0.05])
# x = as.numeric(estcov.rb$MEANWAVEHEIGHT[estcov.rb$pvals>=0.05])
# ft = gam(y~s(x))
# summ = summary(ft)
# plot(ft, xlab="Mean wave height (m)", ylab="smooth term",  rug=F, ylim=ylim, shade=T, 
#      main=paste0("effect on b/r time coefficient\n(p=",round(summ$s.pv,3),"; dev expl = ",round(summ$dev.expl*100, 2),"%)"))
# abline(h=0, lty=1)
# 
# dev.off()
# 
# 
# #factor
# CairoPDF(file="POSTER/gam-posthoc_fac.pdf", width=30, height=15)
# par(oma=c(6,3,1,1), mgp=c(3,1,0), mfrow=c(1,2))
# 
# y = as.numeric(estcov.rg$slopes[estcov.rg$pvals>=0.05])
# x = factor(estcov.rg$STATE[estcov.rg$pvals>=0.05])
# ft = lm(y~x)
# summ = summary(ft)
# anov = anova(ft)
# boxplot(y~x, xlab="", ylab="", ylim=ylim,
#         main=paste0("g/r time coefficient ~ STATE\n(p=",round(anov$`Pr(>F)`[1],3),"; R^2 = ",round(summ$r.squared, 2),")"),
#         outline=F, las=2)
# abline(h=0, lty=1)
# 
# y = as.numeric(estcov.rg$slopes[estcov.rg$pvals>=0.05])
# x = factor(estcov.rg$CONDITION[estcov.rg$pvals>=0.05])
# ft = lm(y~x)
# summ = summary(ft)
# anov = anova(ft)
# boxplot(y~x, xlab="", ylab="", ylim=ylim,
#         main=paste0("g/r time coefficient ~ COND\n(p=",round(anov$`Pr(>F)`[1],3),"; R^2 = ",round(summ$r.squared, 2),")"),
#         outline=F, las=2)
# abline(h=0, lty=1)
# 
# dev.off()



# map comparison of models with/without rainfall covariates ---------------

# fit the models
# rg
gam_rg_rain <- fit.gams.rain(model.data.filter.lite, "mean.rgratio")
gam_rg_norain <- fit.gams.norain(model.data.filter.lite, "mean.rgratio")
# rb
gam_rb_rain <- fit.gams.rain(model.data.filter.lite, "mean.rbratio")
gam_rb_norain <- fit.gams.norain(model.data.filter.lite, "mean.rbratio")

# calcualte difference in deviance explained
dev_expl <- data.frame(rg_dev_rain = gam_rg_rain$dev_expl, 
                       rg_dev_norain = gam_rg_norain$dev_expl, 
                       rg_dev_diff = gam_rg_rain$dev_expl - gam_rg_norain$dev_expl,
                       rb_dev_rain = gam_rb_rain$dev_expl, 
                       rb_dev_norain = gam_rb_norain$dev_expl, 
                       rb_dev_diff = gam_rb_rain$dev_expl - gam_rb_norain$dev_expl)
# round
dev_expl <- round(dev_expl, 2)

# join to estuary data
load("est.locs.RData")
dev_expl$estuary <- as.character(gam_rg_rain$estuary)
dev_expl = inner_join(x=est.locs, y=dev_expl, by="estuary")

# make categories for easy plotting
dev_expl$rg_dev_diff_cat <- as.factor(cut(dev_expl$rg_dev_diff, breaks = c(-0.1,0.0,0.05,0.1,0.15,0.2,0.5), 
                                labels = NULL, include.lowest = T))
dev_expl$rb_dev_diff_cat <- as.factor(cut(dev_expl$rb_dev_diff, breaks = c(-0.1,0.0,0.05,0.1,0.15,0.2,0.5), 
                                labels = NULL, include.lowest = T))

# quick check
ggplot(data=dev_expl, aes(x=lon, y=lat)) +
  geom_point(size=2, aes(colour=rg_dev_diff_cat)) + 
  scale_colour_brewer(palette = "YlGnBu") + theme_bw()
ggplot(data=dev_expl, aes(x=lon, y=lat)) +
  geom_point(size=2, aes(colour=rb_dev_diff_cat)) + 
  scale_colour_brewer(palette = "YlGnBu") + theme_bw()

# save off
write.csv(dev_expl, file="dev_expl_rain-no-rain.csv")



# map comparison of models with/without season covariate ---------------

# fit the models
# rg
gam_rg_season <- fit.gams.season(model.data.filter.lite, "mean.rgratio")
gam_rg_noseason <- fit.gams.noseason(model.data.filter.lite, "mean.rgratio")
# rb
gam_rb_season <- fit.gams.season(model.data.filter.lite, "mean.rbratio")
gam_rb_noseason <- fit.gams.noseason(model.data.filter.lite, "mean.rbratio")

# calcualte difference in deviance explained
dev_expl <- data.frame(rg_dev_season = gam_rg_season$dev_expl, 
                       rg_dev_noseason = gam_rg_noseason$dev_expl, 
                       rg_dev_diff = gam_rg_season$dev_expl - gam_rg_noseason$dev_expl,
                       rb_dev_season = gam_rb_season$dev_expl, 
                       rb_dev_noseason = gam_rb_noseason$dev_expl, 
                       rb_dev_diff = gam_rb_season$dev_expl - gam_rb_noseason$dev_expl)
# round
dev_expl <- round(dev_expl, 2)

# join to estuary data
load("est.locs.RData")
dev_expl$estuary <- as.character(gam_rg_season$estuary)
dev_expl = inner_join(x=est.locs, y=dev_expl, by="estuary")

# make categories for easy plotting
dev_expl$rg_dev_diff_cat <- as.factor(cut(dev_expl$rg_dev_diff, breaks = c(-0.3,0.0,0.05,0.1,0.15,0.2,0.5), 
                                          labels = NULL, include.lowest = T))
dev_expl$rb_dev_diff_cat <- as.factor(cut(dev_expl$rb_dev_diff, breaks = c(-0.3,0.0,0.05,0.1,0.15,0.2,0.5), 
                                          labels = NULL, include.lowest = T))

# quick check
ggplot(data=dev_expl, aes(x=lon, y=lat)) +
  geom_point(size=2, aes(colour=rg_dev_diff_cat)) + 
  scale_colour_brewer(palette = "YlGnBu") + theme_bw()
ggplot(data=dev_expl, aes(x=lon, y=lat)) +
  geom_point(size=2, aes(colour=rb_dev_diff_cat)) + 
  scale_colour_brewer(palette = "YlGnBu") + theme_bw()

# save off
write.csv(dev_expl, file="dev_expl_season-no-season.csv")



# detailed GAM analysis --------------------------------------------------

## make some more combinations of daily rain sums instead of daily cummulative
## we can get away with this now since we don't have to worry about estuaries with small sample sizes

library(ggplot2)
#library(gplots)
library(Cairo)
library(mgcv)
library(dplyr)
library(tidyr)

# source functions
source("DataCubeWQ_functions.R")

# get rain data - ########### OR LOAD BELOW #################
# daily.rain.dat =
#   model.data.filter.lite %>%
#   select(rain1:rain30)
# # apply cumulative -> daily function
# daily.rain.dat = data.frame(t(apply(as.matrix(daily.rain.dat), 1, daily.from.cum)))
# names(daily.rain.dat) = paste0("rain",1:30)
# # make model.data again
# model.data.dailyrain =
#   model.data.filter.lite %>%
#   select(-rain1:-rain30)
# model.data.dailyrain = data.frame(model.data.dailyrain, daily.rain.dat)
# rm(daily.rain.dat)
# model.data.dailyrain =
#   model.data.dailyrain %>%
#   rowwise() %>% # this is clunky as hell, the : operator (ie. rain1:rain5) didn't work, but i wanted to find a dplyr way of doing this
#   mutate(rainsum5 = sum(rain1,rain2,rain3,rain4,rain5),
#          rainsum10 = sum(rain6,rain7,rain8,rain9,rain10),
#          rainsum15 = sum(rain11,rain12,rain13,rain14,rain15),
#          rainsum20 = sum(rain16,rain17,rain18,rain19,rain20),
#          rainsum25 = sum(rain21,rain22,rain23,rain24,rain25),
#          rainsum30 = sum(rain26,rain27,rain28,rain29,rain30))
# save(model.data.dailyrain, file="AllEstuaries_dailyrain.RData")

load("AllEstuaries_dailyrain.RData")


# check out some random examples
examples=factor(sample(model.data.dailyrain$estuary.fac, 100))
for (i in examples) {
  plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary=i, which=4)
}
for (i in examples) {
  plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary=i, which=4)
}


# # gam graph plots
# CairoPDF(file="POSTER/gam_tully.pdf", width=20, height=20)
# plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=4)
# dev.off()
# 
# CairoPDF(file="POSTER/tully_rain.pdf", width=10, height=10)
# barplot(c(343.6,468.8,480.2,359.0,238.0,150.5,124.6,100.4,82.6,94.7,120.3,188.7))
# dev.off()
# 
# CairoPDF(file="POSTER/gam_tully_1stD.pdf", width=20, height=20)
# plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=5)
# dev.off()
# 
# CairoPDF(file="POSTER/gam_tully_lin.pdf", width=20, height=20)
# plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=9)
# dev.off()
# 
# 
# # effect of rain on time smooth
# CairoPDF(file="POSTER/gam_raineffect.pdf", width=20, height=20)
# plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Murrah Lagoon", which=4)
# dev.off()
# 
# 
# # rainfall plots
# gam.dat.out = plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Tully River", which=6, gam.sum=T, return.dat=T)
# 
# rain.dat = 
#   gam.dat.out %>%
#   select(rain1:rainsum30) %>%
#   exp()
# 
# CairoPDF(file="POSTER/rainfall.pdf", width=20, height=20)
# par(mfrow=c(2,1), mar=c(4,4,6,4))
# param=list(40,c(0,13000),c(0,3500))
# barplot(daily.from.cum(rain.dat[param[[1]],]), ylim=param[[3]])
# barplot(as.numeric(rain.dat[param[[1]],]), ylim=param[[2]])
# dev.off()

# return yhats (for each component of the linear predictor)
tully_yhats <- plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=4, return.yhat = T)

# return partial effects and test correlation over time
# first for two rivers we expect to be correlated
tully_partials <- plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=4, return.partial = T)
hull_partials <- plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Hull River", which=4, return.partial = T)
time_partial_cor(tully_partials, hull_partials)
# now for two rivers we expect to be quite different
derwent_partials <- plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Derwent River", which=4, return.partial = T)
time_partial_cor(tully_partials, derwent_partials, plot = F)
pjackson_partials <- plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Port Jackson", which=4, return.partial = T)
time_partial_cor(tully_partials, pjackson_partials, plot = T)

# in-situ / RS analysis titbits
load("IS.RS.rain.data.RData")
IS.RSonly <- IS.RS.rain.data[IS.RS.rain.data$data.source=="RS",]
RSonly <- IS.RS.rain.data[IS.RS.rain.data$data.source=="IS",]

# check data
summary(IS.RSonly$RB); which(IS.RSonly$RB <= 0)
summary(IS.RSonly$RG)

summary(IS.RSonly$RED)
summary(IS.RSonly$GREEN)
summary(IS.RSonly$BLUE); which(IS.RSonly$BLUE <= 0)
summary(IS.RSonly$NEAR_INFRARED)

# check n for each IS measurement
for (i in unique(ISonly$EstuaryName)) {
  message(paste0("Estuary: ", i))
  temp_dat <- ISonly[ISonly$EstuaryName==i,c("Turbidity","Secchi","Chl","TSS","Carot","Lat_GDA94","Long_GDA94")]
  cat(paste0(" - Turbidity(", sum(!is.na(temp_dat$Turbidity)),") "))
  cat(paste0(" - Secchi(", sum(!is.na(temp_dat$Secchi)),") "))
  cat(paste0(" - Chl(", sum(!is.na(temp_dat$Chl)),") "))
  cat(paste0(" - TSS(", sum(!is.na(temp_dat$TSS)),") "))
  cat(paste0(" - Carot(", sum(!is.na(temp_dat$Carot)),") "))
  if (sum(!is.na(temp_dat$Lat_GDA94))==0) {
    message(paste0("No lat/lon data for ",i))
  } else {
    print(plot(temp_dat$Lat_GDA94 ~ temp_dat$Long_GDA94, main = i))
  }
}

# check for appropriate date ranges
ggplot(data = ISonly, aes(x = date)) +
  geom_histogram() +
  facet_wrap(~estuary.fac)

# check individual metrics etc.
for (i in c("Turbidity","Secchi","Chl","TSS","Carot")) {
  print(
    ggplot(data = ISonly, aes_string(x = "date", y = i)) +
    geom_line() +
    facet_wrap(~estuary.fac)
  )
}



# compare various linear and smooth terms 

# Tully River
par(mfcol=c(3,2))
for (i in 7:12) {
  plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Tully River", which=i, gam.sum = T)
}
par(mfcol=c(3,2))
for (i in 7:12) {
  plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=i)
}

# Mainwaring River
par(mfcol=c(3,2))
for (i in 7:12) {
  plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Mainwaring River", which=i)
}
par(mfcol=c(3,2))
for (i in 7:12) {
  plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Mainwaring River", which=i)
}


# then peel them all off **still to do
setwd("B:/0_scratchProcessing/Bugnot_LandsatWQ/detailed-gam-plots/")

for (estuary in unique(model.data.dailyrain$estuary.fac)) {
  print(estuary)
  CairoPNG(filename = paste0(estuary,".png"), width = 1000, height = 700)
  par(mfrow=c(2,2), mar=c(5,5,2,2), oma=c(0,0,2,0))
  try(plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary=estuary, which=14), silent = T)
  try(plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary=estuary, which=14), silent = T)
  dev.off()
}


# multi-panel figure 

par(mfcol=c(3,2), mar=c(5,5,2,2), oma=c(0,0,2,0))
# Tully r:g
plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River - red:blue ratio", which=12)
title("Tully River")
title("Partial effect on water clarity ratio", outer = T)
plot.detailed.gam(model.data.dailyrain, ratio="rb", estuary="Tully River", which=13)
barplot(c(343.6,468.8,480.2,359.0,238.0,150.5,124.6,100.4,82.6,94.7,120.3,188.7), 
        names.arg = seq(1,12,1), space = 0, ylab=("Rainfall (mm)"), xlab=("Mean monthly rainfall"))
# Tully r:b (or another estuary)
plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Tully River", which=12)
title("Tully River")
title("Partial effect on water clarity ratio", outer = T)
plot.detailed.gam(model.data.dailyrain, ratio="rg", estuary="Tully River - red:green tatio", which=13)
barplot(c(343.6,468.8,480.2,359.0,238.0,150.5,124.6,100.4,82.6,94.7,120.3,188.7), 
        names.arg = seq(1,12,1), space = 0, ylab=("Rainfall (mm)"), xlab=("Mean monthly rainfall"))




