####modelling/plotting functions --------------------------------------------
  
  ## somtimes correlations structure might be needed - hard to tell without checking every model
  ##  - could do it and choose via AIC?
  ##  - few tests suggest not a big deal in our dataset, since the obs are weeks/months apart
  
  ## get rid of redundant rain covariates (i.e. if no rain in last 30 days, they're all zero)
  ## polynomial terms for rain (i.e. some rain could decrease clarity, lots could clear it up??)

log.plusk = function(X) {
  as.numeric(log(X+0.0001))
}

# list of models to fir on each estuary
gam.functions = list(
  
  fit.gam1 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 +
               time, # the thing we're interested in
             data=dat
    )
    return(fm)
  },
  
  fit.gam2 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 + rain3 +
               time,
             data=dat
    )
    return(fm)
  },
  
  fit.gam3 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 + rain3 + rain5 +
               time,
             data=dat
    )
    return(fm)
  },
  
  fit.gam4 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 + rain3 + rain5 + rain10 +
               time,
             data=dat
    )
    return(fm)
  },
  
  fit.gam5 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 + rain3 + rain5 + rain10 + rain15 +
               time,
             data=dat
    )
    return(fm)
  },
  
  fit.gam6 = function(dat) {
    k = length(unique(dat$month))
    fm = gam(formula=mean.ratio ~ 
               s(month, bs="cc", k=k) + 
               rain1 + rain3 + rain5 + rain10 + rain15 + rain25 +
               time,
             data=dat
    )
    return(fm)
  }
  
)  

# function to fit models
fit.gam.functions = function(X, gam.functions, dat) {
  fit.fn = gam.functions[[X]]
  fm.out = try(fit.fn(dat),silent=T); if (inherits(fm.out, "try-error")) fm.out = NULL
  return(fm.out)
}


## models to test for significant linear trend
fit.lms = function(data, yvar) {
  unique.estuaries = unique(data$estuary.fac)
  slopes = numeric(length(unique.estuaries))
  pvals = numeric(length(unique.estuaries))
  for (i in 1:length(unique.estuaries)) {
    if (round(i/50)==(i/50)) {print(paste0(i,": ",unique.estuaries[i]))}
    dat = data[data$estuary.fac==unique.estuaries[i],]
    dat$mean.ratio = dat[,yvar]
    fm = lm(formula=log(mean.ratio) ~ time, data=dat)
    slopes[i] = coef(fm)[2]
    pvals[i] = anova(fm)$Pr[1]
  }
  return(data.frame(slopes=slopes, pvals=pvals, estuary=unique.estuaries))
}

## models to test for significant linear trend after seasonality and covariates
# fit models and get info from them
fit.gams = function(data, yvar) {
  unique.estuaries = unique(data$estuary.fac)
  slopes = numeric(length(unique.estuaries))
  ci.up = numeric(length(unique.estuaries))
  ci.low = numeric(length(unique.estuaries))
  pvals = numeric(length(unique.estuaries))
  s.pvals = numeric(length(unique.estuaries))
  
  for (i in 1:length(unique.estuaries)) {
    if (round(i/50)==(i/50)) {print(paste0(i,": ",unique.estuaries[i]))}
    dat = 
      data %>%
      filter(estuary.fac==unique.estuaries[i]) %>%
      #mutate(mean.ratio = log(mean.rgratio)) %>%
      mutate_each(funs(log.plusk), rain1:rain30)
    dat$mean.ratio = log(dat[,yvar])
    fms.out.list = lapply(as.list(1:length(gam.functions)), FUN=fit.gam.functions, 
                          gam.functions=gam.functions, dat=dat) ## could exit here and keep all models for further diagnostics
    fms.out.list = fms.out.list[!unlist(lapply(fms.out.list, is.null))]
    # get best model
    if (length(fms.out.list)==0) {
      slopes[i] = NA; pvals[i] = NA; s.pvals[i] = NA
      next()
    }
    best.idx = which.min(lapply(fms.out.list, AIC))
    if (round(i/50)==(i/50)) {print(paste0("Best model is ",names(gam.functions[best.idx])))}
    fm = fms.out.list[[best.idx]]
    # fill in the blanks
    fm.summary = summary(fm)
    slopes[i] = fm.summary$p.coeff["time"]
    ci.up[i] = slopes[i] + (2*fm.summary$se["time"])
    ci.low[i] = slopes[i] - (2*fm.summary$se["time"])
    pvals[i] = fm.summary$p.pv["time"]
    s.pvals[i] = fm.summary$s.pv # works since there's only one smooth term
  }
  
  return(data.frame(slopes=slopes, ci.up=ci.up, ci.low=ci.low, 
                    pvals=pvals, s.pvals=s.pvals, estuary=unique.estuaries))
}

## Functions for derivatives of GAM(M) models
Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  newDF <- data.frame(newD) ## needs to be a data frame for predict
  X0 <- predict(mod, newDF, type = "lpmatrix")
  newDF <- newDF + eps
  X1 <- predict(mod, newDF, type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  lD$eval <- newD - eps
  lD ##return
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { ## how many attempts to get this right!?!?
    ##term <- match(term, term.labs)
    ##term <- term[match(term, term.labs)]
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else {
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(miss))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")
    names(xlab) <- xlab
  }
  if (missing(main)) {
    main <- term
    names(main) <- term
  }
  ## compute confidence interval
  CI <- confint(x, term = term)
  ## plots
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  for(i in term) {
    upr <- CI[[i]]$upper
    lwr <- CI[[i]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,i], x[[i]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,i], rev(x$eval[,i])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,i], upr, lty = "dashed")
      lines(x$eval[,i], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
      S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,i], S$incr, lwd = lwd, col = "blue")
      lines(x$eval[,i], S$decr, lwd = lwd, col = "red")
    } else {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}

#output from plot.detailed.gam(return.partial=T)
time_partial_cor <- function(par1, par2, plot = F) {
  # extract where time covariate overlaps
  par1dat <- data.frame(time = par1[[2]][["x"]],
                        fit = par1[[2]][["fit"]])
  par2dat <- data.frame(time = par2[[2]][["x"]],
                        fit = par2[[2]][["fit"]])
  fitted1 <- par1dat %>%
    mutate(time = floor(time)) %>%
    group_by(time) %>%
    summarise(fitted1 = mean(fit))
  fitted2 <- par2dat %>%
    mutate(time = floor(time)) %>%
    group_by(time) %>%
    summarise(fitted2 = mean(fit))
  
  joined_fits <- inner_join(x = fitted1, y = fitted2, by = "time")
  
  # test correlation
  if (plot == T) { plot(joined_fits$fitted1, joined_fits$fitted2) }
  #PearsonR <- cor(par.eff1, par.eff2, method = "pearson")
  SpearmanR <- cor(joined_fits$fitted1, joined_fits$fitted2, method = "spearman")
  #Corr <- as.data.frame(list(Pearson=PearsonR,Spearman=SpearmanR))
  return(SpearmanR)
}

plot.detailed.gam = function(data, var="Green:Red", estuary="Port Jackson", which=4, return.yhat=F, return.partial = F, gam.sum=F, return.dat=F) {
  
  # there is the option of using gamm() here - i.e. penalised regression as a mixed model (using MAS:::glmmPQL and nlme:::lme)
  # the penalty is part of the variance component and is estimated, so no need for an explicit penalty coef, apparently
  # i am not toally sure i understand that though, so a gcv (with mgcv:::gam) routine to pick the best penalty term is more tractable for me

  if (all(return.yhat, return.partial)) {stop("Only one of return.yhat= or return.partial= can be TRUE")}
  if (return.dat == T & any(return.yhat, return.partial)) {stop("Only one of return.dat=, return.yhat= or return.partial= can be TRUE")}
  
  if (var=="GreenRed") {
    dat = 
      data[data$data.source=="RS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log(mean.rgratio)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  
  if (var=="BlueRed") {
    dat = 
      data[data$data.source=="RS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log(mean.rbratio)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  if (var=="Turbidity") {
    dat = 
      data[data$data.source=="IS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log.plusk(Turbidity)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  if (var=="Secchi") {
    dat = 
      data[data$data.source=="IS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log(Secchi)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  if (var=="Chl") {
    dat = 
      data[data$data.source=="IS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log(Chl)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  if (var=="TSS") {
    dat = 
      data[data$data.source=="IS",] %>%
      filter(estuary.fac==estuary) %>%
      mutate(mean.var = log(TSS)) %>%
      mutate_each(funs(log.plusk), rain1:rainsum30)
  }
  #print(estuary)
  #print(paste0("There are ",nrow(dat)," observations for this estuary"))
  
  ft.time = gam(formula=mean.var ~ s(time), data=dat)
  
  ft.SeasonTime = gam(formula=mean.var ~ s(month, bs="cc", k=12) + s(time), data=dat)
  
  ft.SeasonTimeRain = gam(formula=mean.var ~ s(month, bs="cc", k=12) + 
                            rain1 + rain3 + rain5 + rain7 + rain9 + rain11 + rain13 + rain15 + rain17 + rain19 + 
                            rain21 + rain23 + rain25 + rain27 + rain29 +
                            s(time),
                          data=dat)
  
  ft.SeasonTimeRain.lin = gam(formula=mean.var ~ s(month, bs="cc", k=12) + 
                                rain1 + rain3 + rain5 + rain7 + rain9 + rain11 + rain13 + rain15 + rain17 + rain19 + 
                                rain21 + rain23 + rain25 + rain27 + rain29 +
                                time,
                              data=dat)
  
  if (gam.sum==T) {print(summary(ft.SeasonTimeRain))}
  
  
  if (which==5 | which==6) {
    # 1st derivitive and significance
    ft.SeasonTimeRain.d = Deriv(ft.SeasonTimeRain, n=nrow(dat))
    
    # create varaibles to plot derivitive significance on original smooth term
    pdat = arrange(dat, time)  
    p2 = predict(ft.SeasonTimeRain, newdata=pdat, type="terms", se.fit = TRUE)
    pdat$p2 = p2[[1]][,"s(time)"]; pdat$se2 = p2[[2]][,"s(time)"]
    df.res = df.residual(ft.SeasonTimeRain)
    crit.t = qt(0.025, df.res, lower.tail = FALSE)
    pdat$upper = pdat$p2 + (crit.t * pdat$se2); pdat$lower = pdat$p2 - (crit.t * pdat$se2)
    
    
    ft.SeasonTimeRain.dci = confint(ft.SeasonTimeRain.d, term="time")
    ft.SeasonTimeRain.dsig = signifD(pdat$p2, 
                                     d=ft.SeasonTimeRain.d[["time"]]$deriv, 
                                     upper=ft.SeasonTimeRain.dci[["time"]]$upper, 
                                     lower=ft.SeasonTimeRain.dci[["time"]]$lower)
  }
  
  if (which==1) {
    par(mfrow=c(1,1))
    plot(ft.time, n=1000, rug=F)
  }
  
  if (which==2) {
    par(mfrow=c(1,2))
    plot(ft.SeasonTime, n=1000, rug=F)
  }
  
  if (which==3) {
    par(mfrow=c(1,2))
    # check for autocorrelation
    acf(resid(ft.SeasonTime), lag.max = 50, main = "ACF")
    pacf(resid(ft.SeasonTime), lag.max = 50, main = "pACF")
    # i wouldn't actually expect it in this type of data, but these plots and AIC suggest it's not needed
    # the smoothing terms don't really change shape either, so probably not worth the computation
    # ft.SeasonTime.cor = gam(formula=log(mean.var) ~ s(month, bs="cc", k=12) + s(time), 
    #                           data=dat, correlation = corARMA(form = ~ 1|year, p = 2)) # try with 1/2/3
    # plot(ft.SeasonTime.cor)
    # acf(resid(ft.SeasonTime.cor), lag.max = 50, main = "ACF")
    # pacf(resid(ft.SeasonTime.cor), lag.max = 50, main = "pACF")
    # anova(ft.SeasonTime,ft.SeasonTime.cor)
  }
  
  if (which==4) {
    par(mfrow=c(2,2))
    plot(ft.SeasonTime, n=1000, rug=F, main=paste0(estuary," (",var,"; n=",nrow(dat),")"), shade=T, select=1)
    abline(h=0, lty=1)
    plot(ft.SeasonTime, n=1000, rug=F, main=paste0(estuary," (",var,"; n=",nrow(dat),")"), shade=T, select=2)
    abline(h=0, lty=1)
    partial_dat <- plot(ft.SeasonTimeRain, n=100000, rug=F, shade=T, select=1)
    abline(h=0, lty=1)
    plot(ft.SeasonTimeRain, n=1000, rug=F, shade=T, select=2)
    abline(h=0, lty=1)
    
    # reset par
    par(mfrow=c(1,1))
    
    if (return.partial == T) { return(partial_dat) }
    
    if (return.yhat == T) {
      #yhat_SeasonTime <- predict.gam(ft.SeasonTime, type = "terms")
      yhat_SeasonTimeRain <- predict.gam(ft.SeasonTimeRain, type = "terms")
      #yhat_df <- data.frame(yhat_SeasonTime, yhat_SeasonTimeRain)
      yhat_df <- data.frame(yhat_SeasonTimeRain)
      return(yhat_df)
    }
  }
 
  
  if (which==5) {
    par(mfrow=c(1,1))
    plot.Deriv(ft.SeasonTimeRain.d, sizer=T, alpha=0.05, term="time")
    abline(h=0, lty=3)
  }
  
  if (which==6) {
    # plot significant first derivs onto smooth term
    ylim = with(pdat, range(upper, lower, p2))
    ylab = paste0("",var,"; n=",nrow(dat),"")
    plot(p2 ~ date, data = pdat, type = "n", ylab = ylab, ylim = ylim)
    lines(p2 ~ date, data = pdat)
    lines(upper ~ date, data = pdat, lty = "dashed")
    lines(lower ~ date, data = pdat, lty = "dashed")
    lines(unlist(ft.SeasonTimeRain.dsig$incr) ~ date, data = pdat, col = "blue", lwd = 3)
    lines(unlist(ft.SeasonTimeRain.dsig$decr) ~ date, data = pdat, col = "red", lwd = 3)
  }
  
  if (which==7) {
    termplot(ft.SeasonTimeRain.lin, terms="time", se=TRUE,# ylim=c(-0.21,0.22),
             data=dat, col.term="black", col.se="black", lty.se=1,
             main=paste0(estuary," (",var,"; n=",nrow(dat),";r.sq=",round(summary(ft.SeasonTimeRain.lin)$r.sq,4),";p=",round(summary(ft.SeasonTimeRain.lin)$pTerms.pv,2),")"))
    abline(h=0, lty=1)
    
    coef <- summary(ft.SeasonTimeRain.lin)$p.table
    write.csv(file=paste0("Data/LinCoef-",estuary,"-",var,".csv"),as.data.frame(coef))
  }
  
  if (which==8) {
    
    ylab = paste0("",var,"; Dev expl = ",round(as.numeric(summary(ft.SeasonTimeRain)[14])*100,1),"")
    partial_dat <- plot(ft.SeasonTimeRain, n=100000, ylab=ylab, rug=F, shade=T, select=2, xlab="Year", xaxt='n')
    abline(h=0, lty=1)
    
    if (return.partial == T) { return(partial_dat) }
    
    if (return.yhat == T) {
      #yhat_SeasonTime <- predict.gam(ft.SeasonTime, type = "terms")
      yhat_SeasonTimeRain <- predict.gam(ft.SeasonTimeRain, type = "terms")
      #yhat_df <- data.frame(yhat_SeasonTime, yhat_SeasonTimeRain)
      yhat_df <- data.frame(yhat_SeasonTimeRain)
      #return(yhat_df)
    }
  }
  
  if (return.dat==T) {return(dat)}
}


