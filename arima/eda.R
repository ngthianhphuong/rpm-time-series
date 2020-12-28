library(readxl)
library(latex2exp)
training_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=3)
val_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=1)

training_set = training_set[c("Date", "Site", "RPM")]
val_set = val_set[c("Date", "Site", "RPM")]

View(training_set)

#install.packages("xts")
library("xts")


nlag = 36 # nombre de lag maxi pour les plots
sais = 7 # saiso hebdo

## modèle additif
sites = c("business insider", "ca m'intéresse", "capital", "ce soir tv",
          "cuisine actuelle", "femme actuelle", "gala", "geo", "neon",
          "télé 2 semaines", "télé loisirs", "voici")

for (site in sites)
  {
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  acf = acf(subset_ts, lag.max=nlag, plot=FALSE, ylim=c(-1,1))
  acf$lag = acf$lag 
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=TeX(site))
    
}

# série différenciée y_dif_1: sortie ACF et PACF

for (site in c("business insider", "ca m'intéresse", "capital", "femme actuelle", "gala", "neon", "télé loisirs"))
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  y_dif_1 = diff(subset_ts, lag=1, differences=1)
  
  
  acf = acf(y_dif_1, lag.max=nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  acf$lag = acf$lag
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=paste(site, ": y_dif_1"))
  
  pacf = pacf(y_dif_1, lag.max = nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  pacf$lag = pacf$lag
  plot(pacf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="h", ylab=TeX("PACF"),
       main=paste(site, ": y_dif_1"))
  
}

# 2e différenciation y_dif_1_7 pour CAC, CTV, GEO, T2S, TEL

for (site in c("cuisine actuelle", "ce soir tv", "geo", "télé 2 semaines", "voici"))

{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  y_dif_1 = diff(subset_ts, lag=1, differences=1) # retard 1, différencier une seule fois
  y_dif_1_7 = diff(y_dif_1, lag=7, differences=1) # retard 7, différencier une seule fois
  
  acf = acf(y_dif_1_7, lag.max=nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  acf$lag = acf$lag
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=paste(site, ": y_dif_1_7"))
  
  pacf = pacf(y_dif_1_7, lag.max = nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  pacf$lag = pacf$lag
  plot(pacf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="h", ylab=TeX("PACF"),
       main=paste(site, ": y_dif_1_7"))
  
}





## modèle multiplicative
for (site in sites)
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  subset_ts_log = log(subset_ts)
  acf = acf(subset_ts_log, lag.max=nlag, plot=FALSE, ylim=c(-1,1))
  acf$lag = acf$lag 
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=TeX(paste(c("log(", site, ")"), collapse="")))
  
}

# série différenciée y_dif_1: sortie ACF et PACF

for (site in c("business insider", "ca m'intéresse", "capital",
               "femme actuelle", "gala", "geo", "neon",
               "télé loisirs", "voici"))
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  subset_ts_log = log(subset_ts)
  y_dif_1 = diff(subset_ts_log, lag=1, differences=1)
  
  acf = acf(y_dif_1, lag.max=nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  acf$lag = acf$lag
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=paste(site, ": y_dif_1"))
  
  
  pacf = pacf(y_dif_1, lag.max = nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  pacf$lag = pacf$lag
  plot(pacf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab=TeX("PACF"),
       main=paste(site, ": y_dif_1"))
  
}

# # 2e différenciation y_dif_1_7 pour CAC, CTV, T2S

for (site in c("cuisine actuelle", "ce soir tv", "télé 2 semaines"))
  
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  subset_ts_log = log(subset_ts)
  y_dif_1 = diff(subset_ts_log, lag=1, differences=1) # retard 1, différencier une seule fois
  y_dif_1_7 = diff(y_dif_1, lag=7, differences=1) # retard 7, différencier une seule fois
  
  acf = acf(y_dif_1_7, lag.max=nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  acf$lag = acf$lag
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=paste(site, ": y_dif_1_7"))
  
  pacf = pacf(y_dif_1_7, lag.max = nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  pacf$lag = pacf$lag
  plot(pacf, ci=0.95, xlim=c(2, nlag-1), ylim=c(-1, 1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab=TeX("PACF"),
       main=paste(site, ": y_dif_1_7"))
}

