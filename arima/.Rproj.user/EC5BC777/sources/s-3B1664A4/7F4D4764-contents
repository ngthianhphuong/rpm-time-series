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

# série différenciée
for (site in sites)
{
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_train$Date = as.Date(subset_train$Date, "%Y - %m - %d")
  subset_ts = xts(subset_train$RPM, subset_train$Date)
  y_dif_1 = diff(subset_ts, lag=1, differences=1)
  acf = acf(y_dif_1, lag.max=nlag, plot=FALSE, ylim=c(-1,1), na.action = na.pass)
  acf$lag = acf$lag
  plot(acf, ci=0.05, xlim=c(2, nlag-1), ylim=c(-1,1), xaxp=c(1, nlag, nlag-1),
       type="h", lend="butt", lwd=5, xlab="t", ylab="ACF",
       main=paste(site, ": Série différenciée"))
  
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

# série différenciée

for (site in sites)
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
       main=paste(site, ": série différenciée"))
  
}


