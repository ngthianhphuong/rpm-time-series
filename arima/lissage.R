library(readxl)
library(latex2exp)
training_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=3)
val_set <- read_excel("~/MSc Data Management/Thèse professionnelle/ORION_04_Site.xlsx", sheet=1)

training_set = training_set[c("Date", "Site", "RPM")]
val_set = val_set[c("Date", "Site", "RPM")]



gala_train = subset(training_set, Site=="gala", select=c("Date", "RPM"))
gala_ts = ts(data = gala_train$RPM, frequency=7)

decomp_gala = decompose(gala_ts, type="additive")
plot(decomp_gala)

geo_train = subset(training_set, Site=="geo", select=c("Date", "RPM"))
geo_ts = ts(data = geo_train$RPM, frequency=7)

decomp_geo = decompose(geo_ts, type="multiplicative")
plot(decomp_geo)



library(forecast)

# first letter: M # second: M or A # third: M or A
# MAA
# MAM
# MMM

hw_maa = ets(gala_ts, model = "MAA")
# checkresiduals(hw_maa)
hw_maa.pred = predict(hw_maa, 31)
plot(hw_maa.pred)
#summary(hw_maa)
#AIC(hw_maa)
#BIC(hw_maa)

hw_mam = ets(gala_ts, model = "MAM")
hw_mam.pred = predict(hw_mam, 31)
plot(hw_mam.pred)

hw_mmm = ets(gala_ts, model = "MMA")
hw_mmm.pred = predict(hw_mmm, 31)
plot(hw_mmm.pred)


# calculate RMSE 
gala_val = subset(val_set, Site=="gala", select=c("Date", "RPM"))
gala_val_ts = ts(data = gala_val$RPM)

accuracy(hw_maa.pred, gala_val_ts)
accuracy(hw_mam.pred, gala_val_ts)
accuracy(hw_mmm.pred, gala_val_ts)
gala_rmse = c("gala", accuracy(hw_maa.pred, gala_val_ts)[2, 2], accuracy(hw_mam.pred, gala_val_ts)[2, 2],
              accuracy(hw_mmm.pred, gala_val_ts)[2, 2])
gala_rmse


# create dataframe that store accuracy

rmse_df = data.frame(site=character(),
                     maa=double(),
                     mam=double(),
                     mmm=double(),
                     stringsAsFactors = F)

aic_df = data.frame(site=character(),
                     maa=double(),
                     mam=double(),
                     mmm=double(),
                     stringsAsFactors = F)

bic_df = data.frame(site=character(),
                     maa=double(),
                     mam=double(),
                     mmm=double(),
                     stringsAsFactors = F)

sites = c("business insider", "ca m'intéresse", "capital", "ce soir tv",
          "cuisine actuelle", "femme actuelle", "gala", "geo", "neon",
          "télé 2 semaines", "télé loisirs", "voici")

for (site in sites)
  {
  subset_train = subset(training_set, Site==site, select=c("Date", "RPM"))
  subset_ts = ts(data = subset_train$RPM, frequency=7)
  
  hw_maa = ets(subset_ts, model = "MAA")
  hw_maa.pred = predict(hw_maa, 31)
  #plot(hw_maa.pred)
  
  hw_mam = ets(subset_ts, model = "MAM")
  hw_mam.pred = predict(hw_mam, 31)
  #plot(hw_mam.pred)
  
  hw_mmm = ets(subset_ts, model = "MMM")
  hw_mmm.pred = predict(hw_mmm, 31)
  #plot(hw_mmm.pred)
  
  # calculate RMSE 
  site_val = subset(val_set, Site==site, select=c("Date", "RPM"))
  site_val_ts = ts(data = site_val$RPM)
  
  site_aic = c(site, AIC(hw_maa), AIC(hw_mam), AIC(hw_mmm))
  aic_df[nrow(aic_df)+1,] = site_aic
  
  site_bic = c(site, BIC(hw_maa), BIC(hw_mam), BIC(hw_mmm))
  bic_df[nrow(bic_df)+1,] = site_bic
  
  site_rmse = c(site, accuracy(hw_maa.pred, site_val_ts)[2, 2], accuracy(hw_mam.pred, site_val_ts)[2, 2],
                accuracy(hw_mmm.pred, site_val_ts)[2, 2])
  rmse_df[nrow(rmse_df)+1,] = site_rmse

}

write.csv(aic_df, "lissage_aic.csv")
write.csv(bic_df, "lissage_bic.csv")
write.csv(rmse_df, "lissage_rmse.csv")
