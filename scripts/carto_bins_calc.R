

library(geoR)



bio_results <- "/home/userz/Desktop/predictive_grid/interpolation_data/biodiversity_cartodb.csv"
bio <- read.csv(bio_results)

bio_coords <- as.data.frame(bio[c("latitude", "longitude")])
bio_coords <- jitterDupCoords(bio_coords, max=0.001)
bio$latitude <- bio_coords$latitude
bio$longtiude <- bio_coords$longitude

names(bio)[names(bio) == 'val'] <- 'biodiversity_val'
names(bio)[names(bio) == 'std_err'] <- 'biodiversity_std_err'
names(bio)[names(bio) == 'std_err_val'] <- 'biodiversity_std_err_val'
bio$biodiversity_treatment <- bio$coef_TrtBin + bio$coef_TrtBin.sector_group

bio$biodiversity_val_ref <- 0
bio[which(bio$biodiversity_val > 0),]$biodiversity_val_ref <- bio[which(bio$biodiversity_val > 0),]$biodiversity_val / abs(max(bio[which(bio$biodiversity_val > 0),]$biodiversity_val))
bio[which(bio$biodiversity_val < 0),]$biodiversity_val_ref <- bio[which(bio$biodiversity_val < 0),]$biodiversity_val / abs(min(bio[which(bio$biodiversity_val < 0),]$biodiversity_val))

bio$biodiversity_trt_ref <- 0
bio[which(bio$biodiversity_treatment > 0),]$biodiversity_trt_ref <- bio[which(bio$biodiversity_treatment > 0),]$biodiversity_treatment / abs(max(bio[which(bio$biodiversity_treatment > 0),]$biodiversity_treatment))
bio[which(bio$biodiversity_treatment < 0),]$biodiversity_trt_ref <- bio[which(bio$biodiversity_treatment < 0),]$biodiversity_treatment / abs(min(bio[which(bio$biodiversity_treatment < 0),]$biodiversity_treatment))

write.table(bio, '/home/userz/Desktop/biodiversity_cartodb_jitter.csv', sep=",", row.names=FALSE, quote=TRUE)




# bio value positive
bvp <-bio$biodiversity_val_ref[bio$biodiversity_val_ref > 0]

bvpm <- mean(bvp)
bvps <- sd(bvp)

quantile(bvp[btp < bvpm+bvps], 0.75)
quantile(bvp[btp < bvpm+bvps], 0.5)
quantile(bvp[btp < bvpm+bvps], 0.25)

# mean(bvp[bvp < bvpm+bvps]) + sd(bvp[bvp < btpm+bvps]) * 0.25 
# mean(bvp[bvp < bvpm+bvps]) - sd(bvp[bvp < btpm+bvps]) * 0.25 
# mean(bvp[bvp < bvpm+bvps]) - sd(bvp[bvp < bvpm+bvps]) * 0.5


# bio value negative
bvn <-abs(bio$biodiversity_val_ref[bio$biodiversity_val_ref < 0])

bvnm <- mean(bvn)
bvns <- sd(bvn)

quantile(bvn[bvn < bvnm+bvns], 0.75)
quantile(bvn[bvn < bvnm+bvns], 0.5)
quantile(bvn[bvn < bvnm+bvns], 0.25)

# mean(bvn[bvn > bvnm-bvns]) + sd(bvn[bvn > bvnm-bvns]) * 0.5 
# mean(bvn[bvn > bvnm-bvns]) + sd(bvn[bvn > bvnm-bvns]) * 0.25 
# mean(bvn[bvn > bvnm-bvns]) - sd(bvn[bvn > bvnm-bvns]) * 0.25 






# bio treatment positive
btp <-bio$biodiversity_trt_ref[bio$biodiversity_trt_ref > 0]
    
btpm <- mean(btp)
btps <- sd(btp)

quantile(btp[btp < btpm+btps], 0.75)
quantile(btp[btp < btpm+btps], 0.5)
quantile(btp[btp < btpm+btps], 0.25)

# mean(btp[btp < btpm+btps]) + sd(btp[btp < btpm+btps]) * 0.5
# mean(btp[btp < btpm+btps]) - sd(btp[btp < btpm+btps]) * 0.5 
# mean(btp[btp < btpm+btps]) - sd(btp[btp < btpm+btps]) 


# bio treatment negative
btn <-abs(bio$biodiversity_trt_ref[bio$biodiversity_trt_ref < 0])

btnm <- mean(btn)
btns <- sd(btn)

quantile(btn[btn < btnm+btns], 0.75)
quantile(btn[btn < btnm+btns], 0.5)
quantile(btn[btn < btnm+btns], 0.25)

# mean(btn[btn < btnm+btns]) + sd(btn[btn > btnm+btns]) * 0.25 
# mean(btn[btn < btnm+btns]) - sd(btn[btn > btnm+btns]) * 0.125 
# mean(btn[btn < btnm+btns]) - sd(btn[btn > btnm+btns]) * 0.125 
# 



# -------------------------------------------------



carbon_results <- "/home/userz/Desktop/predictive_grid/interpolation_data/carbon_cartodb.csv"
carb <- read.csv(carbon_results)

carb_coords <- as.data.frame(carb[c("latitude", "longitude")])
carb_coords <-jitterDupCoords(carb_coords, max=0.001)
carb$latitude <- carb_coords$latitude
carb$longtiude <- carb_coords$longitude



names(carb)[names(carb) == 'val'] <- 'carbon_val'
names(carb)[names(carb) == 'std_err'] <- 'carbon_std_err'
names(carb)[names(carb) == 'std_err_val'] <- 'carbon_std_err_val'
carb$carbon_treatment <- carb$coef_TrtBin + carb$coef_TrtBin.sector_group

carb$carbon_val_ref <- 0
carb[which(carb$carbon_val > 0),]$carbon_val_ref <- carb[which(carb$carbon_val > 0),]$carbon_val / abs(max(carb[which(carb$carbon_val > 0),]$carbon_val))
carb[which(carb$carbon_val < 0),]$carbon_val_ref <- carb[which(carb$carbon_val < 0),]$carbon_val / abs(min(carb[which(carb$carbon_val < 0),]$carbon_val))

carb$carbon_trt_ref <- 0
carb[which(carb$carbon_treatment > 0),]$carbon_trt_ref <- carb[which(carb$carbon_treatment > 0),]$carbon_treatment / abs(max(carb[which(carb$carbon_treatment > 0),]$carbon_treatment))
carb[which(carb$carbon_treatment < 0),]$carbon_trt_ref <- carb[which(carb$carbon_treatment < 0),]$carbon_treatment / abs(min(carb[which(carb$carbon_treatment < 0),]$carbon_treatment))

write.table(carb, '/home/userz/Desktop/carbon_cartodb_jitter.csv', sep=",", row.names=FALSE, quote=TRUE)




# carb value positive
cvp <-carb$carbon_val_ref[carb$carbon_val_ref > 0]

cvpm <- mean(cvp)
cvps <- sd(cvp)

quantile(cvp[btp < cvpm+cvps], 0.75)
quantile(cvp[btp < cvpm+cvps], 0.5)
quantile(cvp[btp < cvpm+cvps], 0.25)


# carb value negative
cvn <-abs(carb$carbon_val_ref[carb$carbon_val_ref < 0])

cvnm <- mean(cvn)
cvns <- sd(cvn)

quantile(cvn[cvn < cvnm+cvns], 0.75)
quantile(cvn[cvn < cvnm+cvns], 0.5)
quantile(cvn[cvn < cvnm+cvns], 0.25)





# carb treatment positive
ctp <-carb$carbon_trt_ref[carb$carbon_trt_ref > 0]

ctpm <- mean(ctp)
ctps <- sd(ctp)

quantile(ctp[ctp < ctpm+ctps], 0.75)
quantile(ctp[ctp < ctpm+ctps], 0.5)
quantile(ctp[ctp < ctpm+ctps], 0.25)


# carb treatment negative
ctn <-abs(carb$carbon_trt_ref[carb$carbon_trt_ref < 0])

ctnm <- mean(ctn)
ctns <- sd(ctn)

quantile(ctn[ctn < ctnm+ctns], 0.75)
quantile(ctn[ctn < ctnm+ctns], 0.5)
quantile(ctn[ctn < ctnm+ctns], 0.25)









