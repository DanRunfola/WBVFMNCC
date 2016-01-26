
library(sp)
library(maptools)
library(rgdal)
library(gstat)
library(RColorBrewer)
library(classInt)
library(raster)
library(rgeos)
library(geoR)

base_dir <- '/home/userz/Desktop/wb_vfm_work'

pstring <- CRS("+init=epsg:4326")

raw_anc_data <- read.csv(paste(base_dir, "/predictive_grid/interpolation_data/wb_pc1_extract_merge.csv", sep=""))
anc_data <- raw_anc_data[!is.na(raw_anc_data$gpw3_1995e),]

bio_results <- paste(base_dir, "/predictive_grid/interpolation_data/biodiversity_cartodb.csv", sep="")
bio <- read.csv(bio_results)
bio_coords <- as.data.frame(bio[c("latitude", "longitude")])
bio_coords <- jitterDupCoords(bio_coords, max=0.001)
bio$latitude <- bio_coords$latitude
bio$longtiude <- bio_coords$longitude
coordinates(bio) <- ~longitude+latitude
proj4string(bio) <- pstring
bio <- merge(bio, anc_data, by="project_location_id")
b <- bio


carbon_results <- paste(base_dir, "/predictive_grid/interpolation_data/carbon_cartodb.csv", sep="")
carb <- read.csv(carbon_results)
carb_coords <- as.data.frame(carb[c("latitude", "longitude")])
carb_coords <-jitterDupCoords(carb_coords, max=0.001)
carb$latitude <- carb_coords$latitude
carb$longtiude <- carb_coords$longitude
coordinates(carb) <- ~longitude+latitude
proj4string(carb) <- pstring
carb <- merge(carb, anc_data, by="project_location_id")
c <- carb


pred_grid <- paste(base_dir, "/predictive_grid/interpolation_data/WB_Extra_grd.shp", sep="")
pred_gr <- readShapePoly(pred_grid)
proj4string(pred_gr) <- pstring
uID <- rownames(pred_gr@data)
pred_gr@data <- cbind(uID=uID, pred_gr@data)
pred_data <- paste(base_dir, "/predictive_grid/interpolation_data/wbvfm_predictive_points_merge.csv", sep="")
pred_dta <- read.csv(pred_data)
pred_gr <- merge(pred_gr, pred_dta, by.x = "uID", by.y="ad_id")
p <- pred_gr


px <- p[,c('uID','SP_ID.x','SP_ID.y','longitude','latitude')]
writeOGR(px, paste(base_dir, '/predictive_grid_template.geojson', sep=""), 'OGRGeoJSON', drive='GeoJSON')


# countries <- readShapePoly("/media/userz/PENDRIVE/WBVFM/ne_110m_admin_0_countries.shp")
# proj4string(countries) <- pstring




#HISTORIC MODELING FUNCTION

interp_sector = function(x, m) {
    
    proj4string(x) <- pstring
    
    x <- x[x$val <= (sd(x$val)*2),]
    x <- x[x$val >= (-1*(sd(x$val)*2)),]
    # col_se = sector_colors[factor(x$sector_group)[1]]
    # secLook = as.character(factor(x$sector_group)[1])
    # mTitle = sector_title[[secLook]]
    # plot(x, main=mTitle, cex=0.5, col=col_se[[1]])
    # lines(countries)
    
    
    
    
    g <- gstat(id="val", formula=val~1, data=x)
    g <- gstat(g, id="gpw3_2000e", formula=gpw3_2000e~1, data=x)
    
    vario <- variogram(g)
    # plot(vario, main=mTitle)
    
    g <- gstat(g, id=c("val","gpw3_2000e"), model=vgm(psill=cov(x$val, x$gpw3_2000e), model="Exp", range=1500))
    g <- fit.lmc(vario, g, model=vgm(psill=cov(x$gpw3_2000e, x$val), model="Exp", range=1500))
    # plot(vario, g$model, main=mTitle)
    
    #Select a subset of the grid to interpolate to
    xBuf <- gBuffer(x, width=10, byid=TRUE)
    # plot(xBuf, main=mTitle)
    proj4string(xBuf) <- pstring
    # lines(countries)
    m <- m[!is.na(over(m, as(xBuf, "SpatialPolygons"))),]
    # plot(m, main=mTitle)
    # lines(countries)
    proj4string(m) <- pstring
    
    #test <- as.data.frame(coordinates(m))
    
    
    mPtPred <- SpatialPointsDataFrame(coordinates(m), data = m@data, proj4string = CRS(proj4string(m)))
    k <- predict(g, newdata=mPtPred)
    
    uID <- rownames(k@data)
    k@data <- cbind(uID=uID, k@data)
    
    mPred <- merge(m, k@data, by = "uID")
    mPred_out <- mPred[, c("uID", "val.pred")]
    
    #Standard Error interpolations 
    h <- gstat(id="std_err_val", formula=std_err_val~1, data=x)
    h <- gstat(h, id="gpw3_2000e", formula=gpw3_2000e~1, data=x)
    
    varioH <- variogram(h)
    # plot(varioH, main=mTitle)
    
    h <- gstat(h, id=c("std_err_val", "gpw3_2000e"), model=vgm(psill=cov(x$std_err_val, x$gpw3_2000e), model="Exp", range=1500))
    h <- fit.lmc(varioH, h, model=vgm(psill=cov(x$gpw3_2000e, x$std_err_val), model="Exp", range=1500))
    # plot(vario,h$model, main=mTitle)
    
    
    kH <- predict(h, newdata=mPtPred)
    
    uID <- rownames(kH@data)
    kH@data <- cbind(uID=uID, kH@data)
    
    mPred2 <- merge(mPred_out, kH@data, by = "uID")
    mPred_outSE <- mPred2[, c("uID", "val.pred", "std_err_val.pred")]
    proj4string(mPred_outSE) <- pstring
    
    return(mPred_outSE)
    
}




library(foreach)
library(doMC)

registerDoMC(5) 


foreach (active_sector = c('A','B','D','E')) %dopar% {
# for (active_sector in c('A','B','D','E')) {
        
    for (active_commitment in c('low_trtbin', 'medium_trtbin', 'high_trtbin')) {


        # print(paste(active_sector, active_commitment))

#         type <- 'biodiversity'
#         sB <- b[which(b$sector_group == active_sector & b$commitment_group == active_commitment),]
#         out_grid <- interp_sector(sB, p)

        type <- 'carbon'
        sC <- c[which(c$sector_group == active_sector & c$commitment_group == active_commitment),]
        out_grid <- interp_sector(sC, p)
        
        writeOGR(out_grid, paste(base_dir, '/predictive_grid/',type,'/pg_',active_sector,'_',active_commitment,'.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
        
        
    }
}



# -------------------------------------------------


src_grid_path <-  paste(base_dir, "/predictive_grid_template.geojson", sep="")
src_grid <- readOGR(src_grid_path, 'OGRGeoJSON')

# src_grid@data <- cbind(uID=rownames(src_grid@data), src_grid@data)
row.names(src_grid) <- as.character(src_grid$uID)
    
src_grid$uID <- as.numeric(as.character(src_grid$uID))
src_grid <- src_grid[order(src_grid$uID),]
# writeOGR(src_grid, '/home/userz/Desktop/pg_sg2_test.geojson', "OGRGeoJSON", driver='GeoJSON')

tmp_merge <- 0

for (active_sector in c('A','B','C','D','E')) {
    
    for (active_commitment in c('low_trtbin', 'medium_trtbin', 'high_trtbin')) {
 
        # load bio and carbon
        tmp_partial_bio <- readOGR(paste(base_dir, '/predictive_grid/biodiversity/pg_',active_sector,'_',active_commitment,'.geojson', sep=""), "OGRGeoJSON")
        tmp_partial_carbon <- readOGR(paste(base_dir, '/predictive_grid/carbon/pg_',active_sector,'_',active_commitment,'.geojson', sep=""), "OGRGeoJSON")
        
        tmp_partial_bio$uID <- as.numeric(as.character(tmp_partial_bio$uID))
        tmp_partial_carbon$uID <- as.numeric(as.character(tmp_partial_carbon$uID))
        
        # merge
        tmp_partial_merge <- merge(tmp_partial_bio@data, tmp_partial_carbon@data, by = "uID", all = TRUE)
        
        tmp_partial_merge$sector_group <- active_sector
        tmp_partial_merge$commitment_group <- active_commitment
        tmp_partial_merge <- tmp_partial_merge[order(tmp_partial_merge$uID),]
        rownames(tmp_partial_merge) <- tmp_partial_merge$uID
        
        t1 <- src_grid[which(src_grid$uID %in% tmp_partial_merge$uID),]
        t1 <- t1[order(t1$uID),]
        # row.names(t1) <- as.character(t1$uID)
        t2 <- spCbind(t1, tmp_partial_merge)
        # writeOGR(t1, '/home/userz/Desktop/pg_t1_test.geojson', "OGRGeoJSON", driver='GeoJSON')
        

        if (class(tmp_merge) == "numeric") {
            row.names(t2) <- as.character(1:length(t2))
            tmp_merge <- t2
            
        } else {
            row.names(t2) <- as.character((length(tmp_merge)+1):(length(tmp_merge)+length(t2)))
            tmp_merge <- spRbind(tmp_merge, t2)
        }
        # add to main
        
    }
}

names(tmp_merge)[names(tmp_merge) == 'val.pred.x'] <- 'biodiversity_val'
names(tmp_merge)[names(tmp_merge) == 'std_err_val.pred.x'] <- 'biodiversity_std_err_val'
names(tmp_merge)[names(tmp_merge) == 'val.pred.y'] <- 'carbon_val'
names(tmp_merge)[names(tmp_merge) == 'std_err_val.pred.y'] <- 'carbon_std_err_val'
tmp_merge <- tmp_merge[,names(tmp_merge)[!names(tmp_merge) %in% c('uID.1', 'rep_0__len')]]

tmp_merge$carbon_std_err_val <- as.numeric(tmp_merge$carbon_std_err_val)

write.table(tmp_merge, paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid_core.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)

writeOGR(tmp_merge, paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid_core.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')


# -------------------------------------------------


# ratio_list <- c(seq(0.04,1,0.04), seq(1.2, 6, 0.2))

ratio_list <- c('0.04', '0.08', '0.12', '0.16', '0.20', '0.24', '0.28', '0.32', '0.36', '0.40', 
                '0.44', '0.48', '0.52', '0.56', '0.60', '0.64', '0.68', '0.72', '0.76', '0.80', 
                '0.84', '0.88', '0.92', '0.96', '1.00', '1.20', '1.40', '1.60', '1.80', '2.00', 
                '2.20', '2.40', '2.60', '2.80', '3.00', '3.20', '3.40', '3.60', '3.80', '4.00',
                '4.20', '4.40', '4.60', '4.80', '5.00', '5.20', '5.40', '5.60', '5.80', '6.00')


ratio_df <- tmp_merge


ratio_df$carbon_val[is.na(ratio_df$carbon_val)] <- 0.00001
ratio_df$biodiversity_val[is.na(ratio_df$biodiversity_val)] <- 0.00001

for (i in ratio_list) {
    
    tmp_col <- paste('ratio_',gsub("\\.", "_", as.character(i)), sep="")
    ratio_df[[tmp_col]] <- NA
    
    ratio_df[[tmp_col]] <- ratio_df$carbon_val + ratio_df$biodiversity_val * as.numeric(i)
    
}


write.table(ratio_df, paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)
writeOGR(ratio_df, paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')


writeOGR(ratio_df[1:floor(nrow(ratio_df)/2),], paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_gridA.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
writeOGR(ratio_df[floor(nrow(ratio_df)/2):nrow(ratio_df),], paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_gridB.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')

# INSERT INTO predictive_grida (uID, SP_ID, longitude, latitude, biodiversity_val, biodiversity_std_err_val, carbon_val, carbon_std_err_val, sector_group, commitment_group, ratio_0_04, ratio_0_08, ratio_0_12, ratio_0_16, ratio_0_20, ratio_0_24, ratio_0_28, ratio_0_32, ratio_0_36, ratio_0_40, ratio_0_44, ratio_0_48, ratio_0_52, ratio_0_56, ratio_0_60, ratio_0_64, ratio_0_68, ratio_0_72, ratio_0_76, ratio_0_80, ratio_0_84, ratio_0_88, ratio_0_92, ratio_0_96, ratio_1_00, ratio_1_20, ratio_1_40, ratio_1_60, ratio_1_80, ratio_2_00, ratio_2_20, ratio_2_40, ratio_2_60, ratio_2_80, ratio_3_00, ratio_3_20, ratio_3_40, ratio_3_60, ratio_3_80, ratio_4_00, ratio_4_20, ratio_4_40, ratio_4_60, ratio_4_80, ratio_5_00, ratio_5_20, ratio_5_40, ratio_5_60, ratio_5_80, ratio_6_00)
# SELECT uID, SP_ID, longitude, latitude, biodiversity_val, biodiversity_std_err_val, carbon_val, carbon_std_err_val, sector_group, commitment_group, ratio_0_04, ratio_0_08, ratio_0_12, ratio_0_16, ratio_0_20, ratio_0_24, ratio_0_28, ratio_0_32, ratio_0_36, ratio_0_40, ratio_0_44, ratio_0_48, ratio_0_52, ratio_0_56, ratio_0_60, ratio_0_64, ratio_0_68, ratio_0_72, ratio_0_76, ratio_0_80, ratio_0_84, ratio_0_88, ratio_0_92, ratio_0_96, ratio_1_00, ratio_1_20, ratio_1_40, ratio_1_60, ratio_1_80, ratio_2_00, ratio_2_20, ratio_2_40, ratio_2_60, ratio_2_80, ratio_3_00, ratio_3_20, ratio_3_40, ratio_3_60, ratio_3_80, ratio_4_00, ratio_4_20, ratio_4_40, ratio_4_60, ratio_4_80, ratio_5_00, ratio_5_20, ratio_5_40, ratio_5_60, ratio_5_80, ratio_6_00
# FROM predictive_gridb


pure_ratio_df <- ratio_df[which(ratio_df$carbon_val != 0.00001 & ratio_df$biodiversity_val != 0.00001),]
writeOGR(pure_ratio_df, paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid_pure.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
# writeOGR(pure_ratio_df[1:2,], paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid_pureA.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
# writeOGR(pure_ratio_df[2:nrow(pure_ratio_df),], paste(base_dir, '/predictive_grid/predictive_grid_final/predictive_grid_pureB.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')







