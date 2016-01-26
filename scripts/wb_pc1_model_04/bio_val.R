

library(rgdal)


base_dir <- '/home/userz/Desktop/wb_vfm_work'

bio_path <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/bio_par_test_1_merge_area.geojson', sep="")
bio_data <- readOGR(bio_path, "OGRGeoJSON")


# summary(bio_data$iba_area)

bio_data$val <- (bio_data$coef_TrtBin + bio_data$coef_TrtBin.sector_group) * bio_data$iba_area / 3



output_base <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/bio_par_test_1_', sep="")
writeOGR(bio_data, paste(output_base,'final.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(bio_data@data, paste(output_base,'final.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)



# ------------------------------------------------------


bio_path <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/bio_par_test_1_final.geojson', sep="")
bio_data <- readOGR(bio_path, "OGRGeoJSON")


bio_data$std_err <- -999
bio_data$std_err_val <- -999

for (i in 1:nrow(bio_data@data)) {
    
    coef_main <- bio_data@data[i,'coef_TrtBin']
    pval_main <- bio_data@data[i,'pval_TrtBin']
    coef_int <- bio_data@data[i,'coef_TrtBin.sector_group']
    pval_int <- bio_data@data[i,'pval_TrtBin.sector_group']
    
    std_err_main <- 0
    std_err_int <- 0
    
    if (!is.na(pval_main)) {
        std_err_main <- abs(coef_main / qnorm(1 - (pval_main/2)))
    }
    
    if (!is.na(pval_int)) {
        std_err_int <- abs(coef_int / qnorm(1 - (pval_int/2)))
    } 
    
    std_err <- std_err_main + std_err_int
    
    bio_data@data[i, 'std_err'] <- std_err

    bio_data@data[i, 'std_err_val'] <- std_err * bio_data@data[i,'iba_area'] / 3
    
}


carto_fields <- c("iba_area", "TrtBin", "coef_TrtBin", "project_id", "geoname_id", "iba_statescore", "coef_sector_group", "ad_sector_codes", "place_name", "pval_TrtBin",
"sector_split_id", "iba_distance", "ad_sector_names", "latitude", "pval_TrtBin.sector_group", "coef_TrtBin.sector_group", "pval_sector_group", "even_split_commitments",
"start_actual_isodate", "project_title", "sector_group", "commitment_group", "project_location_id", "longitude", "sector_split_aid", "lnye_1999e", "total_commitments", 
"val", "std_err", "std_err_val")

bio_data_out <- bio_data[,carto_fields]

output_base <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/bio_par_test_1_', sep="")
writeOGR(bio_data, paste(output_base,'final.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(bio_data@data, paste(output_base,'final.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)

carto_base <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/biodiversity_cartodb', sep="")
writeOGR(bio_data_out, paste(carto_base,'.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(bio_data_out@data, paste(carto_base,'.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)



