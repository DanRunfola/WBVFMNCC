
library(rgdal)
library(maptools)

base_dir <- '/home/userz/Desktop/wb_vfm_work'


# data_low <- readOGR(paste(base_dir, "/wb_pc_model_04/biodiversity_output/bio_par_test_1_low_trtbin_results.geojson", sep=""), "OGRGeoJSON")
# data_medium <- readOGR(paste(base_dir, "/wb_pc_model_04/biodiversity_output/bio_par_test_1_medium_trtbin_results.geojson", sep=""), "OGRGeoJSON")
# data_high <- readOGR(paste(base_dir, "/wb_pc_model_04/biodiversity_output/bio_par_test_1_high_trtbin_results.geojson", sep=""), "OGRGeoJSON")

data_low <- readOGR(paste(base_dir, "/wb_pc_model_04/carbon_output/carbon_par_test_1_low_trtbin_results.geojson", sep=""), "OGRGeoJSON")
data_medium <- readOGR(paste(base_dir, "/wb_pc_model_04/carbon_output/carbon_par_test_1_medium_trtbin_results.geojson", sep=""), "OGRGeoJSON")
data_high <- readOGR(paste(base_dir, "/wb_pc_model_04/carbon_output/carbon_par_test_1_high_trtbin_results.geojson", sep=""), "OGRGeoJSON")


data_low_cut <-data_low[which(data_low@data$TrtBin == 1),]
data_medium_cut <- data_medium[which(data_medium@data$TrtBin == 1),]
data_high_cut <- data_high[which(data_high@data$TrtBin == 1),]


to_numeric <- "coef_|pval_"

for ( cname in colnames(data_low_cut@data) ) {
    if (to_numeric != "" & regexpr(to_numeric, cname)[1] != -1) {
        data_low_cut@data[cname] <- lapply(data_low_cut@data[cname], as.character)
        data_low_cut@data[cname] <- lapply(data_low_cut@data[cname], as.numeric)
        
        data_medium_cut@data[cname] <- lapply(data_medium_cut@data[cname], as.character)
        data_medium_cut@data[cname] <- lapply(data_medium_cut@data[cname], as.numeric)
        
        data_high_cut@data[cname] <- lapply(data_high_cut@data[cname], as.character)
        data_high_cut@data[cname] <- lapply(data_high_cut@data[cname], as.numeric)
        
    }
}


data_out_raw <- 0
data_out <- 0

data_out_raw <- spRbind(data_low_cut, data_medium_cut)
data_out_raw <- spRbind(data_out_raw, data_high_cut)

data_out <- data_out_raw[which(!is.na(data_out_raw$coef_TrtBin) & !is.na(data_out_raw$coef_sector_group) & !is.na(data_out_raw$coef_TrtBin.sector_group)),]

# data_out <- data_out_raw[which(!is.na(data_out_raw$coef_TrtBin)),]
# data_out <- data_out_raw[which(!is.na(data_out_raw$coef_sector_group)),]
# data_out <- data_out_raw[which(!is.na(data_out_raw$coef_TrtBin.sector_group)),]



# output_base <- paste(base_dir, '/wb_pc_model_04/biodiversity_output/bio_par_test_1_', sep="")

output_base <- paste(base_dir, '/wb_pc_model_04/carbon_output/carbon_par_test_1_', sep="")

writeOGR(data_out, paste(output_base,'merge.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(data_out@data, paste(output_base,'merge.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)


