

library(rgdal)


base_dir <- '/home/userz/projects/wb_vfm_work'

carbon_zones_path <- paste(base_dir, '/carbon_data/wb_pc1_carbon_with_zones_and_extract_fill_type.geojson', sep="")
carbon_zones <- readOGR(carbon_zones_path, "OGRGeoJSON")


# format data types as needed
to_numeric <- "carbon_extract"

for (cname in colnames(carbon_zones@data)) {
    if (to_numeric != "" & regexpr(to_numeric, cname)[1] != -1) {
        carbon_zones@data[cname] <- lapply(carbon_zones@data[cname], as.character)
        carbon_zones@data[cname] <- lapply(carbon_zones@data[cname], as.numeric)
    }
}

carbon_zones@data['carbon_code'] <- lapply(carbon_zones@data['carbon_code'], as.factor)

carbon_zones_backup <- carbon_zones



# carbon_zones <- carbon_zones[which(!is.na(carbon_zones@data$carbon_type)),]
carbon_zone_model <- "carbon_extract_type_fill ~ lnye_2000e*carbon_type"
carbon_zone_results <- lm(carbon_zone_model, carbon_zones@data)

all_coef <- coef(summary(carbon_zone_results))
coef_names <- names(all_coef[,1])
zone_names <- unique(carbon_zones@data$carbon_type)

main_reg <- 'carbon_type'
int_reg <- 'lnye_2000e:carbon_type'



# coef <- all_coef[,1]
# stde <- all_coef[,2]
# tval <- all_coef[,3]
# pval <- all_coef[,4]



# carbon_zones_x <- carbon_zones[which(!is.na(carbon_zones@data$carbon_extract_code_fill)),]
# carbon_zone_model_x <- "carbon_extract_code_fill ~ lnye_2000e*carbon_code"
# carbon_zone_results_x <- lm(carbon_zone_model_x, carbon_zones_x@data)
# 
# all_coef <- coef(summary(carbon_zone_results_x))
# coef_names <- names(all_coef[,1])
# zone_names <- unique(carbon_zones@data$carbon_type)
# 
# main_reg <- 'carbon_code'
# int_reg <- 'lnye_2000e:carbon_code'




# -------------------------------------------------



carbon_data_path <- paste(base_dir,'/wb_pc1_model_04/carbon_output/carbon_par_test_1_merge.geojson', sep="")
carbon_data <- readOGR(carbon_data_path, "OGRGeoJSON")


carbon_data$coef_cz_main <- -999
carbon_data$pval_cz_main <- -999
carbon_data$stde_cz_main <- -999

carbon_data$coef_cz_int <- -999
carbon_data$pval_cz_int <- -999
carbon_data$stde_cz_int <- -999


for (i in 1:nrow(carbon_data)) {
    
    x <- carbon_data@data[i, 'longitude']
    y <- carbon_data@data[i, 'latitude']
    
    match <- carbon_zones[which(carbon_zones@data$longitude == x & carbon_zones@data$latitude == y),]
    # match <- carbon_zones[which(carbon_zones_x@data$longitude == x & carbon_zones_x@data$latitude == y),]
    
    
    if (nrow(match) == 1) {
        
        zone_type <- match@data[1, 'carbon_type']
        # zone_type <- match@data[1, 'carbon_code']
        
        tmp_main_reg <- paste(main_reg, zone_type, sep="")
        tmp_int_reg <- paste(int_reg, zone_type, sep="")
        
        if (tmp_main_reg %in% coef_names & tmp_int_reg %in% coef_names) {

            carbon_data@data[i, 'coef_cz_main'] <- all_coef[tmp_main_reg, 1]
            carbon_data@data[i, 'pval_cz_main'] <- all_coef[tmp_main_reg, 4]
            carbon_data@data[i, 'stde_cz_main'] <- all_coef[tmp_main_reg, 2]
            
            carbon_data@data[i, 'coef_cz_int'] <- all_coef[tmp_int_reg, 1]
            carbon_data@data[i, 'pval_cz_int'] <- all_coef[tmp_int_reg, 4]
            carbon_data@data[i, 'stde_cz_int'] <- all_coef[tmp_int_reg, 2]
            
        } else if (zone_type %in% zone_names) {
            
            carbon_data@data[i, 'coef_cz_main'] <- 0
            carbon_data@data[i, 'pval_cz_main'] <- 0
            carbon_data@data[i, 'stde_cz_main'] <- 0
            
            carbon_data@data[i, 'coef_cz_int'] <- 0
            carbon_data@data[i, 'pval_cz_int'] <- 0
            carbon_data@data[i, 'stde_cz_int'] <- 0
            
        } else {
            
            print(zone_type)
            
            carbon_data@data[i, 'coef_cz_main'] <- -666
            carbon_data@data[i, 'pval_cz_main'] <- -666
            carbon_data@data[i, 'stde_cz_main'] <- -666
            
            carbon_data@data[i, 'coef_cz_int'] <- -666
            carbon_data@data[i, 'pval_cz_int'] <- -666
            carbon_data@data[i, 'stde_cz_int'] <- -666
            
        }
        
    } else if (nrow(match) == 0) {
        
        carbon_data@data[i, 'coef_cz_main'] <- -888
        carbon_data@data[i, 'pval_cz_main'] <- -888
        carbon_data@data[i, 'stde_cz_main'] <- -888
        
        carbon_data@data[i, 'coef_cz_int'] <- -888
        carbon_data@data[i, 'pval_cz_int'] <- -888
        carbon_data@data[i, 'stde_cz_int'] <- -888
        
    } else {
        
        carbon_data@data[i, 'coef_cz_main'] <- -777 
        carbon_data@data[i, 'pval_cz_main'] <- -777
        carbon_data@data[i, 'stde_cz_main'] <- -777
        
        carbon_data@data[i, 'coef_cz_int'] <- -777
        carbon_data@data[i, 'pval_cz_int'] <- -777
        carbon_data@data[i, 'stde_cz_int'] <- -777
        
    }
    
}


carbon_data_out <- carbon_data[which(! carbon_data@data$coef_cz_main %in% c(-999, -888, -777, -666)),]



carbon_data_out$val <- all_coef['(Intercept)', 1] + 
    all_coef['lnye_2000e', 1] * (carbon_data_out$coef_TrtBin + carbon_data_out$coef_TrtBin.sector_group) +
    carbon_data_out$coef_cz_main +
    carbon_data_out$coef_cz_int * (carbon_data_out$coef_TrtBin + carbon_data_out$coef_TrtBin.sector_group)



carbon_data_out$std_err <- -999
carbon_data_out$std_err_val <- -999

for (i in 1:nrow(carbon_data_out@data)) {
    
    coef_main <- carbon_data_out@data[i,'coef_TrtBin']
    pval_main <- carbon_data_out@data[i,'pval_TrtBin']
    coef_int <- carbon_data_out@data[i,'coef_TrtBin.sector_group']
    pval_int <- carbon_data_out@data[i,'pval_TrtBin.sector_group']
    
    std_err_main <- 0
    std_err_int <- 0
    
    if (!is.na(pval_main)) {
        std_err_main <- abs(coef_main / qnorm(1 - (pval_main/2)))
    }
    
    if (!is.na(pval_int)) {
        std_err_int <- abs(coef_int / qnorm(1 - (pval_int/2)))
    } 
    
    std_err <- std_err_main + std_err_int 
    
    carbon_data_out@data[i, 'std_err'] <- std_err
    
    carbon_data_out@data[i, 'std_err_val'] <- all_coef['(Intercept)', 1] + 
        all_coef['lnye_2000e', 1] * std_err +
        carbon_data_out@data[i, 'coef_cz_main'] +
        carbon_data_out@data[i, 'coef_cz_int'] * std_err
    
    
    
}



output_base <- paste(base_dir,'/wb_pc1_model_04/carbon_output/carbon_par_test_1_', sep="")

writeOGR(carbon_data_out, paste(output_base,'final.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(carbon_data_out@data, paste(output_base,'final.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)




carto_fields <- c("TrtBin", "coef_TrtBin", "project_id", "geoname_id", "coef_sector_group", "ad_sector_codes", "place_name", "pval_TrtBin",
                  "sector_split_id", "ad_sector_names", "latitude", "pval_TrtBin.sector_group", "coef_TrtBin.sector_group", "pval_sector_group", "even_split_commitments",
                  "start_actual_isodate", "project_title", "sector_group", "commitment_group", "project_location_id", "longitude", "sector_split_aid", "lnye_1999e", "total_commitments", 
                  "val", "std_err", "std_err_val", "coef_cz_main", "pval_cz_main", "stde_cz_main", "coef_cz_int", "pval_cz_int", "stde_cz_int")

               
carbon_data_carto <- carbon_data_out[,carto_fields]


carto_base <- paste(base_dir,'/wb_pc1_model_04/carbon_output/carbon_cartodb', sep="")
writeOGR(carbon_data_carto, paste(carto_base,'.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(carbon_data_carto@data, paste(carto_base,'.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)


