
library(rgdal)
library(maptools)


base_dir <- '/home/userz/Desktop/wb_vfm_work'

data_merge <- 0

for (i in as.character(1:8)) {

	data_tmp <- readOGR(paste(base_dir, "/wb_pc_model_04/carbon_output/carbon_par_medium/carbon_medium_trtbin_",i,"_results.geojson", sep=""), "OGRGeoJSON")

	if (class(data_merge) == class(0)) {

		data_merge <- data_tmp

	} else {

		data_merge <- spRbind(data_merge, data_tmp)

	}

}


#to_numeric <- "coef_|pval_"

#for ( cname in colnames(data_merge@data) ) {
#    if (to_numeric != "" & regexpr(to_numeric, cname)[1] != -1) {
#        data_merge@data[cname] <- lapply(data_merge@data[cname], as.character)
#        data_merge@data[cname] <- lapply(data_merge@data[cname], as.numeric)
#    }
#}


output_base <- paste(base_dir, '/wb_pc_model_04/carbon_output/carbon_par_test_1_medium_trtbin_results', sep="")

writeOGR(data_merge, paste(output_base,'.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
write.table(data_merge@data, paste(output_base,'.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)


