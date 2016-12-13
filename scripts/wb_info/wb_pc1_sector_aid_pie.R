
library("rgdal")
shpfile = "/home/userz/projects/wb_vfm_work/wb_pc1/wb_pc1.geojson"
dta_Shp = readOGR(shpfile, "OGRGeoJSON")


combined_sectors <- c()
for (i in 1:length(dta_Shp@data$ad_sector_names)) {
    x = unlist(strsplit(as.character(dta_Shp@data$ad_sector_names[i]), "|", fixed=TRUE))
    combined_sectors <-c (combined_sectors, x)
}

unique_sectors <- unique(combined_sectors)


dollar_list <- c()

for (i in 1:length(unique_sectors)) {
    
    sector <- unique_sectors[i]

    filtered <- subset(dta_Shp@data, grepl(sector, ad_sector_names))
    
    dollars <- sum(filtered$total_commitments)
    
    dollar_list[sector] <- dollars

}

dlist <- dollar_list[ dollar_list >= sort(dollar_list, decreasing=TRUE)[10] ]
dlist['Other'] <- dlist['Other'] + sum(dollar_list[ dollar_list < sort(dollar_list, decreasing=TRUE)[10] ])

pie(dlist, cex=1.7)

