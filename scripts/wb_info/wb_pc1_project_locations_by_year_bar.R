
library("rgdal")
shpfile = "/home/userz/projects/wb_vfm_work/wb_pc1/wb_pc1.geojson"
dta_Shp = readOGR(shpfile, "OGRGeoJSON")


isodates <- sapply(dta_Shp$start_actual_isodate, as.character)

year_str <- sapply(isodates, function(z) {
    return(substr(z, 1, 4))
})

year <- sapply(year_str, as.numeric)


data <- data.frame(table(year))
colnames(data)[2] <- "projects"


library(ggplot2)


ggplot(data=data, aes(x=year,y=projects)) +
    geom_bar(position="dodge",stat="identity", color='black', fill='blue') + 
    coord_flip() +
    ggtitle("Project Locations by Year")+
    theme(text = element_text(color='white'), 
          axis.text.y = element_text(size=35, color='black'),
          axis.text.x = element_text(size=35, color='black'))







