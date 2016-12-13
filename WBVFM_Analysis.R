library(sp)
library(maptools, quietly=TRUE)
library(doBy)
library(ggplot2)
library(gridExtra)
library(maps)

carbon_data_path = "/home/aiddata/Desktop/Github/WBVFMNCC/data/carbon_cartodb.csv"
cdb <- read.csv(carbon_data_path)

##Bind the latitude and longitude for the SPDF
coords = c(cdb["longitude"],cdb["latitude"])

##Create a spatial points data frame
spdf_LL <- SpatialPointsDataFrame(coords, cdb)

#read in Natural Earth country boundaries
countryBounds <- "/home/aiddata/Desktop/Github/WBVFMNCC/data/ne_10m_admin_0_countries.shp"
cBnd <- readShapePoly(countryBounds)

overlay <- over(spdf_LL, cBnd[,"REGION_WB"])
spdf_LL$REGION_WB <- overlay$REGION_WB

#Just keep the columns we need for the chart
uni_spdf_LL2 <- spdf_LL[spdf_LL@data$commitment_group == "high_trtbin",]
uni_spdf_LL <- uni_spdf_LL2@data[unique(uni_spdf_LL2$project_location_id),]

#Map
uni_spdf_map <- spdf_LL


fig_df_uni <- uni_spdf_LL[c("REGION_WB","val","start_actual_isodate")]
fig_df_uni$count <- 1
figSum_uni <- summaryBy(val + count ~ REGION_WB + start_actual_isodate, data=fig_df_uni, FUN=sum)
#Remove project locations which did not fall into a country for this figure
figSum_uni_noNA <- figSum_uni[!is.na(figSum_uni["REGION_WB"]),]

spdf_LL2 <- spdf_LL[spdf_LL@data$commitment_group == "high_trtbin",]

fig_df <- spdf_LL2@data[c("REGION_WB","val","start_actual_isodate")]
fig_df$count <- 1



figSum <- summaryBy(val + count ~ REGION_WB + start_actual_isodate, data=fig_df, FUN=sum)
#Remove project locations which did not fall into a country for this figure
figSum_noNA <- figSum[!is.na(figSum["REGION_WB"]),]

figSum_noNA$avgProjVal <- figSum_noNA$val.sum / figSum_noNA$count.sum

text_size_for_figs = 11

Avg_Proj <- ggplot(data=figSum_noNA, aes(x=start_actual_isodate, y=avgProjVal, group=REGION_WB, colour=factor(REGION_WB))) +
  geom_line(size=.5, linetype=3) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  stat_smooth(se=FALSE, method="loess") +
  geom_point()+
  labs(x="Project Start Year", y="Avg. Location Sequestration (Tonnes)") +
  theme_bw() +
  theme(legend.position="none", text=element_text(size=text_size_for_figs))

Total_Proj <- ggplot(data=figSum_noNA, aes(x=start_actual_isodate, y=val.sum, group=REGION_WB, colour=factor(REGION_WB), name="Region")) +
  geom_line(size=.5, linetype=3) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  stat_smooth(se=FALSE, method="loess") +
  geom_point()+
  labs(x="Project Start Year", y="Total Tonnes of Sequestered Carbon Attributable to World Bank")+
  guides(color=guide_legend(title="Region")) +
  theme_bw()+
  theme(legend.position = c(0,1), legend.justification = c(0, 1), legend.key.size = unit(0.2,"cm"), legend.background=element_rect(fill=alpha('white', 0.1)), text=element_text(size=text_size_for_figs)) 

Count_Proj <- ggplot(data=figSum_uni_noNA, aes(x=start_actual_isodate, y=count.sum, group=REGION_WB, colour=factor(REGION_WB), name="Region")) +
  geom_line(size=.5, linetype=3) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  stat_smooth(se=FALSE, method="loess") +
  geom_point()+
  labs(x="Project Start Year", y="Total Project Locations")+
  guides(color=guide_legend(title="Region")) +
  theme_bw() +
  theme(legend.position="none", text=element_text(size=text_size_for_figs))

proj_count <- as.character(61243)
active_proj_count <- as.character(41306)
pc1_proj_count <- as.character(19940)
analysis_proj_count <- as.character(19940/3)



proj_est_count <- length(unique(cdb$project_location_id))

high_proj <- cdb[cdb$commitment_group == "high_trtbin",]
proj_est_count_high <- length(unique(high_proj$project_location_id))


tonnes_sequestered <- formatC(sum(spdf_LL@data$val), format="d", big.mark=',')

png('result_disag.png')
grid.arrange(Total_Proj, Count_Proj, Avg_Proj, ncol=2, layout_matrix = cbind(c(1,1),c(2,3)))
dev.off()

png('WLocs.png', width=6*300, height=4*300)
map("world", fill=TRUE, col="white", bg="white", ylim=c(-60, 90), mar=c(0,0,0,0))
points(coordinates(uni_spdf_map), col="red", pch=20, cex=0.001)
dev.off()
