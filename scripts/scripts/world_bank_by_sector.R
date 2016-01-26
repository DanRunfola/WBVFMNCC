
all_data = read.delim("/home/userz/Desktop/wb_vfm_work/wb_pc1/wb_pc1.tsv")


p_sect = data.frame(all_data$project_location_id, all_data$ad_sector_names, all_data$even_split_commitments)
names(p_sect) = c("project_location_id", "ad_sector_names", "even_split_commitments")


sector_groups <- c()

sector_groups[["A"]] <- c(
    "Health",
    "Health, general",
    "Water supply and sanitation"
)

sector_groups[["B"]] <- c(
    "Agriculture",
    "Agriculture, forestry, fishing",
    "Forestry",
    "General environmental protection"
)

sector_groups[["C"]] <- c(
    "Education, level unspecified",
    "Basic education",
    "Post-secondary education",
    "Secondary education"
)

sector_groups[["D"]] <- c(
    "Mineral resources and mining",
    "Industry",
    "Energy generation and supply",
    "Transport and storage"
)

sector_groups[["E"]] <- c(
    "Communications",
    "Banking and financial services",
    "Trade policy and regulations",
    "Government and civil society, general",
    "Other social infrastructure and services"
)

sector_groups[["F"]] <- c(
    "Other",
    "Unallocated/ unspecified"
)


sector_counts <- c()
sector_counts[["A"]] <- 0
sector_counts[["B"]] <- 0
sector_counts[["C"]] <- 0
sector_counts[["D"]] <- 0
sector_counts[["E"]] <- 0
sector_counts[["F"]] <- 0


p_sect$sector_split_aid <- 0
p_sect$sector_group <- 0
output <- p_sect[FALSE,]

for (k in 1:nrow(p_sect)){
    row <- p_sect[k,]
    sector_string <- row$ad_sector_names
    
    # sector_log = c()
    split_row_sectors <- unlist(strsplit(as.character(sector_string), '|', fixed = TRUE))
    unique_row_sectors <- unique(split_row_sectors)
    
    outrows <- c()
    outrows[['A']] <- row
    outrows[['B']] <- row
    outrows[['C']] <- row
    outrows[['D']] <- row
    outrows[['E']] <- row
    outrows[['F']] <- row
    
    for (i in unique_row_sectors) {
        for (j in names(sector_counts)){
            if (i %in% sector_groups[[j]] ) {
                outrows[[j]]$sector_split_aid <- outrows[[j]]$sector_split_aid + row$even_split_commitments * (sum(i==split_row_sectors) / length(split_row_sectors))
            }
        }
    }
    

    for (j in names(sector_counts)){
        if (outrows[[j]]$sector_split_aid != 0) {
            outrows[[j]]$sector_group <- j
            sector_counts[[j]] <- sector_counts[[j]] + 1
            output[nrow(output)+1,] <- outrows[[j]]
        }
    }    

}




unique_names = unique(unlist(strsplit(as.character(p_sect$ad_sector_names), '|', fixed = TRUE)))
unique_counts = data.frame(unique_names, rep(0, length(unique_names)), rep(0, length(unique_names)))
names(unique_counts) = c("unique_sector", "project_count", "sum of split aid" )

for( i in 1:length(unique_names)){
  name = unique_names[i]
  temp_all_data = all_data[grep(name, p_sect$ad_sector_names), ]
  unique_counts[i,2] = nrow(temp_all_data)
  unique_counts[i,3] = sum(temp_all_data$even_split_commitments)
}



par(las=2)
barplot(unique_counts$project_count, 
        space = .5,
        main = "Project count by Sector", 
        names.arg = unique_counts$unique_sector,
        cex.names = 0.6)

par(las = 2)
barplot(unique_counts$"sum of split aid", 
        space = .5,
        main = "Aid by Sector", 
        names.arg = unique_counts$unique_sector,
        cex.names = 0.6)


