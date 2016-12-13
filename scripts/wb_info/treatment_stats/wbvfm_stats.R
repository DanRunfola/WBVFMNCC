

library(ggplot2)

biodiversity_path <- "/home/userz/projects/wb_vfm_work/wb_pc1_model_04/biodiversity_output/biodiversity_cartodb.csv"
carbon_path <- "/home/userz/projects/wb_vfm_work/wb_pc1_model_04/carbon_output/carbon_cartodb.csv"

data <- c()
data$biodiversity <- read.csv(biodiversity_path)
data$carbon <- read.csv(carbon_path)


output <- data.frame(id=rep(1:15, 2))
output$type <- c(rep('biodiversity', 15), rep('carbon', 15))
output$sector_group <- rep(c('A','B','C','D','E'), 6)
output$commitment_group <- rep(c(rep('low', 5), rep('medium', 5), rep('high', 5)), 2)


output$field_dir <- 'positive'
output$field_sig <- '<0.05'
output$field_dir_sig <- 'positive'

output$count_dir <- NA
output$count_sig <- NA
output$count_dir_sig <- NA

output_1 <- output
output_2 <- output
output_2$field_dir <- 'negative'
output_2$field_sig <- '>0.05'
output_2$field_dir_sig <- 'negative'

output <- rbind(output_1, output_2)



# -------------------------------------------------
# field dir


for (i in 1:nrow(output)) {
    
    field <- output[i, 'field_dir']
    type <- output[i, 'type']
    sector <- output[i, 'sector_group']
    commitment <- paste(output[i, 'commitment_group'], 'trtbin', sep="_")

    print(paste(field,type,sector,commitment))
    
    tmp_data <- data[[type]][which(data[[type]]$sector_group == sector & data[[type]]$commitment_group == commitment),]

    
    if (field == "positive") {

        tmp_count <- sum(tmp_data$val > 0)

    } else {
        tmp_count <- sum(tmp_data$val < 0)
    }

    output[i, 'count_dir'] <- tmp_count

}


b_out_dir <- output[which(output$type == 'biodiversity'),]

ggplot(b_out_dir, aes(x=commitment_group, y=count_dir, fill = field_dir)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
    ggtitle("Biodiversity - positive / negative")



c_out_dir <- output[which(output$type == 'carbon'),]

ggplot(c_out_dir, aes(x=commitment_group, y=count_dir, fill = field_dir)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
    # scale_fill_brewer(palette = "Set1") + 
    ggtitle("Carbon - positive / negative")


# -------------------------------------------------
# field sig


for (i in 1:nrow(output)) {
    
    field <- output[i, 'field_sig']
    type <- output[i, 'type']
    sector <- output[i, 'sector_group']
    commitment <- paste(output[i, 'commitment_group'], 'trtbin', sep="_")
    
    print(paste(type,sector, commitment))
    
    tmp_data <- data[[type]][which(data[[type]]$sector_group == sector & data[[type]]$commitment_group == commitment),]
    
    
    if (field == "<0.05") {
        
        tmp_count <- sum(tmp_data$pval_TrtBin <= 0.05)
        
    } else {
        tmp_count <- sum(tmp_data$pval_TrtBin > 0.05)
    }
    
    output[i, 'count_sig'] <- tmp_count
    
}


b_out_sig <- output[which(output$type == 'biodiversity'),]

ggplot(b_out_sig, aes(x=commitment_group, y=count_sig, fill = field_sig)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#377eb8", "#e41a1c")) +
    ggtitle("Biodiversity - pval <0.05 / >0.05")



c_out_sig <- output[which(output$type == 'carbon'),]

ggplot(c_out_sig, aes(x=commitment_group, y=count_sig, fill = field_sig)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#377eb8", "#e41a1c")) +
    ggtitle("Carbon - pval <0.05 / >0.05")



# -------------------------------------------------
# field dir sig


for (i in 1:nrow(output)) {
    
    field <- output[i, 'field_dir_sig']
    type <- output[i, 'type']
    sector <- output[i, 'sector_group']
    commitment <- paste(output[i, 'commitment_group'], 'trtbin', sep="_")
    
    print(paste(field,type,sector,commitment))
    
    tmp_data <- data[[type]][which(data[[type]]$sector_group == sector & data[[type]]$commitment_group == commitment & data[[type]]$pval_TrtBin <= 0.05),]
    
    
    if (field == "positive") {
        
        tmp_count <- sum(tmp_data$val > 0)
        
    } else {
        tmp_count <- sum(tmp_data$val < 0)
    }
    
    output[i, 'count_dir_sig'] <- tmp_count
    
}


b_out_dir_sig <- output[which(output$type == 'biodiversity'),]

ggplot(b_out_dir_sig, aes(x=commitment_group, y=count_dir_sig, fill = field_dir_sig)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
    ggtitle("Biodiversity (pval <= 0.05) - positive / negative")



c_out_dir_sig <- output[which(output$type == 'carbon'),]

ggplot(c_out_dir_sig, aes(x=commitment_group, y=count_dir_sig, fill = field_dir_sig)) + 
    geom_bar(stat="identity", position = "stack") +
    facet_grid(~ sector_group) +
    scale_fill_manual(values=c("#e41a1c", "#377eb8")) +
    # scale_fill_brewer(palette = "Set1") + 
    ggtitle("Carbon (pval <= 0.05) - positive / negative")





