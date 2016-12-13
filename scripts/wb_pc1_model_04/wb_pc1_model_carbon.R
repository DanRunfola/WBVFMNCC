

# library(devtools)
# devtools::install_github("sgoodm/SCI@master")
library(SCI2)
loadLibs()

base_dir <- '/home/userz/Desktop/wb_vfm_work'

shpfile = paste(base_dir, "/home/userz/Desktop/wb_pc1_model_04/wb_pc1_data_sectors_short_carbon.geojson", sep="")

dta_Shp = readOGR(shpfile, "OGRGeoJSON")

# backup for testing
tmp <- dta_Shp


# format data types as needed
# to_numeric <- "lnye|pre_avg_NDVI_max|post_avg_NDVI_max"

# for (i in 1:length(colnames(dta_Shp@data))) {
#     cname <- colnames(dta_Shp@data)[i]
#     if (to_numeric != "" & regexpr(to_numeric, cname)[1] != -1) {
#         dta_Shp@data[cname] <- lapply(dta_Shp@data[cname], as.character)
#         dta_Shp@data[cname] <- lapply(dta_Shp@data[cname], as.numeric)
#     }
# }


# writeOGR(bio, "/home/userz/Desktop/wb_pc1_test_short_bio.geojson", "results", driver='GeoJSON')
# write.table(dta_Shp@data, "/home/userz/Desktop/wb_pc1_test_shortz.csv", sep=",", row.names=FALSE, quote = TRUE)


# ==========================
# ==========================


# treatment_thresh_half <- quantile(dta_Shp@data[["sector_split_aid"]], 0.5)
# dta_Shp@data[["TrtBin"]] <- dta_Shp@data[["sector_split_aid"]] > treatment_thresh_half

# -----

# treatment_thresh_low <- quantile(dta_Shp@data[["sector_split_aid"]], 0.33)
# treatment_thresh_high <- quantile(dta_Shp@data[["sector_split_aid"]], 0.66)

# dta_Shp@data[["TrtBin"]] <- dta_Shp@data[["sector_split_aid"]] < treatment_thresh_low
# dta_Shp@data[["TrtBin"]] <- dta_Shp@data[["sector_split_aid"]] >= treatment_thresh_low & dta_Shp@data[["sector_split_aid"]] <= treatment_thresh_high
# dta_Shp@data[["TrtBin"]] <- dta_Shp@data[["sector_split_aid"]] > treatment_thresh_high
# dta_Shp@data[["TrtBin"]] <- as.integer(dta_Shp@data[["TrtBin"]])

# -----


# dta_Shp$TrtBin <- 0
# dta_Shp$low_trtbin <- 0
# dta_Shp$medium_trtbin <- 0
# dta_Shp$high_trtbin <- 0


# for ( i in c('A','B','C','D','E','F') ) {
#     tmp_thresh_1 <- quantile(dta_Shp@data[dta_Shp@data$sector_group == i,][["sector_split_aid"]], 0.33)
#     tmp_thresh_2 <- quantile(dta_Shp@data[dta_Shp@data$sector_group == i,][["sector_split_aid"]], 0.66)

#     dta_Shp@data[dta_Shp@data$sector_group == i,][['low_trtbin']] <- dta_Shp@data[dta_Shp@data$sector_group == i,][['sector_split_aid']] < tmp_thresh_1
#     dta_Shp@data[dta_Shp@data$sector_group == i,][['medium_trtbin']] <- dta_Shp@data[dta_Shp@data$sector_group == i,][['sector_split_aid']] >= tmp_thresh_1 & dta_Shp@data[dta_Shp@data$sector_group == i,][['sector_split_aid']] <= tmp_thresh_2
#     dta_Shp@data[dta_Shp@data$sector_group == i,][['high_trtbin']] <- dta_Shp@data[dta_Shp@data$sector_group == i,][['sector_split_aid']] > tmp_thresh_2
    
# }

# example trtbin assignment
# dta_Shp@data[["TrtBin"]] <- as.integer(dta_Shp@data[["low_trtbin"]])

# -----

dta_Shp$commitment_group <- 0

for ( i in c('A','B','C','D','E','F') ) {
    tmp_thresh_1 <- quantile(dta_Shp@data[dta_Shp@data$sector_group == i,][["sector_split_aid"]], 0.33)
    tmp_thresh_2 <- quantile(dta_Shp@data[dta_Shp@data$sector_group == i,][["sector_split_aid"]], 0.66)
    
    dta_Shp@data[which(dta_Shp@data$sector_group == i & dta_Shp@data$sector_split_aid < tmp_thresh_1),][['commitment_group']] <- 'low_trtbin'
    dta_Shp@data[which(dta_Shp@data$sector_group == i & dta_Shp@data$sector_split_aid >= tmp_thresh_1 & dta_Shp@data$sector_split_aid <= tmp_thresh_2),][['commitment_group']] <- 'medium_trtbin' 
    dta_Shp@data[which(dta_Shp@data$sector_group == i & dta_Shp@data$sector_split_aid > tmp_thresh_2 ),][['commitment_group']] <- 'high_trtbin'
    
}

# example trtbin assignment
# dta_Shp@data[["TrtBin"]] <- as.integer(dta_Shp@data[["commitment_group"]] == "low_trtbin")



# ==========================
# ==========================


test <- function (model, dta, type, table_out = NULL, opts = NULL, force_posdef = TRUE) {
    
    ret_var <- list()
    
    if (type == "lm") {
        m_fit <- lm(model, dta)
        # print("==========================")
        # print("UNSTANDARDIZED MODEL RESULTS")
        # print("==========================")
        # print(summary(m_fit))
        ret_var$unstandardized <- m_fit
        # mTab <- stargazer(m_fit,type="html",title="Unstandardized Model Results")
        # texreg::plotreg(m_fit, omit.coef="(match)|(Intercept)", custom.model.names="Unstandardized Model", custom.note=model)
        
        if (!is.null(table_out)) {
            dta_tmp <- dta
            
            if (class(dta) == "data.frame") {
                d_index <- sapply(dta_tmp, is.numeric)
                dta_tmp[d_index] <- lapply(dta_tmp[d_index], scale)
            } else {
                d_index <- sapply(dta_tmp@data, is.numeric)
                # dta_tmp@data[d_index] <- lapply(dta_tmp@data[d_index], scale)
                dta_tmp@data[d_index] <- sapply(dta_tmp@data[d_index], function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
            }
            
            dta_fit_std <- lm(model, dta_tmp)
            ret_var$standardized <- dta_fit_std
            print("==========================")
            print("STANDARDIZED MODEL RESULTS")
            print("==========================")
            print(summary(dta_fit_std))
            # texreg::plotreg(dta_fit_std,omit.coef="(match)|(Intercept)",custom.model.names="Standardized Model", custom.note=model)
            
        }
        
    }
    return(ret_var)
}


# ==========================
# ==========================


# options

library(raster)
library(foreach)
library(doMC)
registerDoMC(3) 

psmModel <-  "TrtBin ~ gpw3_2000e + at41_1999e + pc41_1999e + alp4_1999e + lnye_1999e + 
am50_e + sslp_e + selv_e + dari_e + droa_e +
pre_avg_NDVI_max + pre_avg_temp_mean + pre_avg_precip_mean + post_avg_NDVI_max + post_avg_temp_mean + post_avg_precip_mean + 
pre_trend_temp_mean + pre_trend_precip_mean + post_trend_temp_mean + post_trend_precip_mean +
sector_group"

pModelMax <- "post_avg_NDVI_max ~ TrtBin + PSM_match_ID + gpw3_2000e + alp4_1999e +
am50_e + sslp_e + selv_e + dari_e + droa_e + 
pre_avg_NDVI_max + pre_avg_temp_mean + pre_avg_precip_mean + post_avg_temp_mean + post_avg_precip_mean +
pre_trend_temp_mean + pre_trend_precip_mean + post_trend_temp_mean + post_trend_precip_mean + 
sector_group + sector_group:TrtBin"


zone_radius_dist_thresh <- 800 * 1000

sat_drop_set <- c(drop_unmatched=TRUE, drop_method="None", drop_thresh=0.25)
sat_constraints <- c() # c("CONT")


foreach ( k=c('low_trtbin', 'medium_trtbin', 'high_trtbin') ) %dopar% {    
# for ( k in c('low_trtbin') ) {
# for ( k in c('low_trtbin', 'medium_trtbin', 'high_trtbin') ) {


    output_base <- paste(base_dir, '/wb_pc_model_04/carbon_output/carbon_par_test_1_',k,'_', sep='')


    print(k)
    # writeLines(c(''), paste('/home/userz/Desktop/wb_pc1_model_04/log_bio1_',k,'.txt', sep=''))
    writeLines(c(''), paste(output_base,'log.txt', sep=""))


    # dta_Shp@data[["TrtBin"]] <- as.integer(dta_Shp@data[[k]])
    dta_Shp@data[["TrtBin"]] <- as.integer(dta_Shp@data[["commitment_group"]] == k)



    psmRes <- SCI2::SpatialCausalPSM(dta_Shp, mtd="logit", psmModel, drop="None", visual=FALSE)
    tmp_psmRes <- psmRes$data


    # build list of all lng, lat
    allPoints <- cbind(dta_Shp@data[['longitude']], dta_Shp@data[['latitude']])

    proj_count <- c()

    output_gen <- 0

    output_data <- dta_Shp


    # ======================================
    

    # for (i in 165:170)  {    
    for (i in 1:nrow(tmp_psmRes@data)) {
    
        
        sink(paste(output_base,'log.txt', sep=""), append=TRUE)
        print(paste('start iteration:', i, '-',Sys.time()))

        
        # get row dat and project location id
        row <- tmp_psmRes@data[i,]
        row_id <- row['sector_split_id']
        row_sector_group <- row[['sector_group']]
        print(paste('row: id =', row_id, ' and group = ',row_sector_group))
        
        # get lng, lat
        rowPoint <- c(row[['longitude']], row[['latitude']])
        
        # check distance to all points
        # pointDistance is from raster package
        dist_list <- pointDistance(rowPoint, allPoints, lonlat=TRUE)
        
        # filter data based on points under specified distance
        matches <- dist_list < zone_radius_dist_thresh
        proj_count[i] <- sum(matches)
        print(paste('total matches:', proj_count[i]))
        
        # tmp_data <- dta_Shp[matches,]
        tmp_data <- tmp_psmRes[matches,]
        

        pairs_data <- tmp_data[!is.na(tmp_data@data$PSM_trtProb),]
        print(paste('nrows pairs_data:', nrow(pairs_data)))
        
        if (nrow(pairs_data) > 100) {            
            # based on the Propensity Score Matches, pair comprable treatment and control units

            psmPairs <- SCI2::SAT(dta = pairs_data, mtd = "fastNN", constraints=sat_constraints, psm_eq = psmModel, ids = "sector_split_id", drop_opts = sat_drop_set, visual="FALSE", TrtBinColName="TrtBin")
        
              
            if ( class(psmPairs) != class('drop') ) {
                print(paste('nrows psmPairs:', nrow(psmPairs@data)))

                if ( nrow(psmPairs@data) < 100 ) {
                    psmPairs <- 'drop1'
                }       
            }
            
        } else {
            psmPairs <- 'drop2'
        }

        
        if ( class(psmPairs) == class('drop') ) {
            print(psmPairs)
            
        } else {
            # psmPairs@data['PSM_match_ID'] <- lapply(psmPairs@data['PSM_match_ID'], as.numeric)
                  
            # run model
            # pModelMax_fit <- Stage2PSM(pModelMax, psmPairs, type="lm", table_out=TRUE, opts=c())
            pModelMax_fit <- test(pModelMax, psmPairs, type="lm", table_out=TRUE, opts=c())
            

            # init coef/pval output columns

            output_base_match <- !grepl('(Intercept)|PSM_match_ID*|sector_group*|TrtBin:sector_group*', names(pModelMax_fit$standardized$coefficients))
            output_base_colnames <- names(pModelMax_fit$standardized$coefficients)[output_base_match]

            if (output_gen == 0) {                
                for (j in 1:length(output_base_colnames)) {
                    output_var <- output_base_colnames[j]
                    
                    output_data@data[i, paste('coef',output_var, sep='_')] <- -9999
                    output_data@data[i, paste('pval',output_var, sep='_')] <- -9999
                    output_data@data[i, 'coef_sector_group'] <- -9999            
                    output_data@data[i, 'pval_sector_group'] <- -9999      
                    output_data@data[i, 'coef_TrtBin.sector_group'] <- -9999            
                    output_data@data[i, 'pval_TrtBin.sector_group'] <- -9999  
                    
                }
                
                output_gen <- 1
            }
            
            

            output_match <- output_base_match | grepl(paste('sector_group',as.character(row_sector_group),'|TrtBin:sector_group',as.character(row_sector_group), sep=""), names(pModelMax_fit$standardized$coefficients))
            output_colnames <- names(pModelMax_fit$standardized$coefficients)[output_match]
            # coefficients & p values
            output_coef_list <- pModelMax_fit$standardized$coefficients[output_match]
            output_pval_list <- coef(summary(pModelMax_fit$standardized))[,4]
            
            
            if ( ! paste('TrtBin:sector_group',row_sector_group, sep="") %in% output_colnames ) {
                output_data@data[i, 'coef_TrtBin.sector_group'] <- 0
                output_data@data[i, 'pval_TrtBin.sector_group'] <- NA
            }        
            
            if ( ! paste('sector_group',row_sector_group, sep="") %in% output_colnames ) {
                output_data@data[i, 'coef_sector_group'] <- 0
                output_data@data[i, 'pval_sector_group'] <- NA               
            }
            
    
            # for each var:
            for (j in 1:length(output_colnames)) {
                output_var <- output_colnames[j]
                
                if ( grepl('TrtBin:sector_group', output_var) ) {
                    
                    if (is.na(output_coef_list[output_var]) | is.nan(output_coef_list[output_var])) {
                        output_coef_list[output_var] <- NA
                        output_pval_list[output_var] <- NA
                    }
                    # add coefficient to dataframe
                    output_data@data[i, 'coef_TrtBin.sector_group'] <- output_coef_list[output_var]
                    
                    
                    if (is.na(output_pval_list[output_var]) | is.nan(output_pval_list[output_var])) {
                        print(output_pval_list[output_var])
                        output_pval_list[output_var] <- NA
                    }
                    # add significance (p val) to dataframe
                    output_data@data[i, 'pval_TrtBin.sector_group'] <- output_pval_list[output_var]
                    
                    
                } else if ( grepl('sector_group', output_var) ) {
                    
                    if (is.na(output_coef_list[output_var]) | is.nan(output_coef_list[output_var])) {
                        output_coef_list[output_var] <- NA
                        output_pval_list[output_var] <- NA
                    }
                    # add coefficient to dataframe
                    output_data@data[i, 'coef_sector_group'] <- output_coef_list[output_var]
                    
                    
                    if (is.na(output_pval_list[output_var]) | is.nan(output_pval_list[output_var])) {
                        output_pval_list[output_var] <- NA
                    }
                    # add significance (p val) to dataframe
                    output_data@data[i, 'pval_sector_group'] <- output_pval_list[output_var]
                    
                    
                } else {
                    # print(output_var)
                    # print(output_coef_list[output_var])
                    # print(output_pval_list[output_var])
                    
                    if (is.na(output_coef_list[output_var]) | is.nan(output_coef_list[output_var])) {
                        output_coef_list[output_var] <- NA
                        output_pval_list[output_var] <- NA
                    }
                    # add coefficient to dataframe
                    output_data@data[i, paste('coef',output_var, sep='_')] <- output_coef_list[output_var]
                    
                    
                    if (is.na(output_pval_list[output_var]) | is.nan(output_pval_list[output_var])) {
                        output_pval_list[output_var] <- NA
                    }
                    # add significance (p val) to dataframe
                    output_data@data[i, paste('pval',output_var, sep='_')] <- output_pval_list[output_var]
                }
            }
            

        }
        
        gc()
       
        print(paste('end iteration:', i, '-', Sys.time()))
        sink()
    }
    
    # ======================================
    
    # write output with coefficient/pvalue data (geojson and csv)
    writeOGR(output_data, paste(output_base,'results.geojson', sep=""), "OGRGeoJSON", driver='GeoJSON')
    write.table(output_data@data, paste(output_base,'results.csv', sep=""), sep=",", row.names=FALSE, quote=TRUE)

}


