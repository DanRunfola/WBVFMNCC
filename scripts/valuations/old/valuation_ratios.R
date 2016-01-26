
carbon_path <- "/home/userz/Desktop/carbon_valuations.csv"
bio_path <-"/home/userz/Desktop/biodiversity_valuations.csv"

carbon_valuation_data <- read.csv(carbon_path, sep = ",")
biodiversity_valuation_data <- read.csv(bio_path, sep = ",")

carbon_vals <- carbon_valuation_data[,"Valuation..US.dollars."]
    
bio_vals <- (biodiversity_valuation_data[,"Valuation...US....Lower.Bound"] + biodiversity_valuation_data[,"Valuation...US....Upper.Bound"]) / 2


within_sd <- function(data, sd_val) {
    return(data[(data < mean(data) + sd_val * sd(data) & data > mean(data) - sd_val * sd(data))])
}

carbon_valuations <- within_sd(carbon_vals, 1)
biodiversity_valuations <- within_sd(bio_vals, 1)

cv <- carbon_valuations
bv <- biodiversity_valuations


cv_df <- data.frame(val=cv, type='carbon')
bv_df <- data.frame(val=bv, type='biodiversity')

val_df <- rbind(cv_df, bv_df)

library(ggplot2)
cv_hist <- ggplot(data=cv_df, aes(x=val, fill=type)) + geom_density(alpha=0.5, aes(y=..density..), fill='forestgreen', color='black') + ggtitle('Carbon Valuation Histogram') + xlab('') + ylab('')
print(cv_hist)

bv_hist <- ggplot(data=bv_df, aes(x=val, fill=type)) + geom_density(alpha=0.5, aes(y=..density..), fill='blue', color='black') + ggtitle('Biodiversity Valuation Histogram') + xlab('') + ylab('')
print(bv_hist)

hist(carbon_valuations)
hist(biodiversity_valuations)
print(hist_plot)




all <- c()
for (i in cv) {
    new <- i / bv
    all <- c(all, new)
}

# for (i in bv) {
#     new <- i / cv
#     all <- c(all, new)  
# }

all <- all[all != Inf]

ratios <- within_sd(all, 0.25)

ratios_top <- ratios[ratios > 1]
ratios_bot <- ratios[ratios < 1]

hist(ratios_top)
hist(ratios_bot)

length(ratios_top)
length(unique(ratios_top))

rt1 <- lapply(unique(ratios_top), function(x) {return(round(x,2))})

v <- sort(as.numeric(unique(rt1)))



rb <- seq(0.04,1,0.04)
rt <- seq(1.2, 6, 0.2)

ratios_final <- c(rb,rt)

