library(maptools)
library(sp)

test <- readShapePoly("/home/userz/Desktop/test.shp")
out <- test

for (i in 2:6) {
    tmp <- spChFIDs(test, as.character( ((i-1)*nrow(test)+1):(i*nrow(test)) ))
    out <- spRbind(out, tmp)
}

drops <- c()
for (i in 60:199) {
    drops <- c(drops, paste("V",i,sep=""))
}
outx <- out[,!(names(out) %in% drops)]

writePolyShape(outx, "/home/userz/Desktop/test_out.shp")


# ----------------------------------------------------------



t1 <- readShapePoly("/home/userz/Desktop/test_out.shp")


rb <- seq(0.04,1,0.04)
rt <- seq(1.2, 6, 0.2)
ratios_final <- lapply(c(rb,rt), function(x){ return(sprintf('%2.2f',x)) })


t1 <- t1[,(names(t1) %in% c('SP_ID','sector','commitment'))]


sg <- lapply(runif(nrow(t1), 0, 6), function(x){return(LETTERS[ceiling(x)])})
cg <- lapply(runif(nrow(t1), 0, 3), function(x){
    cg_in <- ceiling(x)
    if (cg_in == 1) {
        cg_out <- 'low_trtbin'
    } else if (cg_in == 2) {
        cg_out <- 'medium_trtbin'
    } else {
        cg_out <- 'high_trtbin'
    }
    return(cg_out)
})

t1$sector_group <- as.character(sg)
t1$commitment_group <- as.character(cg)


for (i in 1:length(ratios_final)) {
    cname <- paste("r_",gsub("\\.", "-", ratios_final[i]), sep="")
    print(cname)
    t1[[cname]] <- runif(nrow(t1), 0, 100)
}

writePolyShape(t1, "/home/userz/Desktop/test_fill.shp")









