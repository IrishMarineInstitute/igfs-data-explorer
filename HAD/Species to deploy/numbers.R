#Species selection
mapdata=readRDS("dat-all.RDS")
head(mapdata)


combinedtotalbyspecies=aggregate(mapdata[, c("Catch_Kg", "Kg_Per_Hr")],
                                 by=list(mapdata$Species),
                                 FUN=sum,  na.rm=TRUE)

combinedtotalbyspecies[order(combinedtotalbyspecies$Catch_Kg),]


mapdataV2=readRDS("data1-all.RDS")
head(mapdataV2)

combinedtotalbyspeciesV2=aggregate(mapdataV2[, c("CatchKg", "RaisedNo")],
                                 by=list(mapdataV2$Species),
                                 FUN=sum,  na.rm=TRUE)
combinedtotalbyspeciesV2[order(combinedtotalbyspeciesV2$RaisedNo),]

