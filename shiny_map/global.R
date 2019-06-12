library(dplyr)
library(rgdal)




merge.insee.with.immonotaires <- function(){
  


france <- readOGR("../data/geo/ADMIN-EXPRESS_2-0__SHP__FRA_2019-03-14/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2019-03-14/ADE_2-0_SHP_LAMB93_FR/COMMUNE.shp",
                  layer = "COMMUNE", GDAL1_integer64_policy = TRUE)

bretagne <- subset(france, france$INSEE_REG=="53")

bretagne <- spTransform(bretagne, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

data.immo <- read.csv('../all_detailed.csv')
groupy <- data.immo %>% dplyr::group_by(inseeCommune) %>% summarise(MED_PR = median(prix),MED_SQM = median(prix_m2,na.rm = T), CNT_OFFERS = n(),MED_SURF = median(surfaceHabitable),AREA_TOTAL=sum(surfaceHabitable)) %>% filter(is.finite(MED_SQMTR))

bretagne <-  merge(bretagne, groupy, by.x = "INSEE_COM", by.y = "inseeCommune")
bretagne@data$M2perHA <- bretagne@data$AREA_TOTAL/bretagne@data$POPULATION
writeOGR(obj=bretagne, dsn="bretagne_shp",layer = "COMMUNE_BRETAGNE", driver="ESRI Shapefile",overwrite_layer = T)

}

bretagne <-  readOGR("bretagne_shp/COMMUNE_BRETAGNE.shp",
                              layer = "COMMUNE_BRETAGNE", GDAL1_integer64_policy = TRUE)

data.immo <- read.csv('../all_detailed.csv')

