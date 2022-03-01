# Create an example data.frame
set.seed(65.7)
examp_df <- data.frame(x = runif(3, min = -73, max = -72.5), y = runif(3, min = 42,
                                                                       max = 43))
#prj_dd <- "EPSG:4326" US DATA
prj_dd <- "EPSG:2100" 

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "M", "L"))

examp_df2 <- data.frame(examp_df, cats)

# Create an example SpatialPoints
examp_sp <- SpatialPoints(examp_df, proj4string = CRS(prj_dd))

# Create an example SpatialPointsDataFrame
examp_spdf <- SpatialPointsDataFrame(examp_sp, data = cats)

#Using Athens
Athens <- data.frame(x=23.72686111111111, y=37.9726)
Locus <- SpatialPoints(Athens, proj4string = CRS(prj_dd))
  # IF SP doesn't include "proj4string", then use "prj=prj_dd"
  # in get_elev_point
get_elev_point(Locus, src = "epqs")

# Trying again with Athens
Athens2 <- data.frame(y=37.9726, y=23.72686111111111)
Locus2 <- SpatialPoints(Athens2, proj4string = CRS(prj_dd))
get_elev_point(Locus2, src = "epqs")

# ONCE AGAIN
Athens3 <- data.frame(x=23.72686111111111, y=37.9726)
Locus3 <- SpatialPoints(Athens3)
get_elev_point(Locus3, prj = prj_dd, src = "epqs")


# Transfrom from EPSG:4326 WGS 84 to 
# EPSG:2100 GGRS87 / Greek Grid with
# https://epsg.io/transform#s_srs=4326&t_srs=2100&x=23.7268611&y=37.9726000
# Lon (x) = 475860.51
# Lat (y) = 4202522.20

# ONCE AGAIN
library(elevatr)
library(rgdal)
Athens4 <- data.frame(x=475860.51, y=4202522.20)
Locus4 <- SpatialPoints(Athens4)
get_elev_point(Locus4, prj = prj_dd, src = "epqs")






