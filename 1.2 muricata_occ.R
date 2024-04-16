# Prepare Species Occurrence Data
install.packages("rgdal", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("robis", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("spThin")
install.packages("fields")

# load the packages
library(dismo)
library(maptools)
library(maps)    
library(mapdata) 
library(tidyverse)
library(CoordinateCleaner)
library(rgdal)
library(sf)
library(terra)
library(robis)
library(spThin)

# download the species occurrence records
## from GBIF
a.muricata.gbif <- dismo::gbif(
  genus = "Acropora",
  species = "muricata",
  geo = TRUE, 
  removeZeros = TRUE,
  download = TRUE
)

## from OBIS
a.muricata.obis <- robis::occurrence(
  scientificname = "Acropora muricata", 
  absence = NULL  
)



# looking at the data
nrow(a.muricata.gbif) # 3725 observations
str(a.muricata.gbif)
nrow(a.muricata.obis) # 2604 observations
str(a.muricata.obis)


# cleaning the occurrence data

## extent of the Philippines
xmin <- 114.0952145
xmax <- 126.8072562
ymin <- 4.2158064
ymax <- 21.3217806


# gbif data
a.muricata.gbif1 <- a.muricata.gbif %>%   # 31 observations
  dplyr:: select(species, lat, lon) %>%
  filter(complete.cases(.)) %>% 
  filter(lon > xmin & lon < xmax & lat > ymin & lat < ymax) %>%
  distinct()

# obis data
a.muricata.obis1 <- a.muricata.obis %>%   # 11 observations
  dplyr::rename(lat = decimalLatitude, lon = decimalLongitude) %>%
  dplyr::select(species, lat, lon) %>%
  filter(complete.cases(.)) %>% 
  filter(lon > xmin & lon < xmax & lat > ymin & lat < ymax) %>%
  distinct()

# merge filtered obis and gbif dataframes excluding duplicates
a.muricata.merged <- bind_rows(a.muricata.gbif1, a.muricata.obis1) %>%
  distinct(lon, lat, .keep_all = TRUE)  # 55 occurrences


# Using CoordinateCleaner Package
a.muricata.clean <- clean_coordinates(a.muricata.merged, lat = "lat", lon = "lon", species = "species")
# 55 occurrences

# read the shapefile
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# remove data outside the Philippines 
a.muricata.ph <- a.muricata.clean %>%    # 32 occurrences
  filter(lon > 118.5) %>%
  filter(lat != 20.766667)

# plot Philippines
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/muricata/unthin.occ.muricata.png")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names
box()
points(a.muricata.ph$lon,
       a.muricata.ph$lat,
       col = "orange",
       #pch = 21,
       pch = "+",
       bg = "orange",
       cex = 1)
dev.off()


# spatial thinning using spThin package
a.muricata.thin <- spThin::thin.algorithm(a.muricata.ph[,c(2,3)], 
                                          thin.par = 9.28, reps = 1) 
# thinning parameters based on env data resolution 5 arcmin or 9.28km
# reduced to 18 occurrences

muricata.thin.df2 <- as.data.frame(a.muricata.thin[1]) %>%
  dplyr::rename(lat = Longitude, lon = Latitude) %>%  # lat & lon interchanged by spThin
  mutate(species = "A.muricata") %>%                  # add col for species name 
  select(species, lat, lon, everything())             # rearrange cols
row.names(muricata.thin.df2) <- NULL                        # row num arranged correctly

# clean coordinates = still 18 obs
muricata.df2.clean <- clean_coordinates(muricata.thin.df2, lat = "lat", lon = "lon", species = "species")


# Check new spatially thinned occ data 

plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names
points(muricata.df2.clean$lon,
       muricata.df2.clean$lat,
       col = "red",
       pch = 21,
       cex = 1,
       bg = "red")


# save the occurrence data for maxent modeling
a.muricata <- muricata.df2.clean %>% 
  dplyr::select(species, lon, lat)

write.csv(a.muricata, "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/a.muricata.csv", row.names = FALSE)


# save plot as shp file
fungites.shp <- st_as_sf(a.muricata, coords = c("lon", "lat"))
st_write(fungites.shp, "fungites.shp")
