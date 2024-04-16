# Prepare Species Occurrence Data

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
f.fungites.gbif <- dismo::gbif(
  genus = "Fungia",
  species = "fungites",
  geo = TRUE, 
  removeZeros = TRUE,
  download = TRUE
)

## from OBIS
f.fungites.obis <- robis::occurrence(
  scientificname = "Fungia fungites", 
  absence = NULL  
)



# looking at the data
nrow(f.fungites.gbif) #2933 observations
str(f.fungites.gbif)
nrow(f.fungites.obis) #1543 observations
str(f.fungites.obis)


# cleaning the occurrence data

## extent of the Philippines
xmin <- 114.0952145
xmax <- 126.8072562
ymin <- 4.2158064
ymax <- 21.3217806


# gbif data
f.fungites.gbif1 <- f.fungites.gbif %>%   # 31 observations
  dplyr:: select(species, lat, lon) %>%
  filter(complete.cases(.)) %>% 
  filter(lon > xmin & lon < xmax & lat > ymin & lat < ymax) %>%
  distinct()

# obis data
f.fungites.obis1 <- f.fungites.obis %>%   # 11 observations
  dplyr::rename(lat = decimalLatitude, lon = decimalLongitude) %>%
  dplyr::select(species, lat, lon) %>%
  filter(complete.cases(.)) %>% 
  filter(lon > xmin & lon < xmax & lat > ymin & lat < ymax) %>%
  distinct()

# merge filtered obis and gbif dataframes excluding duplicates
f.fungites.merged <- bind_rows(f.fungites.gbif1, f.fungites.obis1) %>%
  distinct(lon, lat, .keep_all = TRUE)  # 40 occurrences


# Using CoordinateCleaner Package
f.fungites.clean <- clean_coordinates(f.fungites.merged, lat = "lat", lon = "lon", species = "species")
# 40 occurrences

#  download the data in your working directory, and then unzip it
if(!dir.exists("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Shape_Files")){
  dir.create("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Shape_Files", recursive = TRUE)
}

# shapefile manually transferred from downloaded folder to Data/Shape_file
unzip("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Shape_Files/ne_10m_admin_0_countries.zip",exdir="C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Shape_Files")

# read the shapefile
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# remove data outside the Philippines 
f.fungites.ph <- f.fungites.clean %>%    # 38 occurrences
  filter(lon > 118.5) %>%
  filter(lat != 20.766667)

# plot Philippines
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/unthin.occ.fungia.png")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names
box()
points(f.fungites.ph$lon,
       f.fungites.ph$lat,
       col = "orange",
       #pch = 21,
       pch = "+",
       bg = "orange",
       cex = 1)
dev.off()


# spatial thinning using spThin package
f.fungites.thin <- spThin::thin.algorithm(f.fungites.ph[,c(2,3)], 
                                          thin.par = 9.28, reps = 1) 
# thinning parameters based on env data resolution 5 arcmin or 9.28km
# reduced to 20 occurrences

ff.thin.df2 <- as.data.frame(f.fungites.thin[1]) %>%
  dplyr::rename(lat = Longitude, lon = Latitude) %>%  # lat & lon interchanged by spThin
  mutate(species = "F.fungites") %>%                  # add col for species name 
  select(species, lat, lon, everything())             # rearrange cols
row.names(ff.thin.df2) <- NULL                        # row num arranged correctly

# clean coordinates
ff.df2.clean <- clean_coordinates(ff.thin.df2, lat = "lat", lon = "lon", species = "species")


# Check new spatially thinned occ data 

plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names
points(ff.df2.clean$lon,
       ff.df2.clean$lat,
       col = "red",
       pch = 21,
       cex = 1,
       bg = "red")


# save the occurrence data for maxent modeling
f.fungites <- ff.df2.clean %>% 
  dplyr::select(species, lon, lat)

write.csv(f.fungites, "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/f.fungites.csv", row.names = FALSE)


# save plot as shp file
fungites.shp <- st_as_sf(f.fungites, coords = c("lon", "lat"))
st_write(fungites.shp, "fungites.shp")
