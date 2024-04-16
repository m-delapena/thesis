install.packages ("gridExtra")
#install.packages("raster")
#install.packages("maps")
install.packages("mapdata")
install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("sdmpredictors")

# load packages
library(raster)
library(maps)
library(mapdata)
library(maptools)
library(sdmpredictors)
library(dplyr)
library(gridExtra)

# List marine data sets
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

# Explore layers in a dataset
layers <- list_layers(datasets=c("Bio-ORACLE"), terrestrial = FALSE, marine = TRUE, version = NULL)

# Variables of interest


variables <- c("BO_sstmin","BO_sstmax",
               "BO22_tempmin_bdmin", "BO22_tempmax_bdmin",
               "BO22_nitratemin_bdmin", "BO22_nitratemax_bdmin",
               "BO22_phosphatemin_bdmin","BO22_phosphatemax_bdmin",
               "BO22_chlomin_bdmin","BO22_chlomax_bdmin",
               "BO22_dissoxmin_bdmin", "BO22_dissoxmax_bdmin",
               "BO22_salinitymin_bdmin","BO22_salinitymax_bdmin",
               "BO22_cloudmin","BO22_cloudmax",
               "BO22_parmean","BO22_parmax",
               "BO22_damin","BO22_damax"
)

# Extract present-day surface-level data sets
variables2 <- list_layers(datasets) %>%
  # select certain columns
  dplyr::select(dataset_code, layer_code, name, units, description, units, derivation, primary_type, primary_spatial_resolution, contains("cellsize"), start_year, end_year, version) %>%
  # keep variables of interest using a regular expression
  dplyr::filter(grepl(paste(variables, collapse = "|"), layer_code))

  # pH levels extracted separately due to unwanted rows
variable_ph <- list_layers(datasets) %>%
  dplyr::select(dataset_code, layer_code, name, units, description, units, derivation, primary_type, primary_spatial_resolution, contains("cellsize"), start_year, end_year, version) %>%
  dplyr::filter(grepl(paste("BO_ph", collapse = "|"), name))

  # final 10 variables with 29 total variations
variables3 <- bind_rows(variables2, variable_ph)

var.desc <- variables3[layers, c(3,4)] #18 variables and its decription
library(grid)
library(gridExtra)
png("variable_description.png", width = 500, height = 500)
grid.draw(tableGrob(var.desc))
dev.off()

# Export present-day data sets to csv file
write.csv(variables3, file =  "variables_rawdataset.csv")

options(sdmpredictors_datadir = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/rasters")
env.raster = load_layers(variables3)


# Set the path to the folder with unzipped TIF files
unzipped <- "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Environmental_data/unzipped"

# List all TIF files in the unzipped folder
tif_files <- list.files(unzipped, pattern = "\\.tif$", full.names = TRUE)


# Create an empty raster stack to store the rasters
env <- raster::stack()

# Loop through each TIF file and add it to the raster stack
for (tif_file in tif_files) {
  raster_layer <- raster::raster(tif_file)
  env <- raster::stack(env, raster_layer)
}



# crop raster
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names
phil <- extent(c(114.0952145, 126.8072562, 4.2158064, 21.3217806))
env.crop <- crop(env, phil)

plot(env.crop)

----------------------------------------------------
# visualization for paper
##plot environmental variables
envlayers6 <- c(1,2,6,9,13,15)
envlayers12 <- c(16,17,19,20,21,22)
envlayers18 <- c(23,25,26,27,30,31)

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/envlayers6.png")
plot(env.crop[[envlayers6]])
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/envlayers12.png")
plot(env.crop[[envlayers12]])
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/envlayers18.png")
plot(env.crop[[envlayers18]])
dev.off()
---------------------------------------



#occurrence points
occ <- read.csv("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/f.fungites.csv")
points(occ$lon, occ$lat, pch = "+", col="black")

# correlate environmental variables
pearson<-layerStats(env.crop,'pearson',na.rm=T)
cor.df<-as.data.frame(pearson)
colnames(cor.df) <- sub("pearson.correlation.coefficient.Present.Surface.", "", colnames(cor.df)) #shorten colnames
rownames(cor.df) <- sub("Present.Surface.", "", rownames(cor.df)) #shorten rownames
rownames(cor.df) <- sub(".BOv2_2", "", rownames(cor.df)) #shorten rownames

cor.df.mean <- data.frame(RowNames = rownames(cor.df), Mean = as.numeric(cor.df[, 22]))
cor.df.mean$Mean <- as.numeric(formatC(cor.df.mean$Mean, format = "f", digits = 3, flag = "0"))
cor.df.mean

# transfrom correlation matrix to distances
var.dist <- abs(as.dist(cor.df[,1:21])) # Only columns of variables excluding mean column

# calculate dendrogram based on distance (less distance = more correlation)
var.cluster <- hclust(1-var.dist)

png('C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Plots/correlation_clusters.png',width=8,height=6,res=360, units = "in")
plot(var.cluster, main = NULL,xlab="Environmental Variables",ylab = "Height",sub="")
abline(h=0.25, lty=2, lwd=2) # variables that have a correlation < 0.75
dev.off()


# Final 15 environmental variables based on correlation:
#   Bot Temp max - 1, Chlo max - 3, Cloud max - 5, Da max - 6, Dissox min - 10
#   Nit max - 11, Nit min - 12, PAR max - 13, PAR mean - 14, pH - 15, Phos max - 16, Phos min - 17, Sal max - 18, Sst max - 20, Sst min - 21

#Final 5 (after training/testing): 1,11,12,17,20 

# choose layers based on correlation graph and save raster
var.cluster$labels # variable names and column number


# Define the layers you want to keep
layers <- c(1,3,5,6,10,11,12,13,14,15,16,17,18,20,21)

# Loop through each layer, convert to ASCII format, and save with original file names
for (i in 1:nlayers(env.crop[[layers]])) {
  layer_name <- names(env.crop[[layers]])[i]
  output_file <- file.path("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Environmental_data/Final", paste0(layer_name, ".asc"))
  writeRaster(env.crop[[layers]][[i]], filename=output_file, format="ascii", overwrite=TRUE)
}




