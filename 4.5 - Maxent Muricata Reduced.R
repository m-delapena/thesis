# install packages
install.packages("rgeos", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("rJava")

# load required packages
library("raster")
library("dismo")
library("rgeos")
library("rJava")

# import occurrence data
mur_occ <- read.csv("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/a.muricata.csv") 

# import environmental data
layers <- c(1,3,5,6,10,11,12,13,14,15,16,17,18,20,21)


# List all files in the directory with .asc extension
ascii_files_mur_red <- list.files("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Environmental_data/Final_Mur_Reduced", 
                          pattern=".asc", full.names=TRUE)

file_names_mur_red <- tools::file_path_sans_ext(basename(ascii_files_mur_red))

# Create an empty list to store raster objects
raster_list_mur_red <- list()

# Loop through each ASCII file, import it, and store in the raster_list
for (i in seq_along(ascii_files_mur_red)) {
  raster_mur_red <- raster(ascii_files_mur_red[i])
  raster_list_mur_red[[file_names_mur_red[i]]] <- raster_mur_red
}


# stack the rasters in the list
env_mur_red <- stack(raster_list_mur_red)
plot(env_mur_red, nc=3, nr=1)
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/mur_red_3env.png",
    width=12,height=8,res=360, units = "in")
dev.off()

# create background points
set.seed(2021) 
bg_mur_red <- sampleRandom(x=env_mur_red,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

# randomly select 75% for training
selected_mur_red <- sample(1:nrow(mur_occ), nrow(mur_occ) * 0.75)

occ_train_mur_red <- mur_occ[selected, ]  
occ_test_mur_red <- mur_occ[-selected, ]

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p_mur_red <- raster:: extract(env_mur_red, occ_train_mur_red[2:3])

# env conditions for testing occ
p_test_mur_red <- raster:: extract(env_mur_red, occ_test_mur_red[2:3])

# extracting env conditions for background
a_mur_red <- raster:: extract(env_mur_red, bg_mur_red)


# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa_mur_red <- c(rep(1, nrow(p_mur_red)), rep(0, nrow(a_mur_red)))
pder_mur_red <- as.data.frame(rbind(p_mur_red, a_mur_red))

pa2_mur_red <- c(rep(1, nrow(p_test_mur_red)), rep(0, nrow(a_mur_red)))
pder2_mur_red <- as.data.frame(rbind(p_test_mur_red, a_mur_red))


# train Maxent with tabular data
mod_mur_red <- maxent(x=pder_mur_red, ## env conditions
                  p=pa_mur_red,   ## 1:presence or 0:absence
                  path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced"), 
                  args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)

mod2_mur_red <- maxent(x=pder2_mur_red, ## env conditions
                   p=pa2_mur_red,   ## 1:presence or 0:absence
                   path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced"), 
                   args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)

# view detailed results
mod_mur_red@results
mod2_mur_red@results

# plot showing importance of each variable
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/muricata_train.var.contribution.png", width=8,height=8,res=360, units = "in")
plot(mod_mur_red, pch = 20)
dev.off()

# project to study area
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names


png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/muricata_continuous_train_occ_red.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/muricata_continuous_train_red.png", width=8,height=8,res=360, units = "in")
ped1_mur_red <- predict(mod_mur_red, env_mur_red)   
plot(ped1_mur_red)  # plot the continuous prediction
points(mur_occ$lon, mur_occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()

# plot test data
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced/muricata_continuous_test_occ_red.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced/muricata_continuous_test_red.png", width=8,height=8,res=360, units = "in")
ped2_mur_red <- predict(mod2_mur_red, env_mur_red)   
plot(ped2_mur_red)  # plot the continuous prediction
points(mur_occ$lon, mur_occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# using 'training data' to evaluate p & a are dataframes
# (the p and a are the training presence and background
# points)
mod_eval_train_mur_red <- dismo::evaluate(p = p_mur_red, a = a_mur_red, model = mod_mur_red)
print(mod_eval_train_mur_red) 
# AUC            : 0.9437708 
# cor            : 0.08953689 

mod_eval_test_mur_red <- dismo::evaluate(p = p_test_mur_red, a = a_mur_red, model = mod2_mur_red)
print(mod_eval_test_mur_red)  
# AUC            : 0.96575 
# cor            : 0.04692941

#  calculate thresholds of models
thd1_mur_red <- threshold(mod_eval_train_mur_red, "no_omission")  # 0% omission rate 
thd2_mur_red <- threshold(mod_eval_train_mur_red, "spec_sens")  # highest TSS
thd3_mur_red <- threshold(mod_eval_train_mur_red, "equal_sens_spec")  # equal sensitivity and specificity
thd3_mur_red # 0.4401478

thd4_mur_red <- threshold(mod_eval_test_mur_red, "no_omission")  # 0% omission rate 
thd5_mur_red <- threshold(mod_eval_test_mur_red, "spec_sens")  # highest TSS
thd6_mur_red <- threshold(mod_eval_test_mur_red, "equal_sens_spec")  # equal sensitivity and specificity
thd6_mur_red # 0.6924744

#plotting threshold test model equal sens/spec 
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced/muricata_thres_test_occ_red.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced/muricata_thres_test_red.png", width=8,height=8,res=360, units = "in")
plot(ped2_mur_red >= thd6_mur_red, col = c("beige","#008631"), colNA = "grey") # test model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(mur_occ$lon, mur_occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# plotting threshold train model at equal sens/spec
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/muricata_thres_train_occ_red.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/muricata_thres_train_red.png", width=8,height=8,res=360, units = "in")
plot(ped1_mur_red >= thd3_mur_red, col = c("beige","#008631"), colNA = "grey") # train model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(mur_occ$lon, mur_occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# Raster for training model ped1
writeRaster(ped1_mur_red, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_train_reduced/mur_train_ped1_red.asc",
            format = 'ascii',
            overwrite = T)

# Raster for test model ped2
writeRaster(ped2_mur_red, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/muricata_test_reduced/mur_train_ped2_red.asc",
            format = 'ascii',
            overwrite = T)




# prepPara function
prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                     responsecurves=TRUE,
                     jackknife=TRUE,      
                     outputformat="logistic",
                     outputfiletype="asc", 
                     projectionlayers=NULL,
                     randomseed=FALSE,
                     removeduplicates=TRUE,
                     betamultiplier=NULL,
                     biasfile=NULL,
                     testsamplesfile=NULL,
                     replicates=1,
                     replicatetype="crossvalidate",
                     writeplotdata=TRUE,
                     extrapolate=TRUE,
                     doclamp=TRUE,
                     beta_threshold=NULL,
                     beta_categorical=NULL,
                     beta_lqp=NULL,
                     beta_hinge=NULL,
                     applythresholdrule=NULL
){
  #20 & 29-33 features, default is autofeature
  if(is.null(userfeatures)){
    args_out <- c("autofeature")
  } else {
    args_out <- c("noautofeature")
    if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
    if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
    if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
    if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
    if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
  }
  
  #1 
  if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
  #2
  #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
  #3
  if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
  #4
  args_out <- c(args_out,paste0("outputformat=",outputformat))
  #5
  args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
  #7
  if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
  #10
  if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
  #16
  if(removeduplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
  #20 & 53-56
  # check if negative
  betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
  if(! is.null(betas) ){
    for(i in 1:length(betas)){
      if(betas[i] <0) stop("betamultiplier has to be positive")
    }
  }
  if (  !is.null(betamultiplier)  ){
    args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
  } else {
    if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
    if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
    if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
    if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
  }
  #22
  if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
  #23
  if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
  #24&25
  replicates <- as.integer(replicates)
  if(replicates>1 ){
    args_out <- c(args_out,
                  paste0("replicates=",replicates),
                  paste0("replicatetype=",replicatetype) )
  }
  #37
  if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
  #39
  if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
  #42
  if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
  #60
  if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
  
  return(args_out)
}
