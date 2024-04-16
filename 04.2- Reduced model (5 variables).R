
# REDUCED MODEL
#  Environmental variables with % contribution / permutation importance of >10% 
# Based on training data: 
#   Bot.Temp.Max	42.3	28;    Nit.Min	21.2	12.6
#   Sst.Max	10.7	18.4;        Phos.Min	8.9	27.1
# Based on test data  
#   Bot.Temp.Max	54.9	35.3;  Nit.Min	7.2	10.7
#   Nit.Max	4.4	29.3  (Phos min 9.8)

# FINAL: Bot.Temp.max,  Nit.Min,  Nit.Max,  Sst.Max,  Phos.Min

# load required packages
library("raster")
library("dismo")
library("rgeos")
library("rJava")


occ <- read.csv("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/f.fungites.csv") 

layers_reduced <- c(1,11,12,17,20)

# List all files in the directory with .asc extension
ascii_files_reduced <- list.files("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Environmental_data/Final_reduced", 
                          pattern=".asc", full.names=TRUE)

file_names_reduced <- tools::file_path_sans_ext(basename(ascii_files_reduced))

# Create an empty list to store raster objects
raster_list_reduced <- list()

# Loop through each ASCII file, import it, and store in the raster_list
for (i in seq_along(ascii_files_reduced)) {
  raster_reduced <- raster(ascii_files_reduced[i])
  raster_list_reduced[[file_names_reduced[i]]] <- raster_reduced
}


# stack the rasters in the list
env_reduced <- stack(raster_list_reduced)

# plot the rasters
plot(env_reduced)
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/final5env.png")
dev.off()

-------------------------------------
# create background points
set.seed(2021) 
bg_reduced <- sampleRandom(x=env,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

# randomly select 75% for training
selected_reduced <- sample(1:nrow(occ), nrow(occ) * 0.75)

occ_train_reduced <- occ[selected_reduced, ]  
occ_test_reduced <- occ[-selected_reduced, ]

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p_reduced <- raster:: extract(env_reduced, occ_train_reduced[3:2])

# env conditions for testing occ
p_test_reduced <- raster:: extract(env_reduced, occ_test_reduced[3:2])

# extracting env conditions for background
a_reduced <- raster:: extract(env_reduced, bg_reduced)


# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa_reduced <- c(rep(1, nrow(p_reduced)), rep(0, nrow(a_reduced)))
pder_reduced <- as.data.frame(rbind(p_reduced, a_reduced))

pa2_reduced <- c(rep(1, nrow(p_test_reduced)), rep(0, nrow(a_reduced)))
pder2_reduced <- as.data.frame(rbind(p_test_reduced, a_reduced))


# train Maxent with tabular data
mod_reduced <- maxent(x=pder_reduced, ## env conditions
              p=pa_reduced,   ## 1:presence or 0:absence
              path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced"), 
              args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)

mod2_reduced <- maxent(x=pder2_reduced, ## env conditions
               p=pa2_reduced,   ## 1:presence or 0:absence
               path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced"), 
               args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)


# view detailed results
mod_reduced@results
mod2_reduced@results


# Histograms
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/env_histograms/bot.temp.max.hist.png")
hist(mod_reduced@presence[["Bot.Temp.Max"]], xlab = "Maximum sea water temperature at min bottom depth (°C)", main = "Bot.Temp.Max")
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/env_histograms/nit.max.hist.png")
hist(mod_reduced@presence[["Nit.Max"]], xlab= "Maximum nitrate concentrationmin at min bottom depth (micromol/m^3)", main = "Nit.Max")
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/env_histograms/nit.min.hist.png")
hist(mod_reduced@presence[["Nit.Min"]], xlab= "Minimum nitrate concentrationmin at min bottom depth (micromol/m^3)", main = "Nit.Min" )
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/env_histograms/phos.min.hist.png")
hist(mod_reduced@presence[["Phos.Min"]], xlab= "Minimum phosphate concentrationmin at min bottom depth (micromol/m^3)", main = "Phos.Min" )
dev.off()

png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/env_histograms/sst.max.hist.png")
hist(mod_reduced@presence[["Sst.Max"]], xlab= "Maximum sea surface temperature (°C)", main = "Sst.Max")
dev.off()



# plot showing importance of each variable
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites.var.contribution_reduced.png", width=8,height=8,res=360, units = "in")
plot(mod_reduced, pch = 20)
dev.off()

# project to study area
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names


png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced/fungites_continuous_train_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced/fungites_continuous_train.png", width=8,height=8,res=360, units = "in")
ped1_reduced <- predict(mod_reduced, env_reduced)   
plot(ped1_reduced)  # plot the continuous prediction
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()

# plot test data
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced/fungites_continuous_test_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced/fungites_continuous_test.png", width=8,height=8,res=360, units = "in")
ped2_reduced <- predict(mod2_reduced, env_reduced)   
plot(ped2_reduced)  # plot the continuous prediction
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# using 'training data' to evaluate p & a are dataframes
# (the p and a are the training presence and background
# points)
mod_eval_train_reduced <- dismo::evaluate(p = p_reduced, a = a_reduced, model = mod_reduced)
print(mod_eval_train_reduced) 
# AUC            : 0.9790933 
# cor            : 0.1553211 
# max TPR+TNR at : 0.3216284 

mod_eval_test_reduced <- dismo::evaluate(p = p_test_reduced, a = a_reduced, model = mod2_reduced)
print(mod_eval_test_reduced)   
# AUC            : 0.97941 
# cor            : 0.09226626 
# max TPR+TNR at : 0.2395214

#  calculate thresholds of models
thd1_reduced <- threshold(mod_eval_train_reduced, "no_omission")  # 0% omission rate 
thd2_reduced <- threshold(mod_eval_train_reduced, "spec_sens")  # highest TSS
thd3_reduced <- threshold(mod_eval_train_reduced, "equal_sens_spec")  # equal sensitivity and specificity
thd3_reduced # 0.384492

thd4_reduced <- threshold(mod_eval_test_reduced, "no_omission")  # 0% omission rate 
thd5_reduced <- threshold(mod_eval_test_reduced, "spec_sens")  # highest TSS
thd6_reduced <- threshold(mod_eval_test_reduced, "equal_sens_spec")  # equal sensitivity and specificity
thd6_reduced #  0.2395214

#plotting threshold test model equal sens/spec 
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced/fungites_threshold_test_occ_reduced.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced/fungites_threshold_test_reduced.png", width=8,height=8,res=360, units = "in")
plot(ped2_reduced >= thd6_reduced, col = c("beige","#008631"), colNA = "grey") # test model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()

# plotting threshold train model at equal sens/spec
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced/fungites_threshold_train_occ_reduced.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced/fungites_threshold_train_reduced.png", width=8,height=8,res=360, units = "in")
plot(ped1_reduced >= thd3_reduced, col = c("beige","#008631"), colNA = "grey") # train model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# Raster for training model ped1
writeRaster(ped1_reduced, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_train_reduced/trainmodelraster_ped1_reduced.asc",
            format = 'ascii',
            overwrite = T)

# Raster for test model ped2
writeRaster(ped2_reduced, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test_reduced/testmodelraster_ped2_reduced.asc",
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
