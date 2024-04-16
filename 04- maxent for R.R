# install packages
install.packages("rgeos", repos = "https://packagemanager.posit.co/cran/2023-10-13")
install.packages("rJava")

# load required packages
library("raster")
library("dismo")
library("rgeos")
library("rJava")

# import occurrence data
occ <- read.csv("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Occurrence_data/f.fungites.csv") 

# import environmental data
layers <- c(1,3,5,6,10,11,12,13,14,15,16,17,18,20,21)


# List all files in the directory with .asc extension
ascii_files <- list.files("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Data/Environmental_data/Final", 
                          pattern=".asc", full.names=TRUE)

file_names <- tools::file_path_sans_ext(basename(ascii_files))

# Create an empty list to store raster objects
raster_list <- list()

# Loop through each ASCII file, import it, and store in the raster_list
for (i in seq_along(ascii_files)) {
  raster <- raster(ascii_files[i])
  raster_list[[file_names[i]]] <- raster
}


# stack the rasters in the list
env <- stack(raster_list)

# plot the rasters
plot(env)
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/final_15_env.png")
dev.off()

# create background points
set.seed(2021) 
bg <- sampleRandom(x=env,
                   size=10000,
                   na.rm=T, #removes the 'Not Applicable' points  
                   sp=T) # return spatial points 

# randomly select 75% for training
selected <- sample(1:nrow(occ), nrow(occ) * 0.75)

occ_train <- occ[selected, ]  
occ_test <- occ[-selected, ]

# extracting env conditions for training occ from the raster
# stack; a data frame is returned (i.e multiple columns)
p <- raster:: extract(env, occ_train[3:2])

# env conditions for testing occ
p_test <- raster:: extract(env, occ_test[3:2])

# extracting env conditions for background
a <- raster:: extract(env, bg)


# repeat the number 1 as many numbers as the number of rows
# in p, and repeat 0 as the rows of background points
pa <- c(rep(1, nrow(p)), rep(0, nrow(a)))
pder <- as.data.frame(rbind(p, a))

pa2 <- c(rep(1, nrow(p_test)), rep(0, nrow(a)))
pder2 <- as.data.frame(rbind(p_test, a))


# train Maxent with tabular data
mod <- maxent(x=pder, ## env conditions
              p=pa,   ## 1:presence or 0:absence
              path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia"), 
              args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)

mod2 <- maxent(x=pder2, ## env conditions
               p=pa2,   ## 1:presence or 0:absence
               path=paste0("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test"), 
               args=prepPara(userfeatures= "LQPTH", jackknife=TRUE, outputfiletype = "asc")
)

# view detailed results
mod@results
mod2@results

# plot showing importance of each variable
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites.var.contribution.png", width=8,height=8,res=360, units = "in")
plot(mod, pch = 20)
dev.off()

# project to study area
world <-maptools:: readShapeSpatial("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/PHIL-Shape_Files/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")
plot(world[world$ADMIN=="Philippines",1])  # Column 1 is the id column, column "ADMIN" contains the country names


png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites_continuousmap_train_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites_continuousmap_train.png", width=8,height=8,res=360, units = "in")
ped1 <- predict(mod, env)   
plot(ped1)  # plot the continuous prediction
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()

# plot test data
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test/fungites_continuousmap_test_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test/fungites_continuousmap_test.png", width=8,height=8,res=360, units = "in")
ped2 <- predict(mod2, env)   
plot(ped2)  # plot the continuous prediction
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# using 'training data' to evaluate p & a are dataframes
# (the p and a are the training presence and background
# points)
mod_eval_train <- dismo::evaluate(p = p, a = a, model = mod)
print(mod_eval_train) # AUC 0.98237 , cor 0.1684914 , max TPR+TNR at 0.1562601

mod_eval_test <- dismo::evaluate(p = p_test, a = a, model = mod2)
print(mod_eval_test)  # AUC  0.97672 , cor 0.08755682 , max TPR+TNR at 0.3931433 


#  calculate thresholds of models
thd1 <- threshold(mod_eval_train, "no_omission")  # 0% omission rate 
thd2 <- threshold(mod_eval_train, "spec_sens")  # highest TSS
thd3 <- threshold(mod_eval_train, "equal_sens_spec")  # equal sensitivity and specificity
thd3 # 0.3801335

thd4 <- threshold(mod_eval_test, "no_omission")  # 0% omission rate 
thd5 <- threshold(mod_eval_test, "spec_sens")  # highest TSS
thd6 <- threshold(mod_eval_test, "equal_sens_spec")  # equal sensitivity and specificity
thd6 # 0.836443

#plotting threshold test model equal sens/spec 
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test/fungites_threshold_test_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia_test/fungites_threshold_test.png", width=8,height=8,res=360, units = "in")
plot(ped2 >= thd6, col = c("beige","#008631"), colNA = "grey") # test model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()

# plotting threshold train model at equal sens/spec
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites_threshold_train_occ.png", width=8,height=8,res=360, units = "in")
png("C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/fungites_threshold_train.png", width=8,height=8,res=360, units = "in")
plot(ped1 >= thd3, col = c("beige","#008631"), colNA = "grey") # train model at thresholds for equal sensitivity and specificity
legend("topright", border = NULL, legend=c("absence", "presence"), box.lwd = NA, fill= c("beige","#008631"))
points(occ$lon, occ$lat, pch = 21, col="black", lwd = 2, bg = "grey", cex = 1.5)
dev.off()


# Raster for training model ped1
writeRaster(ped1, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/trainmodelraster_ped1.asc",
            format = 'ascii',
            overwrite = T)

# Raster for test model ped2
writeRaster(ped2, bylayer=TRUE, 
            filename = "C:/Users/lenovo/OneDrive/Desktop/thesis/thesis-modeling/Outputs/fungia/testmodelraster_ped2.asc",
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
