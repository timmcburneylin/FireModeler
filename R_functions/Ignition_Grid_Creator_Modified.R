#testing:
#fire_data<-st_read(paste0(path,spatial_new,"Fire_History.shp"))
#fire_data = fire_data
#indicator_stack = Ignition_inputs
#reference_grid=rast(FT)
#indicators_1=c("DEM","roadDist","roadDensity","recDist","transmissionDist","structureDensity","wuiDist","TPI","TRI","fuels")
#indicators_2=c("DEM","Aspect","Slope","SolarRad","TPI","fuels")
#causes = c("H","L")
#season_description = c("1","2")
#min_fire_size = 0.01
#model = "rf"
#non_fuel_vals = c(13,19)
#output_location = paste0(path,spatial_final)
#Ignition Grid Function Modified
ign_grid_Mod <- function(fire_data,
                     indicator_stack,
                     reference_grid,
                     indicators_1,
                     indicators_2,
                     causes,
                     season_description,
                     output_location,
                     min_fire_size = "",
                     model = "",
                     factor_vars = NULL,
                     non_fuel_vals = c(99,102),
                     testing = F,
                     Minimum_Num_Ignitions,
                     stratify_by_season,
                     stratify_by_cause){
#beginning of function bracket----------------------------------------------------------------------------------------------------
  
  # Initialize an empty list to store the ign rasters
  ign_raster_list <- list()
  
  #
  if ( grepl("SpatRaster", class(reference_grid)) ) { grast <- reference_grid }
  if ( grepl("character", class(reference_grid)) ) { grast <- terra::rast(reference_grid) }
  if ( !grepl("SpatRaster|character", class(reference_grid)) ) { 
    message("Reference Grid must be the directory of the raster or a raster object.") }
  
  if (model == "") {
    stop("Please define a model. Either rf for an automatically refined random forest, rf_stock for a baseline randomForest::randomForest, gbm for gradient boosting machine or brt for a boosted regression tree.")}
  
  if (min_fire_size != "") {fire_data <- fire_data[which(fire_data$SizeHA > min_fire_size),]}
  season_description <- c(season_description,"All")
  indicators_1 <- tolower(indicators_1)
  indicators_2 <- tolower(indicators_2)
  
  #check ignition  distribution and determine if fire cause or season stratification is needed to occur
  #------------------------------------------------------------------------------
  
  #Decision on Stratification by season or Cause:
  #stratify_by_season <- readline("Stratify by season? (YES/NO): ")
  #stratify_by_cause <- readline("Stratify by cause? (YES/NO): ")
  
  #Season Filtering
  #fire_data<-fire_data %>% filter(!is.na(Season))
  valid_seasons <- sort(unique(na.omit(fire_data$Season)))
  all_seasons <- c(valid_seasons, max(valid_seasons) + 1)
  
  #Causes:CHECK
  causes<-causes
non_fuel_vals<-c(99,102)
testing=F
factor_vars=NULL
      # Adjust flow based on user input
  if (toupper(stratify_by_season) == "YES" & toupper(stratify_by_cause) == "YES"){
    message("Stratifying by both season and cause...")
    # Random Forest -----------------------------------------------------------
    if (model == "rf") {
      for (cause in causes) {
        for (season in all_seasons) {
          if (season == max(all_seasons)) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          } else {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause & fire_data$Season == season,]))
          }
          
          #Changed this section of code to edit data frame correctly: ie stack indicator and pres_abs(as a raster, not SpatRaster)
          print(paste0("Starting ",cause," - ",season_description[season]))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- stack(indicator_stack,raster(pres_abs))
          names(modelling_stack) <- tolower(names(modelling_stack))
          #plot(modelling_stack$fuels)
          ###And changed this, replacing data table funciton with data frame to data table
          # build ign dataframe: 
          #data <- data.table::as.data.table(modelling_stack)
          # Convert SpatRaster to a data frame (cell values)
          modelling_stack<-rast(modelling_stack)
          data_df <- terra::as.data.frame(modelling_stack, xy = TRUE)
          # Convert the data frame to a data.table
          data <- data.table::as.data.table(data_df)
          #set ignitions to a factor
          data$ign<- as.factor(data$ign)
          
          
          ####
          
          data <- data[complete.cases(data)] # remove NA instances
          if (!is.null(non_fuel_vals)) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
          if (!is.null(factor_vars)) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          #change this section to use the rose function to oversample the data and equalize classes
          #data_mod <- caret::downSample(x = data[,-"ign"],
           #                            y = data$ign,
            #                          yname = "ign")#, replace=FALSE)
          # Oversample or undersample the datA
          data_mod <- ROSE::ovun.sample(ign ~ ., data = data, method = "under")$data
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .70)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          if(nrow(data_train)<50 | nrow(data_test) == 0){
          #downsample to more managable size e.g: 50,000, only sampling from the 0s to retain all 1s
            data_sampled <- bind_rows(
              data %>% dplyr::filter(ign == "0") %>% sample_n(1000),
              data %>% dplyr::filter(ign == "1")
            )
            
    
            
              #oversample
            data_mod <- ROSE::ovun.sample(ign ~ ., data = data_sampled, method = "over")$data
            dat_part <- caret::createDataPartition(y = data_mod$ign,p = .70)[[1]]
            data_train <- data_mod[dat_part,]
            data_test <- data_mod[-dat_part,]
          }
          
          
          repeat {
            
            if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
            if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}
            
            control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv",number = 5,repeats = 2)
            results <- caret::rfe(x = predictors[,-1],y = predictors[,"ign"],sizes = c(1:length(predictors)),rfeControl = control, p = 1, metric = "Accuracy")
            
            if ( cause == "L" && max(results$results$Accuracy) > 0.50 ) { break }
            if ( cause == "H" && max(results$results$Accuracy) > 0.60 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
            
          }
          
          predictors <- predictors[,c("ign",results$optVariables)]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          trControl <- caret::trainControl(method = "cv",
                                           number = 10,
                                           search = "grid")
          # Run the model
          rf_default <- caret::train(ign~.,
                                     data = predictors,
                                     method = "rf",
                                     metric = "Accuracy",
                                     trControl = trControl)
          # Print the results
          print(rf_default)
          
          tuneGrid <- expand.grid(.mtry = c(1:10))
          rf_mtry <- caret::train(ign~.,
                                  data = predictors,
                                  method = "rf",
                                  metric = "Accuracy",
                                  tuneGrid = tuneGrid,
                                  trControl = trControl,
                                  importance = TRUE,
                                  nodesize = 14,
                                  ntree = 300)
          print(rf_mtry)
          best_mtry <- rf_mtry$bestTune$mtry
          mtry <- best_mtry
          
          store_maxnode <- list()
          tuneGrid <- expand.grid(.mtry = best_mtry)
          for (maxnodes in c(5:15)) {
            rf_maxnode <- caret::train(ign~.,
                                       data = predictors,
                                       method = "rf",
                                       metric = "Accuracy",
                                       tuneGrid = tuneGrid,
                                       trControl = trControl,
                                       importance = TRUE,
                                       nodesize = 14,
                                       maxnodes = maxnodes,
                                       ntree = 300)
            current_iteration <- toString(maxnodes)
            store_maxnode[[current_iteration]] <- rf_maxnode
          }
          
          results_node <- caret::resamples(store_maxnode)
          x <- summary(results_node)
          print(rf_maxnode)
          if (x$statistics$Accuracy[,"Mean"][length(x$statistics$Accuracy[,"Mean"])]/x$statistics$Accuracy[,"Mean"][1] > 1) {
            
            store_maxnode <- list()
            tuneGrid <- expand.grid(.mtry = best_mtry)
            for (maxnodes in c(20:30)) {
              rf_maxnode <- caret::train(ign~.,
                                         data = predictors,
                                         method = "rf",
                                         metric = "Accuracy",
                                         tuneGrid = tuneGrid,
                                         trControl = trControl,
                                         importance = TRUE,
                                         nodesize = 14,
                                         maxnodes = maxnodes,
                                         ntree = 300)
              key <- toString(maxnodes)
              store_maxnode[[key]] <- rf_maxnode
            }
            results_node <- caret::resamples(store_maxnode)
            summary(results_node)
          }
          x <- summary(results_node)
          maxnodes <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
          
          store_maxtrees <- list()
          for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
            rf_maxtrees <- caret::train(ign~.,
                                        data = predictors,
                                        method = "rf",
                                        metric = "Accuracy",
                                        tuneGrid = tuneGrid,
                                        trControl = trControl,
                                        importance = TRUE,
                                        nodesize = 14,
                                        maxnodes = maxnodes,
                                        ntree = ntree)
            key <- toString(ntree)
            store_maxtrees[[key]] <- rf_maxtrees
          }
          results_tree <- caret::resamples(store_maxtrees)
          x <- summary(results_tree)
          ntrees <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
          
          fit_rf <- caret::train(ign~.,
                                 data = predictors,
                                 method = "rf",
                                 metric = "Accuracy",
                                 tuneGrid = tuneGrid,
                                 trControl = trControl,
                                 importance = TRUE,
                                 nodesize = 14,
                                 maxnodes = maxnodes,
                                 ntree = ntrees)
          
          # model diagnostics
          print(fit_rf)
          
          # model variable randomForest::importance
          
          # predict probability of pres (1) or abs (0)
          prediction <- predict(fit_rf,
                                data_test)
          print(varImp(fit_rf,useModel = T,scale = F))
          
          write(x = paste0(cause," ",
                           season_description[season]," ",
                           results$optVariables," ",
                           varImp(fit_rf,useModel = T,scale = F)),
                file = paste0(output_location,
                              "RF_Var_Imp.txt"),
                append = T,
                sep = "\n",
                ncolumns = 1
          )
          
          caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"]
          names(indicator_stack)<- tolower(names(indicator_stack))
          #change the indicator stack back to a Spatraster and add in fit_rf ( your tuned forest), rather then rf_default (your default forest)
          ign <- terra::predict(object = rast(indicator_stack),
                                model = fit_rf,
                                type = "prob",
                                fun = predict,
                                na.rm=T)[[2]]
          
          #sets values equal to zero for fuels which are water or nf based on the fbp codes, make sure fuel is loaded in raster
          ign[][which(indicator_stack$fuels[] %in% c(102,99))] <- 0

                    terra::writeRaster(raster(ign),
                             paste0(output_location,
                                    paste(cause,
                                          season_description[season],
                                          ifelse(min_fire_size != "",
                                                 paste0("ign_randomforest_auto_over_",
                                                        min_fire_size,
                                                        ".tif"),
                                                 "ign_randomforest_auto.tif"),
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          # Add to the raster list
          ign_raster_list[[paste0(cause, "_", season)]] <- raster(ign)
          
          write(x = c(cause,
                      season_description[season],
                      results$optVariables,
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
                file = paste0(output_location,
                              "RF_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    season,
                                    results$optVariables,
                                    caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])))
        }
      }
    }
    
    # Gradient Boosting Machine -----------------------------------------------
    if (model == "gbm") {
      for (cause in causes) {
        for (season in min(unique(fire_data$Season)):(max(unique(fire_data$Season)) + 1)) {
          if (season == max(unique(fire_data$Season)) + 1) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          } else {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause & fire_data$Season == season,]))
          }
          
          print(paste0("Starting ",cause," - ",season_description[season]))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data)] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          if (cause == causes[1]) {predictors <- indicators_1}
          if (cause == causes[2]) {predictors <- indicators_2}
          data_train <- data_train[,c("ign",predictors)]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          gbm_step <- dismo::gbm.step(data_train,
                                      gbm.y = 1,
                                      #using the same subset of response variables
                                      gbm.x = c(2:ncol(data_train)),
                                      #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                      tree.complexity = 3,
                                      n.folds = 10,
                                      #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                      learning.rate = 0.005,
                                      #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                      family = "bernoulli",
                                      max.trees = 2000,
                                      bag.fraction = 0.5)
          
          data_train <- data_train[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
          data_test <- data_test[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
          
          gbm_step <- dismo::gbm.step(data_train,
                                      gbm.y = 1,
                                      #using the same subset of response variables
                                      gbm.x = c(2:ncol(data_train)),
                                      #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                      tree.complexity = 3,
                                      n.folds = 10,
                                      #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                      learning.rate = 0.005,
                                      #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                      family = "bernoulli",
                                      max.trees = 2000,
                                      bag.fraction = 0.5)
          
          summary(gbm_step)
          
          predictions <- predict(gbm_step,
                                 data_test,
                                 n.trees = gbm_step$n.trees,
                                 type = "response")
          
          gbm_testing <- prediction(predictions, data_test$ign)
          roc_test <- performance(gbm_testing, "tpr","fpr")
          plot(roc_test)
          gbm_auc <- as.numeric(performance(gbm_testing, "auc")@y.values)
          print(gbm_auc)
          
          
          ign <- terra::predict(indicator_stack,
                                gbm_step,
                                n.trees = gbm_step$n.trees,
                                type = "response")
          
          scaled_ign <- (ign - global(ign, fun = 'min')) / (global(ign, fun = 'max') - global(ign, fun = 'min'))
          
          scaled_ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          write(x = c(cause,
                      season_description[season],
                      results$optVariables,
                      paste0("Area Under the Curve: ",gbm_auc),
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(as.factor(ifelse(predictions > 0.8, 1, 0)),as.factor(data_test$ign))$byClass['Balanced Accuracy'])),
                file = paste0(output_location,
                              "GBM_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    season,
                                    results$optVariables,
                                    gbm_auc)
                )
          )
          
          terra::writeRaster(scaled_ign,
                             paste0(output_location,
                                    paste(cause,
                                          season_description[season],
                                          ifelse(min_fire_size != "",
                                                 paste0("ign_gbmstep_over_",
                                                        min_fire_size,
                                                        ".tif"),
                                                 "ign_gbmstep.tif"),
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2"))) # write raster
        }
      }
    }
    
    # Random Forest Stock -----------------------------------------------------
    if (model == "rf_stock") {
      for (cause in causes) {
        for (season in min(unique(fire_data$Season)):(max(unique(fire_data$Season)) + 1)) {
          if (season == max(unique(fire_data$Season)) + 1) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          } else {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause & fire_data$Season == season,]))
          }
          
          print(paste0("Starting ",cause," - ",season_description[season]))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data),] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          repeat {
            
            if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
            if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}
            
            control <- caret::rfeControl(functions = caret::rfFuncs,
                                         method = "cv",
                                         number = 10,
                                         repeats = 2)
            results <- caret::rfe(x = predictors[-1],
                                  y = predictors[,"ign"],
                                  sizes = c(1:length(predictors)),
                                  rfeControl = control,
                                  p = 1,
                                  metric = "Accuracy")
            print(c("Results for ", paste0("Starting ",cause," - ",season_description[season]), "\n",results))
            
            if ( cause == "L" && max(results$results$Accuracy) > 0.65 ) { break }
            if ( cause == "H" && max(results$results$Accuracy) > 0.70 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
            
          }
          
          predictors <- predictors[,c("ign",results$optVariables)]
          
          best_mtry <- randomForest::tuneRF(predictors[-1],
                                            predictors$ign,
                                            ntree = 1500,
                                            stepFactor = 1.5,
                                            improve = 0.01,
                                            trace = TRUE,
                                            plot = TRUE)
          best_mtry <- best_mtry[which.min(best_mtry[,2]),1]
          
          rf <- randomForest::randomForest(ign ~ .,
                                           data = predictors,
                                           ntree = 1500,
                                           mtry = best_mtry,
                                           importance = TRUE,
                                           response.type = "binary")
          
          # model variable randomForest::importance
          imp <- randomForest::importance(rf)
          
          # predict probability of pres (1) or abs (0)
          prediction <- predict(rf, data_test)
          
          print(caret::confusionMatrix(prediction,
                                       data_test$ign)$byClass["Balanced Accuracy"])
          
          # build/name ign raster
          ign <- terra::predict(model = rf,
                                object = indicator_stack,
                                type = "prob")[[2]]
          
          x11()
          plot(ign)
          
          ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          write(x = c(cause,
                      season_description[season],
                      paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
                file = paste0(output_location,
                              "RF_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    season,
                                    paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                                    caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
                )
          )
          
          terra::writeRaster(ign,
                             paste0(output_location,
                                    paste(cause,
                                          season_description[season],
                                          "ign_randomforest.tif",
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          
        }
      }
    }
    
    # Boosted Regression Tree -------------------------------------------------
    if (model == "brt") {
      for (cause in causes) {
        for (season in min(unique(fire_data$Season)):(max(unique(fire_data$Season)) + 1)) {
          if (season == max(unique(fire_data$Season)) + 1) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          } else {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause & fire_data$Season == season,]))
          }
          
          print(paste0("Starting ",cause," - ",season_description[season]))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data),] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          data_train$ign <- as.integer(as.character(data_train$ign))
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)] }
          if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)] }
          
          brt <- dismo::gbm.step(data = predictors,
                                 gbm.x = 2:length(predictors),
                                 gbm.y = 1,
                                 tree.complexity = 3,
                                 family = "bernoulli",
                                 n.folds = 20,
                                 n.trees = 500,
                                 step.size = 50,
                                 max.trees = 7500,
                                 learning.rate = 0.0005)
          
          brt <- dismo::gbm.step(data = predictors,
                                 gbm.x = which(names(predictors) %in% brt$contributions[brt$contributions$rel.inf >= 1.0, "var"]),
                                 gbm.y = 1,
                                 tree.complexity = 3,
                                 family = "bernoulli",
                                 n.folds = 20,
                                 n.trees = 500,
                                 step.size = 50,
                                 max.trees = 7500,
                                 learning.rate = 0.0005,
                                 plot.main = T)
          
          # model variable randomForest::importance
          imp <- summary(brt)
          
          # build/name ign raster
          ign <- terra::predict(model = brt,
                                object = indicator_stack,
                                type = "response",
                                n.trees = brt$n.trees,
                                na.rm = T)
          
          ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          ign <- (ign - min(ign[],na.rm = T))/(max(ign[],na.rm = T) - min(ign[],na.rm = T))
          
          write(x = c(cause,
                      season_description[season],
                      paste("CV AUC: ",round(mean(brt$cv.roc.matrix),2)),
                      paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": ")
          ),
          file = paste0(output_location,
                        "BRT_Model_Inputs.txt"),
          append = T,
          sep = "\t",
          ncolumns = length(c(cause,
                              season,
                              paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": "),
                              mean(brt$cv.roc.matrix))
          )
          )
          
          terra::writeRaster(ign,
                             paste0(output_location,
                                    paste(cause,
                                          season_description[season],
                                          "ign_boostedregressiontree.tif",
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          
        }
      }
    }
    
    #export grids
    return(ign_raster_list)
    
} 
  else if (toupper(stratify_by_season) == "NO" & toupper(stratify_by_cause) == "YES"){
    message("Stratifying by cause only...")
#------------------------
#Stratify by Only Cause   -----------------------------------------------------------------------------------------------------------------
#------------------------  
  
    # Random Forest -----------------------------------------------------------
    if (model == "rf") {
      for (cause in causes) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          
          #Changed this section of code to edit data frame correctly: ie stack indicator and pres_abs(as a raster, not SpatRaster)
          print(paste0("Starting ",cause," - ","ALL Seasons"))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- stack(indicator_stack,raster(pres_abs))
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          ###And changed this, replacing data table funciton with data frame to data table
          # build ign dataframe: 
          #data <- data.table::as.data.table(modelling_stack)
          # Convert SpatRaster to a data frame (cell values)
          data_df <- terra::as.data.frame(modelling_stack, xy = TRUE)
          # Convert the data frame to a data.table
          data <- data.table::as.data.table(data_df)
          #set ignitions to a factor
          data$ign<- as.factor(data$ign)
          
          
          ####
          data <- data[complete.cases(data)] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          #change this section to use the rose function to oversample the data and equalize classes
          #data_mod <- caret::downSample(x = data[,-"ign"],
           #                             y = data$ign,
            #                            yname = "ign", replace=TRUE)
          # Oversample or undersample the data
          
          data_mod <- ROSE::ovun.sample(ign ~ ., data = data, method = "under")$data
        
          #
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          
          repeat {
            if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
            if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}
            
            control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv",number = 10,repeats = 2)
            results <- caret::rfe(x = predictors[-1],y = predictors[,"ign"],sizes = c(1:length(predictors)),rfeControl = control, p = 1, metric = "Accuracy")
            print(results)
            
            if ( cause == "L" && max(results$results$Accuracy) > 0.50 ) { break }
            if ( cause == "H" && max(results$results$Accuracy) > 0.65 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
            
          }
          
          predictors <- predictors[,c("ign",results$optVariables)]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          trControl <- caret::trainControl(method = "cv",
                                           number = 10,
                                           search = "grid")
          # Run the model
          rf_default <- caret::train(ign~.,
                                     data = predictors,
                                     method = "rf",
                                     metric = "Accuracy",
                                     trControl = trControl)
          # Print the results
          print(rf_default)
          
          tuneGrid <- expand.grid(.mtry = c(1:10))
          rf_mtry <- caret::train(ign~.,
                                  data = predictors,
                                  method = "rf",
                                  metric = "Accuracy",
                                  tuneGrid = tuneGrid,
                                  trControl = trControl,
                                  importance = TRUE,
                                  nodesize = 14,
                                  ntree = 300)
          print(rf_mtry)
          best_mtry <- rf_mtry$bestTune$mtry
          mtry <- best_mtry
          
          store_maxnode <- list()
          tuneGrid <- expand.grid(.mtry = best_mtry)
          for (maxnodes in c(5:15)) {
            rf_maxnode <- caret::train(ign~.,
                                       data = predictors,
                                       method = "rf",
                                       metric = "Accuracy",
                                       tuneGrid = tuneGrid,
                                       trControl = trControl,
                                       importance = TRUE,
                                       nodesize = 14,
                                       maxnodes = maxnodes,
                                       ntree = 300)
            current_iteration <- toString(maxnodes)
            store_maxnode[[current_iteration]] <- rf_maxnode
          }
          results_node <- caret::resamples(store_maxnode)
          x <- summary(results_node)
          print(rf_maxnode)
          if (x$statistics$Accuracy[,"Mean"][length(x$statistics$Accuracy[,"Mean"])]/x$statistics$Accuracy[,"Mean"][1] > 1) {
            
            store_maxnode <- list()
            tuneGrid <- expand.grid(.mtry = best_mtry)
            for (maxnodes in c(20:30)) {
              rf_maxnode <- caret::train(ign~.,
                                         data = predictors,
                                         method = "rf",
                                         metric = "Accuracy",
                                         tuneGrid = tuneGrid,
                                         trControl = trControl,
                                         importance = TRUE,
                                         nodesize = 14,
                                         maxnodes = maxnodes,
                                         ntree = 300)
              key <- toString(maxnodes)
              store_maxnode[[key]] <- rf_maxnode
            }
            results_node <- caret::resamples(store_maxnode)
            summary(results_node)
          }
          x <- summary(results_node)
          maxnodes <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
          
          store_maxtrees <- list()
          for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
            rf_maxtrees <- caret::train(ign~.,
                                        data = predictors,
                                        method = "rf",
                                        metric = "Accuracy",
                                        tuneGrid = tuneGrid,
                                        trControl = trControl,
                                        importance = TRUE,
                                        nodesize = 14,
                                        maxnodes = maxnodes,
                                        ntree = ntree)
            key <- toString(ntree)
            store_maxtrees[[key]] <- rf_maxtrees
          }
          results_tree <- caret::resamples(store_maxtrees)
          x <- summary(results_tree)
          ntrees <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
          
          fit_rf <- caret::train(ign~.,
                                 data = predictors,
                                 method = "rf",
                                 metric = "Accuracy",
                                 tuneGrid = tuneGrid,
                                 trControl = trControl,
                                 importance = TRUE,
                                 nodesize = 14,
                                 maxnodes = maxnodes,
                                 ntree = ntrees)
          
          # model diagnostics
          print(fit_rf)
          
          # model variable randomForest::importance
          
          # predict probability of pres (1) or abs (0)
          prediction <- predict(fit_rf,
                                data_test)
          print(varImp(fit_rf,useModel = T,scale = F))
          
          write(x = paste0(cause," ",
                           "ALL"," ",
                           results$optVariables," ",
                           varImp(fit_rf,useModel = T,scale = F)),
                file = paste0(output_location,
                              "RF_Var_Imp.txt"),
                append = T,
                sep = "\n",
                ncolumns = 1
          )
          
          caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"]
          names(indicator_stack)<- tolower(names(indicator_stack))
          #change the indicator stack back to a Spatraster and add in fit_rf ( your tuned forest), rather then rf_default (your default forest)
          ign <- terra::predict(object = rast(indicator_stack),
                                model = fit_rf,
                                type = "prob",
                                fun = predict,
                                na.rm=T)[[2]]
          #sets values equal to zero for fuels which are water or nf based on the fbp codes, make sure fuel is loaded in raster
          ign[][which(indicator_stack$fuels[] %in% c(13:19))] <- 0
          
          terra::writeRaster(raster(ign),
                             paste0(output_location,
                                    paste(cause,
                                          "ALL",
                                          ifelse(min_fire_size != "",
                                                 paste0("ign_randomforest_auto_over_",
                                                        min_fire_size,
                                                        ".tif"),
                                                 "ign_randomforest_auto.tif"),
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          # Add to the raster list
          ign_raster_list[[paste0(cause, "_", "ALLszn")]] <- raster(ign)
          
          write(x = c(cause,
                      "ALL",
                      results$optVariables,
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
                file = paste0(output_location,
                              "RF_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    results$optVariables,
                                    caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
                )
          )
      }
    }
    
    # Gradient Boosting Machine -----------------------------------------------
    
    if (model == "gbm") {
      for (cause in causes) {
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          
          print(paste0("Starting ",cause," - ","ALL Seasons"))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data)] # remove NA instances
          
          if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          if (cause == causes[1]) {predictors <- indicators_1}
          if (cause == causes[2]) {predictors <- indicators_2}
          data_train <- data_train[,c("ign",predictors)]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          gbm_step <- dismo::gbm.step(data_train,
                                      gbm.y = 1,
                                      #using the same subset of response variables
                                      gbm.x = c(2:ncol(data_train)),
                                      #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                      tree.complexity = 3,
                                      n.folds = 10,
                                      #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                      learning.rate = 0.005,
                                      #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                      family = "bernoulli",
                                      max.trees = 2000,
                                      bag.fraction = 0.5)
          
          data_train <- data_train[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
          data_test <- data_test[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
          
          gbm_step <- dismo::gbm.step(data_train,
                                      gbm.y = 1,
                                      #using the same subset of response variables
                                      gbm.x = c(2:ncol(data_train)),
                                      #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                      tree.complexity = 3,
                                      n.folds = 10,
                                      #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                      learning.rate = 0.005,
                                      #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                      family = "bernoulli",
                                      max.trees = 2000,
                                      bag.fraction = 0.5)
          
          summary(gbm_step)
          
          predictions <- predict(gbm_step,
                                 data_test,
                                 n.trees = gbm_step$n.trees,
                                 type = "response")
          
          gbm_testing <- prediction(predictions, data_test$ign)
          roc_test <- performance(gbm_testing, "tpr","fpr")
          plot(roc_test)
          gbm_auc <- as.numeric(performance(gbm_testing, "auc")@y.values)
          print(gbm_auc)
          
          
          ign <- terra::predict(indicator_stack,
                                gbm_step,
                                n.trees = gbm_step$n.trees,
                                type = "response")
          
          scaled_ign <- (ign - global(ign, fun = 'min')) / (global(ign, fun = 'max') - global(ign, fun = 'min'))
          
          scaled_ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          write(x = c(cause,
                      "ALL",
                      results$optVariables,
                      paste0("Area Under the Curve: ",gbm_auc),
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(as.factor(ifelse(predictions > 0.8, 1, 0)),as.factor(data_test$ign))$byClass['Balanced Accuracy'])),
                file = paste0(output_location,
                              "GBM_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    results$optVariables,
                                    gbm_auc)
                )
          )
          
          terra::writeRaster(scaled_ign,
                             paste0(output_location,
                                    paste(cause,
                                          "ALL",
                                          ifelse(min_fire_size != "",
                                                 paste0("ign_gbmstep_over_",
                                                        min_fire_size,
                                                        ".tif"),
                                                 "ign_gbmstep.tif"),
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2"))) # write raster
        }
    }
    
    # Random Forest Stock -----------------------------------------------------
    
    if (model == "rf_stock"){
        for (cause in causes){
          
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          
          print(paste0("Starting ",cause," - ","ALL Seasons"))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data),] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          repeat {
            
            if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)]}
            if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)]}
            
            control <- caret::rfeControl(functions = caret::rfFuncs,
                                         method = "cv",
                                         number = 10,
                                         repeats = 2)
            results <- caret::rfe(x = predictors[-1],
                                  y = predictors[,"ign"],
                                  sizes = c(1:length(predictors)),
                                  rfeControl = control,
                                  p = 1,
                                  metric = "Accuracy")
            print(c("Results for ", paste0("Starting ",cause," - ","All Seasons"), "\n",results))
            
            if ( cause == "L" && max(results$results$Accuracy) > 0.65 ) { break }
            if ( cause == "H" && max(results$results$Accuracy) > 0.70 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
            
          }
          
          predictors <- predictors[,c("ign",results$optVariables)]
          
          best_mtry <- randomForest::tuneRF(predictors[-1],
                                            predictors$ign,
                                            ntree = 1500,
                                            stepFactor = 1.5,
                                            improve = 0.01,
                                            trace = TRUE,
                                            plot = TRUE)
          best_mtry <- best_mtry[which.min(best_mtry[,2]),1]
          
          rf <- randomForest::randomForest(ign ~ .,
                                           data = predictors,
                                           ntree = 1500,
                                           mtry = best_mtry,
                                           importance = TRUE,
                                           response.type = "binary")
          
          # model variable randomForest::importance
          imp <- randomForest::importance(rf)
          
          # predict probability of pres (1) or abs (0)
          prediction <- predict(rf, data_test)
          
          print(caret::confusionMatrix(prediction,
                                       data_test$ign)$byClass["Balanced Accuracy"])
          
          # build/name ign raster
          ign <- terra::predict(model = rf,
                                object = indicator_stack,
                                type = "prob")[[2]]
          
          x11()
          plot(ign)
          
          ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          write(x = c(cause,
                      "All",
                      paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                      paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
                file = paste0(output_location,
                              "RF_Model_Inputs.txt"),
                append = T,
                sep = "\t",
                ncolumns = length(c(cause,
                                    paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                                    caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
                )
          )
          
          terra::writeRaster(ign,
                             paste0(output_location,
                                    paste(cause,
                                          "All",
                                          "ign_randomforest.tif",
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          
        }
    }
    
    # Boosted Regression Tree -------------------------------------------------
    
    if (model == "brt"){
      for (cause in causes){
            tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data[fire_data$Cause == cause,]))
          
          print(paste0("Starting ",cause," - ","All Seasons"))
          pres_abs = terra::setValues(grast,0)
          pres_abs[tab] <- 1
          pres_abs <- terra::mask(pres_abs,grast)
          names(pres_abs) <- "ign"
          modelling_stack <- c(indicator_stack,pres_abs)
          names(modelling_stack) <- tolower(names(modelling_stack))
          
          # build ign dataframe
          data <- data.table::as.data.table(modelling_stack)
          data <- data[complete.cases(data),] # remove NA instances
          if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
          if (!is.null( factor_vars ) ) data[,
                                             (factor_vars) := lapply(.SD, as.factor),
                                             .SDcols = factor_vars]
          
          data_mod <- caret::downSample(x = data[,-"ign"],
                                        y = data$ign,
                                        yname = "ign")
          
          dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
          data_train <- data_mod[dat_part,]
          data_test <- data_mod[-dat_part,]
          
          data_train$ign <- as.integer(as.character(data_train$ign))
          
          if (testing == F) {
            if (nrow(data_train) <= Minimum_Num_Ignitions) {
              warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
            }
          }
          
          if (cause == causes[1]) {predictors <- data_train[,c("ign",indicators_1)] }
          if (cause == causes[2]) {predictors <- data_train[,c("ign",indicators_2)] }
          
          brt <- dismo::gbm.step(data = predictors,
                                 gbm.x = 2:length(predictors),
                                 gbm.y = 1,
                                 tree.complexity = 3,
                                 family = "bernoulli",
                                 n.folds = 20,
                                 n.trees = 500,
                                 step.size = 50,
                                 max.trees = 7500,
                                 learning.rate = 0.0005)
          
          brt <- dismo::gbm.step(data = predictors,
                                 gbm.x = which(names(predictors) %in% brt$contributions[brt$contributions$rel.inf >= 1.0, "var"]),
                                 gbm.y = 1,
                                 tree.complexity = 3,
                                 family = "bernoulli",
                                 n.folds = 20,
                                 n.trees = 500,
                                 step.size = 50,
                                 max.trees = 7500,
                                 learning.rate = 0.0005,
                                 plot.main = T)
          
          # model variable randomForest::importance
          imp <- summary(brt)
          
          # build/name ign raster
          ign <- terra::predict(model = brt,
                                object = indicator_stack,
                                type = "response",
                                n.trees = brt$n.trees,
                                na.rm = T)
          
          ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
          
          ign <- (ign - min(ign[],na.rm = T))/(max(ign[],na.rm = T) - min(ign[],na.rm = T))
          
          write(x = c(cause,
                      "All",
                      paste("CV AUC: ",round(mean(brt$cv.roc.matrix),2)),
                      paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": ")
          ),
          file = paste0(output_location,
                        "BRT_Model_Inputs.txt"),
          append = T,
          sep = "\t",
          ncolumns = length(c(cause,
                              paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": "),
                              mean(brt$cv.roc.matrix))
          )
          )
          
          terra::writeRaster(ign,
                             paste0(output_location,
                                    paste(cause,
                                          "ALL",
                                          "ign_boostedregressiontree.tif",
                                          sep = "_")
                             ),
                             overwrite = T,
                             wopt = list(filetype = "GTiff",
                                         datatype = "FLT4S",
                                         gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
          
      }
    }
  
  #export grids
  return(ign_raster_list)
} else{
  
  message("No Stratification...") 
  
  # Random Forest -----------------------------------------------------------
  if (model == "rf") {
      tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data))
      
      #Changed this section of code to edit data frame correctly: ie stack indicator and pres_abs(as a raster, not SpatRaster)
      print(paste0("Starting all Causes and All Seasons with RandomForest"))
      pres_abs = terra::setValues(grast,0)
      pres_abs[tab] <- 1
      pres_abs <- terra::mask(pres_abs,grast)
      names(pres_abs) <- "ign"
      modelling_stack <- stack(indicator_stack,raster(pres_abs))
      names(modelling_stack) <- tolower(names(modelling_stack))
      
      ###And changed this, replacing data table funciton with data frame to data table
      # build ign dataframe: 
      #data <- data.table::as.data.table(modelling_stack)
      # Convert SpatRaster to a data frame (cell values)
      data_df <- terra::as.data.frame(modelling_stack, xy = TRUE)
      # Convert the data frame to a data.table
      data <- data.table::as.data.table(data_df)
      #set ignitions to a factor
      data$ign<- as.factor(data$ign)
      
      
      ####
      data <- data[complete.cases(data)] # remove NA instances
      if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
      if (!is.null( factor_vars ) ) data[,
                                         (factor_vars) := lapply(.SD, as.factor),
                                         .SDcols = factor_vars]
      
      #change this section to use the rose function to oversample the data and equalize classes
      #data_mod <- caret::downSample(x = data[,-"ign"],
      #                             y = data$ign,
      #                            yname = "ign", replace=TRUE)
      # Oversample or undersample the data
      
      data_mod <- ROSE::ovun.sample(ign ~ ., data = data, method = "under")$data
      
      #
      dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
      data_train <- data_mod[dat_part,]
      data_test <- data_mod[-dat_part,]
      
      
      repeat {
        predictors <- data_train[,c("ign",unique(indicators_1,indicators_2))]
        
        control <- caret::rfeControl(functions = caret::rfFuncs, method = "cv",number = 10,repeats = 2)
        results <- caret::rfe(x = predictors[-1],y = predictors[,"ign"],sizes = c(1:length(predictors)),rfeControl = control, p = 1, metric = "Accuracy")
        print(results)
        
        if (max(results$results$Accuracy) > 0.50 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
      }
      
      predictors <- predictors[,c("ign",results$optVariables)]
      
      if (testing == F) {
        if (nrow(data_train) <= Minimum_Num_Ignitions) {
          warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
        }
      }
      
      trControl <- caret::trainControl(method = "cv",
                                       number = 10,
                                       search = "grid")
      # Run the model
      rf_default <- caret::train(ign~.,
                                 data = predictors,
                                 method = "rf",
                                 metric = "Accuracy",
                                 trControl = trControl)
      # Print the results
      print(rf_default)
      
      tuneGrid <- expand.grid(.mtry = c(1:10))
      rf_mtry <- caret::train(ign~.,
                              data = predictors,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = tuneGrid,
                              trControl = trControl,
                              importance = TRUE,
                              nodesize = 14,
                              ntree = 300)
      print(rf_mtry)
      best_mtry <- rf_mtry$bestTune$mtry
      mtry <- best_mtry
      
      store_maxnode <- list()
      tuneGrid <- expand.grid(.mtry = best_mtry)
      for (maxnodes in c(5:15)) {
        rf_maxnode <- caret::train(ign~.,
                                   data = predictors,
                                   method = "rf",
                                   metric = "Accuracy",
                                   tuneGrid = tuneGrid,
                                   trControl = trControl,
                                   importance = TRUE,
                                   nodesize = 14,
                                   maxnodes = maxnodes,
                                   ntree = 300)
        current_iteration <- toString(maxnodes)
        store_maxnode[[current_iteration]] <- rf_maxnode
      }
      results_node <- caret::resamples(store_maxnode)
      x <- summary(results_node)
      print(rf_maxnode)
      if (x$statistics$Accuracy[,"Mean"][length(x$statistics$Accuracy[,"Mean"])]/x$statistics$Accuracy[,"Mean"][1] > 1) {
        
        store_maxnode <- list()
        tuneGrid <- expand.grid(.mtry = best_mtry)
        for (maxnodes in c(20:30)) {
          rf_maxnode <- caret::train(ign~.,
                                     data = predictors,
                                     method = "rf",
                                     metric = "Accuracy",
                                     tuneGrid = tuneGrid,
                                     trControl = trControl,
                                     importance = TRUE,
                                     nodesize = 14,
                                     maxnodes = maxnodes,
                                     ntree = 300)
          key <- toString(maxnodes)
          store_maxnode[[key]] <- rf_maxnode
        }
        results_node <- caret::resamples(store_maxnode)
        summary(results_node)
      }
      x <- summary(results_node)
      maxnodes <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
      
      store_maxtrees <- list()
      for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
        rf_maxtrees <- caret::train(ign~.,
                                    data = predictors,
                                    method = "rf",
                                    metric = "Accuracy",
                                    tuneGrid = tuneGrid,
                                    trControl = trControl,
                                    importance = TRUE,
                                    nodesize = 14,
                                    maxnodes = maxnodes,
                                    ntree = ntree)
        key <- toString(ntree)
        store_maxtrees[[key]] <- rf_maxtrees
      }
      results_tree <- caret::resamples(store_maxtrees)
      x <- summary(results_tree)
      ntrees <- as.numeric(names(which.max(x$statistics$Accuracy[,"Mean"])))
      
      fit_rf <- caret::train(ign~.,
                             data = predictors,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             importance = TRUE,
                             nodesize = 14,
                             maxnodes = maxnodes,
                             ntree = ntrees)
      
      # model diagnostics
      print(fit_rf)
      
      # model variable randomForest::importance
      
      # predict probability of pres (1) or abs (0)
      prediction <- predict(fit_rf,
                            data_test)
      print(varImp(fit_rf,useModel = T,scale = F))
      
      write(x = paste0("AllCauses"," ",
                       "ALLSeasons"," ",
                       results$optVariables," ",
                       varImp(fit_rf,useModel = T,scale = F)),
            file = paste0(output_location,
                          "RF_Var_Imp.txt"),
            append = T,
            sep = "\n",
            ncolumns = 1
      )
      
      caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"]
      names(indicator_stack)<- tolower(names(indicator_stack))
      #change the indicator stack back to a Spatraster and add in fit_rf ( your tuned forest), rather then rf_default (your default forest)
      ign <- terra::predict(object = rast(indicator_stack),
                            model = fit_rf,
                            type = "prob",
                            fun = predict,
                            na.rm=T)[[2]]
      #sets values equal to zero for fuels which are water or nf based on the fbp codes, make sure fuel is loaded in raster
      ign[][which(indicator_stack$fuels[] %in% c(13:19))] <- 0
      
      terra::writeRaster(raster(ign),
                         paste0(output_location,
                                paste("AllCause",
                                      "ALLseason",
                                      ifelse(min_fire_size != "",
                                             paste0("ign_randomforest_auto_over_",
                                                    min_fire_size,
                                                    ".tif"),
                                             "ign_randomforest_auto.tif"),
                                      sep = "_")
                         ),
                         overwrite = T,
                         wopt = list(filetype = "GTiff",
                                     datatype = "FLT4S",
                                     gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
      # Add to the raster list
      ign_raster_list[[paste0("AllCse", "_", "ALLszn")]] <- raster(ign)
      
      write(x = c("AllCause",
                  "AllSeason",
                  results$optVariables,
                  paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
            file = paste0(output_location,
                          "RF_Model_Inputs.txt"),
            append = T,
            sep = "\t",
            ncolumns = length(c(results$optVariables,
                                caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
            )
      )
  }
  
  
  # Gradient Boosting Machine -----------------------------------------------
  
  if (model == "gbm") {

      tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data))
      
      print(paste0("Starting all Causes and Seasons for GradientBoostingMachine"))
      pres_abs = terra::setValues(grast,0)
      pres_abs[tab] <- 1
      pres_abs <- terra::mask(pres_abs,grast)
      names(pres_abs) <- "ign"
      modelling_stack <- c(indicator_stack,pres_abs)
      names(modelling_stack) <- tolower(names(modelling_stack))
      
      
      # build ign dataframe
      data <- data.table::as.data.table(modelling_stack)
      data <- data[complete.cases(data)] # remove NA instances
      
      if (!is.null( non_fuel_vals ) ) data <- data[!fuels %in% non_fuel_vals] ## Remove Rock and Water
      if (!is.null( factor_vars ) ) data[,
                                         (factor_vars) := lapply(.SD, as.factor),
                                         .SDcols = factor_vars]
      
      data_mod <- caret::downSample(x = data[,-"ign"],
                                    y = data$ign,
                                    yname = "ign")
      
      dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
      data_train <- data_mod[dat_part,]
      data_test <- data_mod[-dat_part,]
      
      predictors<- unique(indicators_1,indicators_2)
      data_train <- data_train[,c("ign",predictors)]
      
      if (testing == F) {
        if (nrow(data_train) <= Minimum_Num_Ignitions) {
          warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
        }
      }
      
      gbm_step <- dismo::gbm.step(data_train,
                                  gbm.y = 1,
                                  #using the same subset of response variables
                                  gbm.x = c(2:ncol(data_train)),
                                  #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                  tree.complexity = 3,
                                  n.folds = 10,
                                  #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                  learning.rate = 0.005,
                                  #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                  family = "bernoulli",
                                  max.trees = 2000,
                                  bag.fraction = 0.5)
      
      data_train <- data_train[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
      data_test <- data_test[,c("ign",as.character(summary(gbm_step)[summary(gbm_step)$rel.inf > 2.5,"var"]))]
      
      gbm_step <- dismo::gbm.step(data_train,
                                  gbm.y = 1,
                                  #using the same subset of response variables
                                  gbm.x = c(2:ncol(data_train)),
                                  #how deep per tree? can lead to overfitting but need to fit more trees with less complexity
                                  tree.complexity = 3,
                                  n.folds = 10,
                                  #how much can a model learn from a tree? you don't want to learn too much from a tree and get pulled by outliers, but don't want to fit a million trees either
                                  learning.rate = 0.005,
                                  #poisson data? I guess it looked poisson-ish... Can't use poisson with non-integer numbers though. Transform and use gaussian.
                                  family = "bernoulli",
                                  max.trees = 2000,
                                  bag.fraction = 0.5)
      
      summary(gbm_step)
      
      predictions <- predict(gbm_step,
                             data_test,
                             n.trees = gbm_step$n.trees,
                             type = "response")
      
      gbm_testing <- prediction(predictions, data_test$ign)
      roc_test <- performance(gbm_testing, "tpr","fpr")
      plot(roc_test)
      gbm_auc <- as.numeric(performance(gbm_testing, "auc")@y.values)
      print(gbm_auc)
      
      
      ign <- terra::predict(indicator_stack,
                            gbm_step,
                            n.trees = gbm_step$n.trees,
                            type = "response")
      
      scaled_ign <- (ign - global(ign, fun = 'min')) / (global(ign, fun = 'max') - global(ign, fun = 'min'))
      
      scaled_ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
      
      write(x = c("AllCause",
                  "AllSeason",
                  results$optVariables,
                  paste0("Area Under the Curve: ",gbm_auc),
                  paste0("Balanced Accuracy: ",caret::confusionMatrix(as.factor(ifelse(predictions > 0.8, 1, 0)),as.factor(data_test$ign))$byClass['Balanced Accuracy'])),
            file = paste0(output_location,
                          "GBM_Model_Inputs.txt"),
            append = T,
            sep = "\t",
            ncolumns = length(c(results$optVariables,
                                gbm_auc)
            )
      )
      
      terra::writeRaster(scaled_ign,
                         paste0(output_location,
                                paste("AllCause",
                                      "AllSeason",
                                      ifelse(min_fire_size != "",
                                             paste0("ign_gbmstep_over_",
                                                    min_fire_size,
                                                    ".tif"),
                                             "ign_gbmstep.tif"),
                                      sep = "_")
                         ),
                         overwrite = T,
                         wopt = list(filetype = "GTiff",
                                     datatype = "FLT4S",
                                     gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2"))) # write raster
    }
  
  
  # Random Forest Stock -----------------------------------------------------
  
  if (model == "rf_stock"){
      
      tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data))
      
      print(paste0("Starting all Causes and all Seasons for RandomForestStock"))
      pres_abs = terra::setValues(grast,0)
      pres_abs[tab] <- 1
      pres_abs <- terra::mask(pres_abs,grast)
      names(pres_abs) <- "ign"
      modelling_stack <- c(indicator_stack,pres_abs)
      names(modelling_stack) <- tolower(names(modelling_stack))
      
      # build ign dataframe
      data <- data.table::as.data.table(modelling_stack)
      data <- data[complete.cases(data),] # remove NA instances
      if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
      if (!is.null( factor_vars ) ) data[,
                                         (factor_vars) := lapply(.SD, as.factor),
                                         .SDcols = factor_vars]
      
      data_mod <- caret::downSample(x = data[,-"ign"],
                                    y = data$ign,
                                    yname = "ign")
      
      dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
      data_train <- data_mod[dat_part,]
      data_test <- data_mod[-dat_part,]
      
      if (testing == F) {
        if (nrow(data_train) <= Minimum_Num_Ignitions) {
          warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
        }
      }
      
      repeat {
        predictors <- data_train[,c("ign",unique(indicators_1,indicators_2))]
        
        control <- caret::rfeControl(functions = caret::rfFuncs,
                                     method = "cv",
                                     number = 10,
                                     repeats = 2)
        results <- caret::rfe(x = predictors[-1],
                              y = predictors[,"ign"],
                              sizes = c(1:length(predictors)),
                              rfeControl = control,
                              p = 1,
                              metric = "Accuracy")
        print(c("Results for ", paste0("Starting All Seasons and Causes"), "\n",results))
        
        if (max(results$results$Accuracy) > 0.60 && results$results[which.max(results$results$Accuracy),"Variables"] > 3 ) { break }
        
      }
      
      predictors <- predictors[,c("ign",results$optVariables)]
      
      best_mtry <- randomForest::tuneRF(predictors[-1],
                                        predictors$ign,
                                        ntree = 1500,
                                        stepFactor = 1.5,
                                        improve = 0.01,
                                        trace = TRUE,
                                        plot = TRUE)
      best_mtry <- best_mtry[which.min(best_mtry[,2]),1]
      
      rf <- randomForest::randomForest(ign ~ .,
                                       data = predictors,
                                       ntree = 1500,
                                       mtry = best_mtry,
                                       importance = TRUE,
                                       response.type = "binary")
      
      # model variable randomForest::importance
      imp <- randomForest::importance(rf)
      
      # predict probability of pres (1) or abs (0)
      prediction <- predict(rf, data_test)
      
      print(caret::confusionMatrix(prediction,
                                   data_test$ign)$byClass["Balanced Accuracy"])
      
      # build/name ign raster
      ign <- terra::predict(model = rf,
                            object = indicator_stack,
                            type = "prob")[[2]]
      
      x11()
      plot(ign)
      
      ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
      
      write(x = c("AllCauses",
                  "AllSeasons",
                  paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                  paste0("Balanced Accuracy: ",caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])),
            file = paste0(output_location,
                          "RF_Model_Inputs.txt"),
            append = T,
            sep = "\t",
            ncolumns = length(c(paste(names(sort(imp[,3],decreasing = T)),round(sort(imp[,3],decreasing = T),2)),
                                caret::confusionMatrix(prediction, data_test$ign)$byClass["Balanced Accuracy"])
            )
      )
      
      terra::writeRaster(ign,
                         paste0(output_location,
                                paste("AllCauses",
                                      "AllSeasons",
                                      "ign_randomforest.tif",
                                      sep = "_")
                         ),
                         overwrite = T,
                         wopt = list(filetype = "GTiff",
                                     datatype = "FLT4S",
                                     gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
      
    }
  
  
  # Boosted Regression Tree -------------------------------------------------
  
  if (model == "brt"){
      tab <- terra::cellFromXY(terra::setValues(grast,0), st_coordinates(fire_data))
      
      print(paste0("Starting All Seasons and Causes with BoostedRegressionTrees"))
      pres_abs = terra::setValues(grast,0)
      pres_abs[tab] <- 1
      pres_abs <- terra::mask(pres_abs,grast)
      names(pres_abs) <- "ign"
      modelling_stack <- c(indicator_stack,pres_abs)
      names(modelling_stack) <- tolower(names(modelling_stack))
      
      # build ign dataframe
      data <- data.table::as.data.table(modelling_stack)
      data <- data[complete.cases(data),] # remove NA instances
      if (!is.null( non_fuel_vals ) ) data <- data[-which(data$fuels %in% non_fuel_vals),] ## Remove Rock and Water
      if (!is.null( factor_vars ) ) data[,
                                         (factor_vars) := lapply(.SD, as.factor),
                                         .SDcols = factor_vars]
      
      data_mod <- caret::downSample(x = data[,-"ign"],
                                    y = data$ign,
                                    yname = "ign")
      
      dat_part <- caret::createDataPartition(y = data_mod$ign,p = .8)[[1]]
      data_train <- data_mod[dat_part,]
      data_test <- data_mod[-dat_part,]
      
      data_train$ign <- as.integer(as.character(data_train$ign))
      
      if (testing == F) {
        if (nrow(data_train) <= Minimum_Num_Ignitions) {
          warning("Sample was less than", Minimum_Num_Ignitions,"elements, skipping. There were, ",nrow(data[data$ign == 1,])," actual ignitions in the training data.");next
        }
      }
      predictors <- data_train[,c("ign",unique(indicators_1,indicators_2))]
      
      brt <- dismo::gbm.step(data = predictors,
                             gbm.x = 2:length(predictors),
                             gbm.y = 1,
                             tree.complexity = 3,
                             family = "bernoulli",
                             n.folds = 20,
                             n.trees = 500,
                             step.size = 50,
                             max.trees = 7500,
                             learning.rate = 0.0005)
      
      brt <- dismo::gbm.step(data = predictors,
                             gbm.x = which(names(predictors) %in% brt$contributions[brt$contributions$rel.inf >= 1.0, "var"]),
                             gbm.y = 1,
                             tree.complexity = 3,
                             family = "bernoulli",
                             n.folds = 20,
                             n.trees = 500,
                             step.size = 50,
                             max.trees = 7500,
                             learning.rate = 0.0005,
                             plot.main = T)
      
      # model variable randomForest::importance
      imp <- summary(brt)
      
      # build/name ign raster
      ign <- terra::predict(model = brt,
                            object = indicator_stack,
                            type = "response",
                            n.trees = brt$n.trees,
                            na.rm = T)
      
      ign[][which(indicator_stack$fuels[] %in% c(101:110))] <- 0
      
      ign <- (ign - min(ign[],na.rm = T))/(max(ign[],na.rm = T) - min(ign[],na.rm = T))
      
      write(x = c("AllCauses",
                  "AllSeasons",
                  paste("CV AUC: ",round(mean(brt$cv.roc.matrix),2)),
                  paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": ")
      ),
      file = paste0(output_location,
                    "BRT_Model_Inputs.txt"),
      append = T,
      sep = "\t",
      ncolumns = length(c(paste(imp[,"var"],round(imp[,"rel.inf"],2),sep = ": "),
                          mean(brt$cv.roc.matrix))
      )
      )
      
      terra::writeRaster(ign,
                         paste0(output_location,
                                paste("AllCauses",
                                      "AllSeasons",
                                      "ign_boostedregressiontree.tif",
                                      sep = "_")
                         ),
                         overwrite = T,
                         wopt = list(filetype = "GTiff",
                                     datatype = "FLT4S",
                                     gdal = c("COMPRESS=DEFLATE","ZLEVEL=9","PREDICTOR=2")))
      
    }

  #export grids
  return(ign_raster_list)

  } 
#end of function bracket-----------------------------------------------------------------------------------------------------------  
}

