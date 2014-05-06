#####################################################################
# Description:
# Train GBM for store weekly sales forecasting, plot the predicted
# weekly sales (also save in png format), and make submission in the
# required csv format.
# Version: 1.0
# 
# Kaggle contest description, rules and data: 
# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
# 
# Author: Chenglong Chen <c.chenglong@gmail.com>
# Contact:
# http://weibo.com/denny2cd
# https://github.com/ChenglongChen
# http://www.kaggle.com/users/102203/yr
#####################################################################
# Code description:
# This code uses the observation that for the same dept, the weekly
# sales are very similar despite of different magnitudes across all
# the stores. So, it seems that the same dept sales the same kind of
# products?
# To see this, use file <visualize_weekly_sales.R>
# 
# This code also plots the predicted weekly sales and saves them in
# png format in a new created folder in the current working dircetory.
# 
# Using the params setting in this code, you should yield WMAE around
# 2500. Feel free to play with those params to see how far you can go!
# 
# NOTE: 
# In this version (version 1.0), some depts are not well modeled.
# Take dept=1 for example. The periodic artifact is not well caputred.
# However, version 2.0 seems doing a job for dept=1. So, motivated by
# such observation, I manually post-process the predictions of version
# 1.0, and replace some of those with the corresponding predictions
# from version 2.0, despite of the fact that version 2.0 yields higher
# overall WMAE. By replacing the obviously dept=1, WMAE reduces about
# 10~15.
# From a practical aspect, however, better approach should be taken
# to ensemble the predictions from these two :)
# 
# For further improvement, one can use models/features that can well
# capature the increasing/decaresing trend+seasonal periodic artifact
# in the weekly sales time series data.
# Code for the Global Energy Forecasting Competition 2012 may be a
# good start:
# https://github.com/jamesrobertlloyd/GEFCOM2012
#####################################################################


rm(list=ls(all=TRUE))
gc(reset=TRUE)


######################
## Helper Functions ##
######################

#### This function predict Weekly_Sales with trained gbm model
predTest <- function(gbm_, dfTest, dfTrain, inverse_transform, lambda=NULL){
  store_test <- sort(unique(dfTest$Store))
  store_train <- sort(unique(dfTrain$Store))
  pred_test <- NULL
  # get the number of best iteration using cross-validation
  best.iter <- gbm.perf(gbm_, method="cv", plot.it =FALSE)
  for(store in store_test){
    ind_ <- which(dfTest$Store == store)
    # NOte that for some dept, e.g., 99, there are some stores with dept 99 in
    # the test data which however is NOT in the training data. So, we have to
    # check this out.
    if(store %in% store_train){
      # we explicityly specify the best iteration to avoid printing mess around
      p <- predict(gbm_, newdata=dfTest[ind_,], n.trees=best.iter)
      if(!is.null(lambda)){
        p <- inverse_transform(p, lambda)
      }else{
        p <- inverse_transform(p)
      }
      pred_test <- c(pred_test, p)
    }else{
      # we have no data corresponding to this combination of store & dept in
      # training data. Since we have store as factor, this will raise error.(?)
      # Here, we use a simple strategy by circling the store factor in this
      # training data, and use that to replace the current non-exist store factor
      # (from the test data). We then make prediction on this modified test data.
      # In the end, we simply take the median (or mean) value as prediction.
      ps <- NULL
      dfTest2 <- dfTest
      for(s in store_train){
        dfTest2$Store[ind_] <- s
        p <- predict(gbm_, newdata=dfTest2[ind_,], n.trees=best.iter)
        if(!is.null(lambda)){
          p <- inverse_transform(p, lambda)
        }else{
          p <- inverse_transform(p)
        }
        ps <- rbind(ps, p)
      }
      p <- apply(ps, 2, median)
      pred_test <- c(pred_test, p)
    }
  }
  return(pred_test)
}

#### This function plots the fitted Weekly_Sales for all the combinations
# of Store & specified pred_depts
plotFittedWeekly_Sales <- function(gbm_, GBM_Params, dfTest, dfTrain, pred_depts,
                                   transform_str, inverse_transform, lambda=NULL,
                                   random_seed=2014){
  
  for(dept in pred_depts){
    this_dept_store <- sort(unique(dfTest$Store[which(dfTest$Dept==dept)]))
    for(store in this_dept_store){
      
      #### Creat the corresponding dir
      filePath <- paste('./figures cross dept v1 - bag.fraction',
                        GBM_Params$bag.fraction,'/',
                        GBM_Params$distribution,'_',
                        transform_str,'_',
                        'weight',holiday_weight,'_',
                        'RandomSeed', random_seed, sep='')
      dir.create(filePath, showWarnings=FALSE, recursive=TRUE)
      
      #### Open png device for plotting
      png(paste(filePath, '/GBM_',
                '[Ntree',GBM_Params$n.trees,']_',
                '[lr', GBM_Params$shrinkage,']_',
                'Store', store, '_Dept', dept,'.png', sep=''))
      
      #### Make prediction for testing data of this store and dept combination
      dfTest2 <- subset(dfTest, Store==store & Dept==dept)
      pred_test <- predTest(gbm_, dfTest2, dfTrain, inverse_transform, lambda)
      
      #### Make prediction for training data of this store and dept combination
      dfTrain2 <- subset(dfTrain, Store==store & Dept==dept)
      # check if we have Weekly_Slaes in the training data for this
      # store and dept combination
      if(dim(dfTrain2)[1]>0){
        pred_train <- predTest(gbm_, dfTrain2, dfTrain, inverse_transform, lambda)
        
        #### Plot the actual and fitted Weekly_Sales
        plot(dfTrain2$Day_Index, dfTrain2$Weekly_Sales,
             type='l', col='black', xlim=c(1,180),
             main=paste('Store: ', store, ' Dept: ', dept, sep=''),
             xlab='Date Index', ylab='Weekly Sales')
        points(c(dfTrain2$Day_Index, dfTest2$Day_Index),c(pred_train,pred_test),
               type='l', col='red')
        
      }else{
        # We don't have, so we just plot the prediction for testing data
        #### Plot the fitted Weekly_Sales
        plot(dfTest2$Day_Index, pred_test,
             type='l', col='red', xlim=c(1,180),
             main=paste('Store: ', store, ' Dept: ', dept, sep=''),
             xlab='Date Index', ylab='Weekly Sales')
      }
      dev.off()
      gc(reset=T)
    }
  }
}


#### set the working directory to the path that contains all the data files:
# - ./data/train.csv
# - ./data/test.csv
# - ./data/features.csv
# - ./data/stores.csv
# - ./data/sampleSubmission.csv
#setwd("E:/Walmart")
#setwd("F:/ChenChenglong/R/Walmart")
setwd("F:/³Â³ÉÁú/R/Walmart/R/git-final")
#setwd("E:/Walmart/R/git-final")


#### put the required packages here
require(gbm)
require(geoR)
require(Hmisc)


#####################
## Preprocess Data ##
#####################

#### Call 'preprocess_data_v1.R' to clean and preprocess the provided data
# source('./preprocess_data_v1.R')

#### Load training and testing data produced by 'preprocess_data_V1.R'
load('./data/training_testing_data_V1.RData')


#####################
## Params Settings ##
#####################

#### number of random seed
# I actually used 10 and averaged those predictions to reduce variance.
# This can give 10~20 improvement.
seed_num <- 1
# random seeds
set.seed(2014) # to ensure reproducable
random_seeds <- sample(10000, seed_num)

#### params for gbm
GBM_Ntrees <- 5000
GBM_Shrinkage <- 0.1
# stochastic GBM to reduce the chance of overfitting
GBM_Bag.fraction <- 0.7
# gaussian seems work better than laplace and quantile
GBM_Distributions <- c('laplace', 'gaussian')[2]

#### setting for holiday weight/response transform
Holiday_Weight <- c(1, 5, 10, 20, 30, 40, 50, 100)[1]
# er... 1 seems work best
Holiday_Weight <- seq(1,5)[1]

#### Settings for response transform
sqr <- function(x){x^2}
## function for Box-Cox transform
BoxCox <- function(x, lambda){
  lambda1 <- lambda[1]
  lambda2 <- lambda[2]
  if(lambda1 == 0){
    x1 <- log(x + lambda2)
  }else{
    x1 <- ((x + lambda2)^lambda1 - 1)/lambda1
  }
  return(x1)
}
## function for inversed Box-Cox transform
invBoxCox <- function(x1, lambda){
  lambda1 <- lambda[1]
  lambda2 <- lambda[2]
  if(lambda1 == 0){
    x <- exp(x1) - lambda2
  }else{
    x <- (x1*lambda1 + 1)^(1/lambda1) - lambda2
  }
  return(x)
}
# note the element are FUNCTION
Transform <- c(BoxCox, sqrt, log, identity)[2]
# note the element are STRING
Transform_Str <- c('BoxCox', "sqrt", "log", "identity")[2]
# note the element are FUNCTION
Inverse_Transform <- c(invBoxCox, sqr, exp, identity)[2]
# Note in the above, identity, log, and sqrt are special cases of BoxCox
# with params (lambda1, lambda2), equal to (1,1), (0,0), and (1/2,0), respectively
# Due to time limit, I put BoxCox in high pority. However, sqrt seems work best.
# NOTE:
# When using Box-Cox transform, some of the final prediction maybe NA. 
# Therefore, some post-processing might be needed to avoid that being happen.
# One simple approach I took is using the corresponding predictions from sqrt
# transform to replace those NA.


#### debugging?
debug_on <- FALSE

if(debug_on == TRUE){
  # plot fitted weekly sales?
  plot_fitted_on <- TRUE
  # make submission?
  save_on <- TRUE
}else{
  # plot fitted weekly sales?
  plot_fitted_on <- TRUE
  # make submission?
  save_on <- TRUE
}


##########
## Main ##
##########
dept_type1 = c(1, 18)
dept_type2 = c(2, 4, 8, 60, 79, 80, 81, 87, 90, 91, 92, 93, 94, 97, 98)
dept_type3 = c(3)
dept_type4 = c(5, 6, 7, 14, 17, 21, 22, 23, 25, 26, 27, 29,
               32, 46, 48, 55, 59, 72, 82, 85, 96, 99)
dept_type5 = c(9, 10, 19, 30, 34, 35, 37, 52)
dept_type6 = c(24, 31, 33)
dept_type7 = c(11, 36)
dept_type8 = c(12, 16, 56, 95)
dept_type9 = c(13, 20, 42, 49, 50, 83)
dept_type10 = c(28, 40)
dept_type11 = c(41, 44)
dept_type12 = c(67)
dept_type13 = c(71, 74)
depts_list = list(dept_type1, dept_type2, dept_type3, dept_type4, dept_type5,
                 dept_type6, dept_type7, dept_type8, dept_type9, dept_type10,
                 dept_type11, dept_type12, dept_type13)

depts_test <- sort(unique(dfTest$Dept))
depts_test <- as.numeric(as.character(depts_test))

dept_alone <- depts_test[!depts_test %in% unlist(depts_list)]
depts_list <- c(depts_list, as.list(dept_alone))

#### Train model
for(count_seed in seq(1, length(random_seeds))){
  for(holiday_weight in Holiday_Weight){
    for(count_transform in seq(1,length(Transform))){
      for(GBM_distribution in GBM_Distributions){        
        for(dept in depts_list){
        #for(dept in depts_list[1]){
          
          #### Set random seed
          random_seed <- random_seeds[count_seed]
          set.seed(random_seed)        
          
          #### Grab params for transform
          apply_transform <- Transform[[count_transform]]
          transform_str <- Transform_Str[[count_transform]]
          inverse_transform <- Inverse_Transform[[count_transform]]        
          
          #### Grab params for gbm training
          GBM_Params <- list(distribution=GBM_distribution,
                             n.trees=GBM_Ntrees,
                             shrinkage=GBM_Shrinkage,
                             bag.fraction=GBM_Bag.fraction)
          
          #### Verbose
          cat('Seed:', count_seed,
              '| Holiday Weight:', holiday_weight,
              '| Response Transform:', transform_str,
              '| GBM Distribution:', GBM_distribution,
              '| Dept:', dept,
              '\n', sep=' ')
          
          #### prepare training and validation data
          indTrain2 <- which(dfTrain$Dept %in% dept)
          dfTrain2 <- dfTrain[indTrain2,]
          # we jgnore those training data with NaN/NA or -Inf/Inf Weekly_Salse after
          # apply transform. To do so we use is.finite() function and note that
          # is.finite(NA) or is.finite(NaN) also returens FALSE!
          if(transform_str == "BoxCox"){
            cat('Estimate Box-Cox transform parameters ...\n')
            require(geoR)
            # for using boxcoxfit() function, we have to only use the positive values          
            indTrain2_IgnoreNA <- which(dfTrain2$Weekly_Sales>0)
            dfTrain2_IgnoreInfNA <- dfTrain2[indTrain2_IgnoreNA,]
            # estimate the params lambda1 and lambda2
            r <- tryCatch(
              expr=boxcoxfit(dfTrain2_IgnoreInfNA$Weekly_Sales, lambda2=TRUE),
              error=function(e)boxcoxfit(dfTrain2_IgnoreInfNA$Weekly_Sales)
            )
            lambda <- r$lambda
            if(length(lambda)==1){
              lambda <- c(lambda, 0)
            }
            cat('lambda1: ', lambda[1],' lambda2: ', lambda[2], '\n\n', sep='')
            
            ## apply transform
            dfTrain2_IgnoreInfNA$Weekly_Sales_Transformed <- BoxCox(dfTrain2_IgnoreInfNA$Weekly_Sales, lambda)
            
          }else{
            indTrain2_IgnoreNA <- which(is.finite(apply_transform(dfTrain2$Weekly_Sales)))
            # for other transform, we set lambda to NULL
            lambda <- NULL
            
            ## apply transform
            dfTrain2_IgnoreInfNA <- dfTrain2[indTrain2_IgnoreNA,]
            dfTrain2_IgnoreInfNA$Weekly_Sales_Transformed <- apply_transform(dfTrain2_IgnoreInfNA$Weekly_Sales)
          }          
          
          ## extract the weights
          this_valid_weights <- trainWeights[indTrain2[indTrain2_IgnoreNA]]
          this_gbm_weights <- trainWeights[indTrain2[indTrain2_IgnoreNA]]
          this_gbm_weights[this_gbm_weights==5] <- holiday_weight
          
          
          #### get the test data for this combination of store and dept
          indTest <- which(dfTest$Dept %in% dept)
          dfTest2 <- dfTest[indTest,]
          testID2 <- testID[indTest]
          
          if(dim(dfTrain2_IgnoreInfNA)[1]>50){
            
            #### before training gbm, we have to do the following cleaning to avoid
            # errors/warnings
            # check if MarkDowni are of all NA, if so, we do not include it in
            # the predictors
            for(count_markdown in seq(1,5)){
              # original MarkDown
              markdown <- paste('MarkDown', count_markdown, sep='')
              if(all(is.na(dfTrain2_IgnoreInfNA[markdown]))){
                # remove it
                dfTrain2_IgnoreInfNA <- dfTrain2_IgnoreInfNA[-which(colnames(dfTrain2_IgnoreInfNA)==markdown)]
              }
            }
            
            
            ##### Traing gbm
            # remeber NOT to include the original weekly sales in the predictiors!
            ind_NotNA <- which(!is.na(dfTrain2_IgnoreInfNA$Weekly_Sales_Transformed))
            gbm_ <- gbm(Weekly_Sales_Transformed ~ .-Weekly_Sales,
                        data = dfTrain2_IgnoreInfNA[ind_NotNA,],
                        distribution = GBM_distribution,
                        weights = this_gbm_weights[ind_NotNA],
                        n.trees = GBM_Ntrees,
                        shrinkage = GBM_Shrinkage,
                        interaction.depth = 10,
                        n.minobsinnode = 10,
                        bag.fraction = GBM_Bag.fraction,
                        train.fraction = 1.0,
                        cv.folds = 2,
                        n.cores = 2,
                        verbose = TRUE,
                        keep.data = FALSE)
            # get the best iteration wrt cv error
            best.iter <- gbm.perf(gbm_, method="cv")
            min.cv.error <- min(gbm_$cv.error)
            abline(h=min.cv.error, col='blue', lwd=2, lty=2)
            cat('Minimum CV error: ', round(min.cv.error,5),
                ' arrive at iteration: ', best.iter, '\n', sep='')
            
            #### Make prediction
            pred_valid <- predTest(gbm_, dfTrain2_IgnoreInfNA, dfTrain2_IgnoreInfNA,
                                   inverse_transform, lambda)
            pred_test <- predTest(gbm_, dfTest2, dfTrain2_IgnoreInfNA,
                                  inverse_transform, lambda)
            
            
            #### plot the histogram before and after Box-Cox transform
            par(mfrow=c(3,1))
            hist(dfTrain2_IgnoreInfNA$Weekly_Sales, breaks=100,
                 main='Histogram of original weekly sales',
                 xlab='Weekly Sales',
                 ylab='Frequency')
            hist(dfTrain2_IgnoreInfNA$Weekly_Sales_Transformed, breaks=100,
                 main=paste('Histogram of weekly sales after ', transform_str, ' transform',sep=''),
                 xlab='Weekly Sales',
                 ylab='Frequency')
            hist(pred_valid, breaks=100,
                 main='Histogram of predicted weekly sales',
                 xlab='Weekly Sales',
                 ylab='Frequency')
            par(mfrow=c(1,1))
            
            
            #### Compute WMAE on the training set (Er, we here do not do CV for simplicity)    
            WMAE <- this_valid_weights*abs(dfTrain2_IgnoreInfNA$Weekly_Sales - pred_valid)
            indGood <- which(is.finite(WMAE))
            WMAE <- sum(WMAE[indGood])/sum(this_valid_weights[indGood])
            cat('WMAE on training set: ', WMAE, '\n', sep='')
            
            
            #### Plot fitted Weekly_Sales
            if(plot_fitted_on == TRUE){
              cat('Plot fitted Weekly_Sales ...', '\n', sep='')
              pred_depts <- dept
              plotFittedWeekly_Sales(gbm_, GBM_Params, dfTest2, dfTrain2_IgnoreInfNA, pred_depts,
                                     transform_str, inverse_transform, lambda, random_seed)
            }
            
          }else if(dim(dfTrain2_IgnoreInfNA)[1]>0){
            # we do not have enough training data to fit gbm, so we just predict
            # the historical median those (positive) weekly sales
            # (could be improved by using glm?)
            # for all the stores in the training data, of course we have the
            # historical weekly sales...
            pred_valid <- rep(0, dim(dfTrain2)[1])
            all_train_store <- sort(unique(dfTrain2$Store))
            for(store in all_train_store){
              ind <- which(dfTrain2$Store == store)
              pred_valid[ind] <- median(dfTrain2$Weekly_Sales[ind], na.rm=T)
            }
            # for those stores in the testing data but not in the training data
            # we use the historical median of ALL the stores in the training data
            med_all_store <- median(dfTrain2$Weekly_Sales, na.rm=T)
            pred_test <- rep(med_all_store, dim(dfTest2)[1])
            all_test_store <- sort(unique(dfTest2$Store))
            # if we have historical weekly sales, we replace the initial prediction
            for(store in all_test_store){
              if(store %in% all_train_store){
                ind_test <- which(dfTest2$Store == store)
                ind_train <- which(dfTrain2$Store == store)
                pred_test[ind_test] <- median(dfTrain2$Weekly_Sales[ind_train], na.rm=T)
              }
            }
            
            WMAE <- this_valid_weights*abs(dfTrain2$Weekly_Sales - pred_valid)
            WMAE <- sum(WMAE)/sum(this_valid_weights)
            cat('WMAE on validation set: ', WMAE, '\n', sep='')
            
          }else{
            # we don't have weekly sales, so we simply predict 0
            pred_test <- rep(0, dim(dfTest2)[1])
            #
            pred_valid <- rep(0, dim(dfTrain2)[1])
            # since we don't have weekly sales, so we treat WMAE as NA, this is the same
            # across different models
            WMAE <- NA
            cat('WMAE for training: ', WMAE, '\n', sep='')
            
          }
          
          #### Make submission
          cat('Make submission ...', '\n', sep='')
          filePath <- paste('./submission cross dept v1 - bag.fraction',
                            GBM_Bag.fraction, sep='')
          dir.create(filePath, showWarnings=FALSE)
          fileName <- paste(filePath, '/Cross_Store_',
                            '[GBM_', capitalize(GBM_distribution),
                            '_', capitalize(transform_str),']_',
                            '[Weight',holiday_weight,']_',
                            '[Ntree',GBM_Ntrees,']_',
                            '[lr', GBM_Shrinkage,']_',
                            '[RandomSeed', random_seed,']',
                            #'[WMAE', round(WMAE,5), ']',
                            '.csv', sep='')
          
          # make submission and save
          dfSampleSubmission$Weekly_Sales[which(dfSampleSubmission$Id %in% testID2)] <- pred_test
          if(save_on == TRUE){
            write.csv(dfSampleSubmission, fileName, quote=F, row.names=F)
          }          
        }
      }
    }
  }
}