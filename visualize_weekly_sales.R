#####################################################################
# Description:
# Plot the weekly sales (also save in png format) for varying depts.
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


rm(list=ls(all=TRUE))
gc(reset=TRUE)


#### set the working directory to the path that contains all the data files:
# - ./data/train.csv
# - ./data/test.csv
# - ./data/features.csv
# - ./data/stores.csv
# - ./data/sampleSubmission.csv
#setwd("E:/Walmart")
#setwd("F:/ChenChenglong/R/Walmart")
setwd("F:/³Â³ÉÁú/R/Walmart/R/git-final")


#### put the required packages here
require(lattice)

#### Read in all the data provided
load('./data/training_testing_data_v1.RData')


figure_format <- 'png'

for(dept in sort(unique(sort(dfTrain$Dept)))){
  
  #### Creat the corresponding dir
  filePath <- './visualization/weekly_sales'
  dir.create(filePath, showWarnings=FALSE, recursive=TRUE)
  if(figure_format=='pdf'){
    pdf(paste(filePath, '/Dept', dept,'.pdf', sep=''))
  }else if(figure_format=='png'){
    png(paste(filePath, '/Dept', dept,'.png', sep=''))
  }
  
  dfTrain2 <- subset(dfTrain, Dept==dept)
  # create scatter plot
  print(xyplot(log(Weekly_Sales)~Day_Index|Store,
               data=dfTrain2, main=paste('Dept: ', dept, sep=''), as.table=TRUE,
               strip = strip.custom(strip.names = TRUE, strip.levels = TRUE),
               par.strip.text = list(cex = 0.75)))
  dev.off()
}