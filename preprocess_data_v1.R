#####################################################################
# Description: Clean the data and generate features.
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
# 
# Many thanks to Kaggler @Kakuda for code of merging dataframes in R:
# http://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting
# /forums/t/7214/merging-into-one-file-using-r
#####################################################################


gc(reset=TRUE)


#### put the required packages here
require(reshape)


#####################
## Preprocess Data ##
#####################
cat('Preprocess data ...\n')

#### Read in all the data provided
dfStore <- read.csv(file='./data/stores.csv', header=TRUE)
dfTrain <- read.csv(file='./data/train.csv', header=TRUE)
dfTest <- read.csv(file='./data/test.csv', header=TRUE)
dfFeatures <- read.csv(file='./data/features.csv', header=TRUE)
dfSampleSubmission <- read.csv(file='./data/sampleSubmission.csv', header=TRUE)


#### Merge Type and Size of stores
dfTrain <- merge(x=dfTrain, y=dfStore, all.x=TRUE)
dfTest <- merge(x=dfTest, y=dfStore, all.x=TRUE)


#### Merge all the features
dfTrain <- merge(x=dfTrain, y=dfFeatures, all.x=TRUE)
dfTest <- merge(x=dfTest, y=dfFeatures, all.x=TRUE)


#### Convert Date to character
# This MUST NOT place in front of the two above merge operations.
# Otherwise, R complains about not enough memory. No idea why.
dfTrain$Date <- as.character(dfTrain$Date)
dfTest$Date <- as.character(dfTest$Date)
dfFeatures$Date <- as.character(dfFeatures$Date)


#### Compute the number of days back to baseline date
baseline_date <- as.Date('2010-02-05')
dfTrain$Days <- as.numeric(as.Date(dfTrain$Date) - baseline_date)
dfTest$Days <- as.numeric(as.Date(dfTest$Date) - baseline_date)


#### Compute the corresponding day index for plotting figure
all_dates <- sort(unique(dfFeatures$Date))
dfTrain$Day_Index <- sapply(dfTrain$Date, function(d)which(d==all_dates))
dfTest$Day_Index <- sapply(dfTest$Date, function(d)which(d==all_dates))


#### Split Date into Year/Month/Day
## train
d <- strsplit(dfTrain$Date, '-')
d <- as.numeric(unlist(d))
d <- matrix(d, dim(dfTrain)[1], 3, byrow=T)
dfTrain$Year <- d[,1]
dfTrain$Month <- d[,2]
dfTrain$Day <- d[,3]
## test
d <- strsplit(dfTest$Date, '-')
d <- as.numeric(unlist(d))
d <- matrix(d, dim(dfTest)[1], 3, byrow=T)
dfTest$Year <- d[,1]
dfTest$Month <- d[,2]
dfTest$Day <- d[,3]


#### Switch columns for convenience :)
col.vars <- c('Store', 'Dept', 'Date', 'Year', 'Month', 'Day', 'Days', 'Day_Index', 'IsHoliday', 
              'Type', 'Size', 'Temperature', 'Fuel_Price', 'CPI', 'Unemployment',
              'MarkDown1', 'MarkDown2', 'MarkDown3', 'MarkDown4', 'MarkDown5', 'Weekly_Sales')
dfTrain <- dfTrain[,col.vars]
# note that we don't have Weekly_Sales for dfTest
dfTest <- dfTest[,col.vars[-length(col.vars)]]


#### Sort the dataframe with respect to c('Store', 'Dept', 'Date')
vars <- c('Store', 'Dept', 'Date')
## train
dfTrain <- sort_df(dfTrain, vars)
row.names(dfTrain) <- seq(1, dim(dfTrain)[1]) # just for nice display
## test
dfTest <- sort_df(dfTest, vars)
row.names(dfTest) <- seq(1, dim(dfTest)[1]) # just for nice display


#### Compute trainWeights for the convenience of training gbm
trainWeights <- rep(1, dim(dfTrain)[1])
trainWeights[dfTrain$IsHoliday==TRUE] <- 5


#### Compute test_ID for the convenience of making submission
testID <- paste(dfTest$Store, dfTest$Dept, dfTest$Date, sep='_')

# verbose
cat('Done\n')

#######################
## Generate Features ##
#######################

cat('Generate features ...\n')
cat('Totally 2 set of features ...\n')


#### Indicate which of the four holidays they are
# ------------------------------------------------------------------------------
# For those IsHoliday=1, we further indicate which of the four holidays they are
# For convenience, the holiday weeks for the four holidays are:
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13
# or in Date coding style
# Super Bowl: 2010-02-12, 2011-02-11, 2012-02-10, 2013-02-08
# Labor Day: 2010-09-10, 2011-09-09, 2012-09-07, 2013-09-06
# Thanksgiving: 2010-11-26, 2011-11-25, 2012-11-23, 2013-11-29
# Christmas: 2010-12-31, 2011-12-30, 2012-12-28, 2013-12-27
# ------------------------------------------------------------------------------
# verbose
cat('Generate feature set 1 ...\n')

Super_Bowl <- c('2010-02-12', '2011-02-11', '2012-02-10', '2013-02-08')
Labor_Day <- c('2010-09-10', '2011-09-09', '2012-09-07', '2013-09-06')
Thanksgiving <- c('2010-11-26', '2011-11-25', '2012-11-23', '2013-11-29')
Christmas <- c('2010-12-31', '2011-12-30', '2012-12-28', '2013-12-27')
Holidays <- data.frame(Super_Bowl=Super_Bowl,
                       Labor_Day=Labor_Day,
                       Thanksgiving=Thanksgiving,
                       Christmas=Christmas)
func <- function(d, Holidays){
  # Note that each column corresponding to a specific holiday, so we
  # use the column index returned by which() and then return the
  # corresponding colname
  d <- as.character(d)
  holiday <- colnames(Holidays)[which(Holidays == d, arr.ind=TRUE)[2]]
  return(holiday)
}

## train
dfTrain$Holiday <- rep('No', dim(dfTrain)[1])
dfTrain$Holiday[dfTrain$IsHoliday == TRUE] <- sapply(dfTrain$Date[dfTrain$IsHoliday == TRUE],
                                                     function(d)func(d, Holidays))
## test
dfTest$Holiday <- rep('No', dim(dfTest)[1])
dfTest$Holiday[dfTest$IsHoliday == TRUE] <- sapply(dfTest$Date[dfTest$IsHoliday == TRUE],
                                                   function(d)func(d, Holidays))
# verbose
cat('Done\n')


#### What is the last/next holiday? How many days are there from/to that?
# ------------------------------------------------------------------------------
# These features are motivated from the following aspects.
# There seems a peak/bell-shape of weekly sales located around the holiday.
# This is easy to understand.
# Before holiday comes, more and more people are going shopping, and thus the
# weekly sales before/during holiday weeks increase. And after that, weekly
# sales begin to fall.
# To capture this, we can use:
# What is the last/next holiday? How many days are there from/to that?
#
# And furthermore, to reflect the peak/bell-shape, we may want to use:
# 1) abs(days) (er...this is days itselt) or exp(-abs(days)/diversity)
# (recall a laplace distribution)
# 2) days^2 or exp(-days^2/2*sigma^2) (recall a gaussian distribution)
# Herein, we set diversity=7 (i.e., one week) and sigma=7 (i.e., two week)
#
# However, the raw days seem more helpful rather than these complex ones.
# But we just leave these in the final features as they are, and let gbm/rf to
# figure out how important they are for the forecasting task.
# ------------------------------------------------------------------------------
# verbose
cat('Generate feature set 2 ...\n')

HolidayTimeLine <- rbind(Super_Bowl,Labor_Day,Thanksgiving,Christmas)
HolidayTimeLine <- unlist(HolidayTimeLine)
# Since the most front holiday in the training data is '2010-02-12', so we have to
# insert the date of Christmas in 2009 to deal with it
Christmas_2009 <- as.character(as.Date('2010-12-31')-364)
# to know how to get 364, check: diff(as.Date(Christmas))/diff(as.Date(Labor_Day))
HolidayTimeLine <- c(Christmas_2009, HolidayTimeLine)
holiday_names <- c('Super_Bowl', 'Labor_Day', 'Thanksgiving', 'Christmas')
holiday_names <- c('Christmas', rep(holiday_names, 4))
# convert to Date class
HolidayTimeLine <- as.Date(HolidayTimeLine)

func <- function(d, HolidayTimeLine, holiday_names){
  # find the closest date to d in HolidayTimeLine
  dif <- as.numeric(d-HolidayTimeLine)
  ind <- which.min(abs(dif))
  # d comes after the closest date
  if(dif[ind] > 0){
    last_holiday_ind <- ind
  }else{
    last_holiday_ind <- ind - 1
  }  
  last_holiday <- holiday_names[last_holiday_ind]
  days_from_last_holiday <- abs(dif[last_holiday_ind])
  next_holiday <- holiday_names[last_holiday_ind+1]
  days_to_next_holiday <- abs(dif[last_holiday_ind+1])
  # while days_from_last_holiday/days_to_next_holiday are numeric, they are
  # coerced to characters due to the fact to last_holiday/next_holiday are
  # characters and the use of c() function.
  # Thus, outside of this function, we have to convert them back to numeric
  # if we want to use them in numerical computation.
  return(c(last_holiday, days_from_last_holiday,
           next_holiday, days_to_next_holiday))
}

## params for laplace and gaussian distribution that model:
# days_from_last_holiday and days_to_next_holiday
diversity <- 7
sigma <- 7

## train
dates <- as.Date(dfTrain$Date)
results <- sapply(dates, function(d)func(d, HolidayTimeLine, holiday_names))

dfTrain$Last_Holiday <- results[1,]
dfTrain$Days_From_Last_Holiday <- as.numeric(results[2,]) # convert back to numeric
dfTrain$Days_From_Last_Holiday_sqr <- dfTrain$Days_From_Last_Holiday^2
dfTrain$Days_From_Last_Holiday_laplace <- exp(-dfTrain$Days_From_Last_Holiday/diversity)
dfTrain$Days_From_Last_Holiday_gaussian <- exp(-(dfTrain$Days_From_Last_Holiday)^2/(2*sigma^2))

dfTrain$Next_Holiday <- results[3,]
dfTrain$Days_To_Next_Holiday <- as.numeric(results[4,]) # convert back to numeric
# the following features don't seem to help much?
dfTrain$Days_To_Next_Holiday_sqr <- dfTrain$Days_To_Next_Holiday^2
dfTrain$Days_To_Next_Holiday_laplace <- exp(-dfTrain$Days_To_Next_Holiday/diversity)
dfTrain$Days_To_Next_Holiday_gaussian <- exp(-(dfTrain$Days_To_Next_Holiday)^2/(2*sigma^2))

## test
dates <- as.Date(dfTest$Date)
results <- sapply(dates, function(d)func(d, HolidayTimeLine, holiday_names))

dfTest$Last_Holiday <- results[1,]
dfTest$Days_From_Last_Holiday <- as.numeric(results[2,]) # convert back to numeric
dfTest$Days_From_Last_Holiday_sqr <- dfTest$Days_From_Last_Holiday^2
dfTest$Days_From_Last_Holiday_laplace <- exp(-dfTest$Days_From_Last_Holiday/diversity)
dfTest$Days_From_Last_Holiday_gaussian <- exp(-(dfTest$Days_From_Last_Holiday)^2/(2*sigma^2))

dfTest$Next_Holiday <- results[3,]
dfTest$Days_To_Next_Holiday <- as.numeric(results[4,]) # convert back to numeric
dfTest$Days_To_Next_Holiday_sqr <- dfTest$Days_To_Next_Holiday^2
dfTest$Days_To_Next_Holiday_laplace <- exp(-dfTest$Days_To_Next_Holiday/diversity)
dfTest$Days_To_Next_Holiday_gaussian <- exp(-(dfTest$Days_To_Next_Holiday)^2/(2*sigma^2))

# verbose
cat('Done\n')

#### Convert variables to factors
factor.vars <- c('Store', 'Dept', 'Type', 'Month',
                 'IsHoliday', 'Holiday', 'Last_Holiday', 'Next_Holiday')
for(v in factor.vars){
  dfTrain[,v] <- as.factor(dfTrain[,v])
  dfTest[,v] <- as.factor(dfTest[,v])
}



###############
## Save Data ##
###############
cat('Save data ...\n')

#### We are now safe to drop variable Date
dfTrain <- dfTrain[,-which(colnames(dfTrain)=='Date')]
dfTest <- dfTest[,-which(colnames(dfTest)=='Date')]


#### save dfTrain and dfTest
save(list=c('dfTrain', 'trainWeights', 'dfTest', 'testID', 'dfSampleSubmission'),
     file='./data/training_testing_data_v1.RData')

# verbose
cat('Done\n\n')
gc(reset=TRUE)
cat('All done\n\n')