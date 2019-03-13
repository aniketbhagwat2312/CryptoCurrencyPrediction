install.packages("zoo","imputeTS")
library(zoo)
library(imputeTS)

# read csv file
crypto_data <- read.csv('crypto-markets.csv', header=TRUE)

#check if file loaded or not
head(crypto_data)

#check column names of dataframe
colnames(crypto_data)

#volume reduction of the file
crypto_data <- crypto_data[(crypto_data$name == 'Bitcoin'|
                              crypto_data$name =='Ethereum'|
                              crypto_data$name =='Ripple'|
                              crypto_data$name =='Litecoin'|
                              crypto_data$name =='IOTA'),]
View(crypto_data)
#check format of each column
class(crypto_data)

#check data type of required columns
sapply(crypto_data, class)

#All required price variables seems to be in appropriate format except date.
#data type of open, close, low and high is numeric

#check any null value in date column
any(is.na(crypto_data$date)) # no null values in date

#convert data type of date column to Date from factor
crypto_data$date <- as.Date(crypto_data$date)

# verify column is converted into Date
class(crypto_data$date)

# verify column is converted into Date
any(is.na(crypto_data$date))
# no missing date values :)


#check for NULL values in required price variables
any(is.na(crypto_data$open))
any(is.na(crypto_data$close))
any(is.na(crypto_data$high))
any(is.na(crypto_data$low))

# each required column seems to have null values we will have to remove those
# we can remove null values by removing the row directly
# but this method will affect time series as some days will be missing
# if we delete those rows. Deleting rows is not a good option.

#to maintain mean value of column replace null with mean
crypto_data<-na.mean(crypto_data)

#check again NA removed or not
any(is.na(crypto_data$open))
any(is.na(crypto_data$close))
any(is.na(crypto_data$high))
any(is.na(crypto_data$low))

#Export cleaned data as a csv file for futher use.
write.csv(x=crypto_data, file = "cleaned_crypto_data.csv")

