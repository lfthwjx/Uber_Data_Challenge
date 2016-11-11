library(RJSONIO)
library(dplyr)
library(ggplot2)
library(corrplot)
library(glmnet)
library(randomForest)

setwd("C:\\UM\\Others\\PL\\uber_data_challenge")
login_fileName='new_logins (1).json'
login_singleString <- paste(readLines(login_fileName), collapse=" ")
login_raw = fromJSON(login_singleString,method='C',unexpected.escape = 'skip')
login_time=login_raw[[1]]
login_time=as.POSIXlt(login_time)
login_cut=cut(login_time,seq(min(login_time),max(login_time),by=900))
login_gp=group_by(as.data.frame(login_cut),login_cut)
login_sum=summarize(login_gp,n=n())
login_sum=ungroup(login_sum)
login_sum$time=as.POSIXct(as.character(login_sum$login_cut))
ggplot(login_sum, aes(time, n)) + geom_line()

ts_login=ts(login_sum$n)
spectrum(ts_login,method='ar')
login_freq=as.data.frame(cbind(spectrum(ts_login,method='ar')$freq,
                               spectrum(ts_login,method='ar')$spec))
names(login_freq)=c('freq','density')
head(login_freq,20)
ggplot(login_sum[login_sum$time<=as.POSIXct('2010-01-21'),], aes(time, n)) + geom_line()


#typical day
#Here I choose a Tuesday, 2010-04-28 as a typical weekday to perform analysis. Basically, the curve has complementary circadian rhythms of working day: at about 8am,
#it reaches the first peak, which is also the morning peak; at about 6pm, it reaches the second peak, which is also the night peak. The rushing hours are
#are 8am-12pm and 6pm-10pm.
ggplot(login_sum[login_sum$time<=as.POSIXct('2010-04-28')&login_sum$time>as.POSIXct('2010-04-27'),], aes(time, n)) + geom_line() + geom_smooth(span=0.3)
#typical Friday
#The Friday at 2010-05-07 shows an obvious rhythm of "Friday". The peak of the morning is late than the weekdays', which is at about 12pm. And on the contrary
#of the weekdays, the curve is in an increasing trend after 7pm, lasting to the midnight given tomorrow is Friday.
ggplot(login_sum[login_sum$time<=as.POSIXct('2010-05-08')&login_sum$time>as.POSIXct('2010-05-07'),], aes(time, n)) + geom_line() + geom_smooth(span=0.3)

#typical weekend 
ggplot(login_sum[login_sum$time<=as.POSIXct('2010-05-10')&login_sum$time>as.POSIXct('2010-05-08'),], aes(time, n)) + geom_line() + geom_smooth(span=0.3)

#typical weekdays  daily cycle
#As what has been described above, in the weekdays, the valley appears in 3am-5am, and the peak appears in 6pm-7pm.
ggplot(login_sum[login_sum$time<=as.POSIXct('2010-05-08')&login_sum$time>as.POSIXct('2010-05-03'),], aes(time, n)) + geom_line() + geom_smooth(span=0.3)

#ggplot(login_sum[login_sum$time<=as.POSIXct('2010-05-10')&login_sum$time>as.POSIXct('2010-05-03'),], aes(time, n)) + geom_line() + geom_smooth(span=0.3)


fileName='uber_data_challenge.json'
singleString <- paste(readLines(fileName), collapse=" ")

uber_raw = fromJSON(singleString,method='C',unexpected.escape = 'skip')
uber_dat=NULL
for(i in 1:length(uber_raw))
{
  uber_dat=rbind(uber_dat,uber_raw[[i]])
}
uber_data=as.data.frame(uber_dat)

##convert list to proper format
uber_data[,1] = as.factor(as.character(uber_data[,1]))
uber_data[,2] = as.numeric(as.character(uber_data[,2]))
uber_data[,3] = as.Date(as.character(uber_data[,3]))
uber_data[,4] = as.numeric(as.character(uber_data[,4]))
uber_data[,5] = as.numeric(as.character(uber_data[,5]))
uber_data[,6] = as.Date(as.character(uber_data[,6]))
uber_data[,7] = as.character(uber_data[,7])
uber_data[,7][uber_data[,7]=='NULL']=NA
uber_data[,7] = as.factor(uber_data[,7])
uber_data[,8] = as.numeric(as.character(uber_data[,8]))
uber_data[,9] = as.factor(as.character(uber_data[,9]))
uber_data[,10] = as.numeric(as.character(uber_data[,10]))
uber_data[,11] = as.numeric(as.character(uber_data[,11]))
uber_data[,12] = as.numeric(as.character(uber_data[,12]))

uber_data$active=as.numeric(uber_data$last_trip_date>=as.Date('2014-06-01'))
summary(uber_data)
write.csv(uber_data,file='uber_data_full.csv')

mean(uber_data$active)

par(mar=c(10,5,3,1))
boxplot(uber_data[,c(2,4,5,8,10,11,12)],las=2,main='Box Plot for Numeric Variables')
M=cor(na.omit(uber_data[,c(2,4,5,8,10,11,12)]),use="pairwise.complete.obs")
corrplot::corrplot(M, method="circle", type="lower", diag=F)
sum(uber_data$active==0&is.na(uber_data$avg_rating_of_driver))
sum(uber_data$trips_in_first_30_days<=1&is.na(uber_data$avg_rating_of_driver))


uber_data=read.csv(file='uber_data_full.csv',header=T)
uber_data=na.omit(uber_data[,-c(1,4,7)])
##Label the outliers
uber_data$trips_in_first_30_days.high=uber_data$trips_in_first_30_days>2.61+1.5*(3-0)
uber_data$avg_rating_of_driver.low=uber_data$avg_rating_of_driver<4.6-1.5*(5-4.3)
uber_data$avg_surge.high=uber_data$avg_surge>1.074+1.5*(1.070-1)
uber_data$surge_pct.high=uber_data$surge_pct>8.899+1.5*(11.1-0)
uber_data$avg_dist.high=uber_data$avg_dist>5.46+1.5*(6.49-2.42)
uber_data$avg_rating_by_driver.low=uber_data$avg_rating_by_driver<4.776-1.5*(5-4.7)

set.seed(7)
m = dim(uber_data)[1]
training_id = sample(m,floor(0.9*m), replace = FALSE)
y=uber_data$active
y.training=y[training_id]
y.test=y[-training_id]
uber_lasso=as.data.frame(cbind(y,uber_data[,-11]))
uber_lasso = model.matrix(y ~., data = uber_lasso)
uber_lasso = as.matrix(data.frame(uber_lasso))[,-1]
training = uber_lasso[training_id,]
test = uber_lasso[-training_id,]

cvfit = cv.glmnet(x = training, y=as.factor(y[training_id]), family = 'binomial', standardize=FALSE)
plot(cvfit)
exp(coef(cvfit, s = "lambda.min"))
y.hat = predict(cvfit, test, s = "lambda.min", type = "response")
y.hat= ifelse(y.hat>0.45, 1, 0)
pred.active=y.hat
true.active=y[-training_id]
sum(y.hat==y[-training_id])/length(y.hat)

