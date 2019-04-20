###City Bikes Data Analysis
# Kalindi Deshmukh
# Rupal Sinha
# Alejandro G

############

#Load different dataset and join them
jan16 <- X201601_citibike_tripdata 

set.seed(123)
samp <- sample(1:nrow(jan16), 7000)
jan16= jan16[samp,]




set.seed(123)
samp <- sample(1:nrow(X201602_citibike_tripdata), 7000)
X201602_citibike_tripdata= X201602_citibike_tripdata[samp,]

set.seed(123)
samp <- sample(1:nrow(X201603_citibike_tripdata), 7000)
X201603_citibike_tripdata= X201603_citibike_tripdata[samp,]

set.seed(123)
samp <- sample(1:nrow(X201604_citibike_tripdata), 7000)
X201604_citibike_tripdata= X201604_citibike_tripdata[samp,]


Q = rbind(jan16,X201602_citibike_tripdata,X201603_citibike_tripdata,X201604_citibike_tripdata)
write.csv(Q,"Q.csv")
#We do not hav zipcode so we would generate it with revgeo function using latitude and longitude

#!!!!!! PLEASE DO NOT RUN THIS LINE OF CODE AS IT TAKES OVERNIGHT TO RUN THIS !!!!!!!!!
#!!!!!!  EACH ROW TAKES 1 sec on an AVERAGE TO EXECUTE, adderss is already stored and added to dataset
#load the downloaded data

library(readr)
Q1 <- read_csv("~/Q1.csv")
Q1 <- read_csv("~/Q1.csv")
FULLRIDER= read.csv("C:\Users\91886\Downloads\FULLRIDER.csv")
View(Q1)

#We do not hav zipcode so we would generate it with revgeo function using latitude and longitude

#!!!!!! PLEASE DO NOT RUN THIS LINE OF CODE AS IT TAKES OVERNIGHT TO RUN THIS !!!!!!!!!
#!!!!!!  EACH ROW TAKES 1 sec on an AVERAGE TO EXECUTE, adderss is already stored and added to dataset 
addressQ1 <- revgeo(Q1$`start station longitude`, Q1$`start station latitude`)

#extracting only the zipcode and storing it in a seperate column
zip <- sapply(strsplit(as.character(addressQ1), ","), "[", 4)
Q$zipcode <- zip


# take sample and subset it
Q1=Q[c(1:9159),]
full_tripdata=Q1
View(full_tripdata)
#splitting the dates column

full_tripdata=Q1


#split startdate and stopdate into date and time
full_tripdata = separate(full_tripdata, starttime, into=c("startday","starttime"), sep=" ")
full_tripdata = separate(full_tripdata, stoptime, into=c("stopday","stoptime"), sep=" ")
View(full_tripdata)
#full_tripdata$Date = as.Date(full_tripdata$Date)
str(full_tripdata)
a <- as.Date(full_tripdata$Date)
a

#converting to proper format - date type
full_tripdata$Date <- as.Date(full_tripdata$startday, format="%m/%d/%Y")


full_riderdata$Date <- as.Date(full_riderdata$Date, format="%m/%d/%Y")


# omit missing values
full_tripdata=na.omit(full_tripdata)

#compute age based on birth year
full_tripdata$age = as.integer(format(Sys.Date(), "%Y")) - as.integer(full_tripdata$`birth year`)
max(full_tripdata$age)
#It is unbelievable and it shows weakness of data

#removing outliers in age section 
full_tripdata= full_tripdata[full_tripdata$age<=90,]


#merge the datasets of full trip data and full rider data
final <- merge(full_tripdata,FULLRIDER, by="Date")


View(final)
write.csv(final,"final.csv")

# compute day of the week
dayofweek1 <- weekdays(final$Date)
dayofweek1
final$WeekDay <- dayofweek1
table(final$WeekDay)
#  Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
#1780       983      1053       902      1433      1129      1305 


#Plot the days via ggplot
ggplot(finalData, aes(x = WeekDay)) + geom_bar()

# compute distance distance using imap

library("Imap")
d<-dim(final)[1]
dist<-rep(0,d)
for (i in 1:d){
  cat("itearation ",i," ")
  dist[i]<-gdist(final$`start station longitude`[i],final$`start station latitude`[i],
                 final$`end station longitude`[i],final$`end station latitude`[i],
                 units="m")}
finaldummy<-final
finaldummy$distance<-dist
final$distance = dist

#compute speed
finaldummy$speedkmh<-finaldummy$distance/finaldummy$tripduration

final$speedkmh<-final$distance/final$tripduration
hist(final$distance,breaks = 100, col = blues9)
hist(final$speedkmh,breaks = 50, col = blues9)


#final= na.omit(final)

#Data Type changes
final$Miles.traveled.to.date.= as.numeric(final$Miles.traveled.to.date.)
final$tripduration = as.numeric(final$tripduration)

# Removing outliers based on age above 90
final$age = as.integer(format(Sys.Date(), "%Y")) - as.integer(final$birth)
max(final$age)
final= final[final$age<=90,]

#timeslot
dd <-gsub(":", "", final$starttime) # remember to reassign the processed output and convert to the correct data type if needed 
dd <- as.numeric(dd)
dd
final$timeComp <- dd

final$Timeslot[final$timeComp<020000]<-"TimeSlot 1"
final$Timeslot[final$timeComp>=020000 & final$timeComp<040000]<-"TimeSlot 2"
final$Timeslot[final$timeComp>=040000 & final$timeComp<060000]<-"TimeSlot 3"
final$Timeslot[final$timeComp>=060000 & final$timeComp<080000]<-"TimeSlot 4"
final$Timeslot[final$timeComp>=080000 & final$timeComp<100000]<-"TimeSlot 5"#
final$Timeslot[final$timeComp>=100000 & final$timeComp<120000]<-"TimeSlot 6"
final$Timeslot[final$timeComp>=120000 & final$timeComp<140000]<-"TimeSlot 7"
final$Timeslot[final$timeComp>=140000 & final$timeComp<160000]<-"TimeSlot 8"
final$Timeslot[final$timeComp>=160000 & final$timeComp<180000]<-"TimeSlot 9"#
final$Timeslot[final$timeComp>=180000 & final$timeComp<200000]<-"TimeSlot 10"#
final$Timeslot[final$timeComp>=200000 & final$timeComp<220000]<-"TimeSlot 11"
final$Timeslot[final$timeComp>=220000 & final$timeComp<240000]<-"TimeSlot 12"


table(final$Timeslot)
# TimeSlot 1 TimeSlot 10 TimeSlot 11 TimeSlot 12  TimeSlot 2  TimeSlot 3  TimeSlot 4  TimeSlot 5  TimeSlot 6 
#101        1324         599         278          34          78         635        1375         757 
#TimeSlot 7  TimeSlot 8  TimeSlot 9 
#832         981        1458 








# As the software does not provide zipcode for all locations ,we considered only those where the zipcode was available
# According to our observation , we found that R Studio stops generating zipcodes after around 10000 iterations

# Histogram with density plot of Total Revenue
ggplot(final, aes(x=final$Total.Revenue)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+  geom_vline(aes(xintercept=mean(final$Total.Revenue)),
                                                   color="blue", linetype="dashed", size=1)
fsa= scale(final$Total.Revenue, center = TRUE, scale = TRUE)
final$Total.Revenue =fsa
final= final[final$Total.Revenue <1,]
#after scaling
ggplot(final, aes(x=final$Total.Revenue)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+  geom_vline(aes(xintercept=mean(final$Total.Revenue)),
                                                   color="blue", linetype="dashed", size=1)
# after removing outliers , it is much more evenly distributed

# Histogram with density plot of miles travelled
ggplot(final, aes(x=final$Miles.traveled.today..midnight.to.11.59.pm.)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")
#it is skewed, so we try to scale it for better analysis

kk=scale(final$Miles.traveled.today..midnight.to.11.59.pm.)
plot(density(kk))
min(kk)
final$Miles.traveled.today..midnight.to.11.59.pm.=kk
final = final[final$Miles.traveled.today..midnight.to.11.59.pm.<3,]

ggplot(final, aes(x=final$Miles.traveled.today..midnight.to.11.59.pm.)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")

ggplot(final, aes(x=final$Miles.traveled.today..midnight.to.11.59.pm.)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")
# The graph shows that the values are somewhat balanced

#trip duration
ggplot(final, aes(x=final$tripduration)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")
max(final$tripduration)
#too skewed beacause of unsual time duratiins such as 10000 and above, 
final= final[final$tripduration<10000,]
#Now the data is not much skewed
final$tripduration= scale(final$tripduration)
ggplot(final, aes(x=final$tripduration)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")


final$age = scale(final$age)
ggplot(final, aes(x=final$age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")

#gender 0 is unknown , so not required
table(final$gender)
final= final[final$gender!=0,]


# Create overage variable to indicate if a ride resulted in an overage
final$overage <- "FALSE"
final$overage[final$usertype == "Subscriber" & final$tripduration > 2700] <- "TRUE"
final$overage[final$usertype == "Customer" & final$tripduration > 1800] <- "TRUE"
prop.table(table(final$overage))



# Calculate average trip time per station where the rider is subscriber
subscriber_data <- subset(full_tripdata, usertype=="Subscriber")
average_trip_duration_station_sub <- aggregate(tripduration~subscriber_data$`start station name`, data=subscriber_data, FUN=function(x) c(mean=mean(x)))
View(average_trip_duration_station_sub)
# Calculate average trip time per station where the rider is Customer
customer_data <- subset(full_tripdata, usertype=="Customer")
average_trip_duration_station_cus <- aggregate(tripduration~customer_data$`start station name`, data=customer_data, FUN=function(x) c(mean=mean(x)))
View(average_trip_duration_station_cus)


# Create round-trip variable to indicate if a ride was a round-trip to the same station
final$roundtrip <- "FALSE"
final$roundtrip[final$Start_Station_ID == final$End_Station_ID] <- "TRUE"
prop.table(table(final$roundtrip))

#converting datatype
str(final)
final$zipcode = as.factor(final$zipcode)
final$`start station id`=as.factor(final$`start station id`)
final[, c(7,8,11,12,15,16,17,18,33,36,38,40)] <- lapply(final[, c(7,8,11,12,15,16,17,18,33,36,38,40)], as.factor)
str(final)

f1=finalData
finalData =final

finalData = finalData[, -c(7,11,13,14,17,20,22,24,25,37,39,40)]
model1 = lm(finalData$Total.Revenue~., data=finalData)

#now dat is ready for analysis

##################################################################################################

#Visualizations
#for Weekday
ggplot(finalData, aes(x = WeekDay)) + geom_bar()

#for Zipcode
ggplot(finalData, aes(x = zipcode)) + geom_bar()

#for Timeslots
ggplot(finalData, aes(x = Timeslot)) + geom_bar()

#for miles travelled on current date
ggplot(finalData, aes(x=finalData$Miles.traveled.today..midnight.to.11.59.pm.)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")

# Histogram with density plot of Revenue
ggplot(finalData, aes(x=finalData$Total.Revenue)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+  geom_vline(aes(xintercept=mean(finalData$Total.Revenue)),
                                                   color="blue", linetype="dashed", size=1)


#for tripduration
ggplot(finalData, aes(x=finalData$tripduration)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")

#


#############################################################################
############ BIVARIATE ANALYSIS
#######################################Factor- Numeric###########################################
#Revenue wrt zip codes
#aggregate(finalData$Total.Revenue ~ finalData$zipcode, data = finalData, FUN="mean", na.rm=T)
#est_modelDD <- aov(log(Total.Revenue+1)~zipcode, data=finalData)
summary(est_modelDD) # correlated!

# Plot
boxplot(Total.Revenue ~ zipcode, data=finalData, main="Revenue wrt ZipCode", 
        xlab="Zip Codes", ylab="Revenue", col=c("orange", "steelblue")) #almost similar C

#Gender
aggregate(finalData$Total.Revenue ~ finalData$gender, data = finalData, FUN="mean", na.rm=T)
est_modelDD <- aov(log(Total.Revenue+1)~gender, data=finalData)
summary(est_modelDD) #no astericks...not correlated!

# Plot
boxplot(Total.Revenue ~ gender, data=finalData, main="Revenue wrt gender", 
        xlab="gender", ylab="Revenue", col=c("orange", "steelblue")) #almost similar 

#Weekday
aggregate(finalData$Total.Revenue ~ finalData$WeekDay, data = finalData, FUN="mean", na.rm=T)
est_modelDD <- aov(log(Total.Revenue+1)~finalData$WeekDay, data=finalData)
summary(est_modelDD) #highly correlated!

# Plot
boxplot(Total.Revenue ~ WeekDay, data=finalData, main="Revenue wrt Week Day", 
        xlab="Week Day", ylab="Revenue", col=c("orange", "steelblue")) #

#Timeslot
aggregate(finalData$Total.Revenue ~ finalData$Timeslot, data = finalData, FUN="mean", na.rm=T)
est_modelDD <- aov(log(Total.Revenue+1)~finalData$Timeslot, data=finalData)
summary(est_modelDD) #highly correlated!

# Plot
boxplot(Total.Revenue ~ Timeslot, data=finalData, main="Revenue wrt Time Slot", 
        xlab="Week Day", ylab="Revenue", col=c("orange", "steelblue")) #
###########################################################


###################################Numeric Variables Bivariate##############################################


#Numeric Variables

#Trip Duration
plot(finalData$tripduration~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, 
     main= "Revenue wrt TripDuration", xlab="Revenue", ylab= "Trip Duration")
abline(lm(log(tripduration)~log(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$tripduration, finalData$Total.Revenue) 

#cor 
#0.04327174 

#24 hours passes purchased
plot(finalData$X24.Hour.Passes.Purchased..midnight.to.11.59.pm.~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, 
     main= "Revenue wrt 24 hours passes purchased", xlab="Revenue", ylab= "24 hours passes purchased")
abline(lm((X24.Hour.Passes.Purchased..midnight.to.11.59.pm.)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$X24.Hour.Passes.Purchased..midnight.to.11.59.pm., finalData$Total.Revenue)
#cor 
#0.8968604 

#Miles travelled
plot(finalData$Miles.traveled.today..midnight.to.11.59.pm.~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, main= "Revenue wrt Miles travelled", xlab="Revenue", ylab= "Miles travelled")
abline(lm((Miles.traveled.today..midnight.to.11.59.pm.)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$Miles.traveled.today..midnight.to.11.59.pm., finalData$Total.Revenue)
# cor 
#0.6147264 

plot(finalData$X3.Day.Passes.Purchased..midnight.to.11.59.pm.~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, main= "Revenue wrt 3 day passes purchased", xlab="Revenue", 
     ylab= "3 days passes purchased")
abline(lm((X3.Day.Passes.Purchased..midnight.to.11.59.pm.)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$X3.Day.Passes.Purchased..midnight.to.11.59.pm., finalData$Total.Revenue)

#cor 
#-0.1503073 

plot(finalData$Annual.members.added~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, main= "Revenue wrt Annual Members Subscriptions", xlab="Revenue", 
     ylab= "Annual.members.added")
abline(lm((Annual.members.added)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$Annual.members.added, finalData$Total.Revenue)
#cor 
#0.9110837 

plot(finalData$speedkmh~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, main= "Revenue wrt Speed", xlab="Revenue", 
     ylab= "Speed km/hr")
abline(lm((speedkmh)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$speedkmh, finalData$Total.Revenue)

# cor: -0.009266251 
finalData$age<- as.numeric(finalData$age)


plot(finalData$age~finalData$Total.Revenue, col="steelblue", pch=20, cex=0.75, main= "Revenue wrt Age", xlab="Revenue", 
     ylab= "Speed km/hr")

abline(lm((age)~(Total.Revenue), data=finalData), col="red", lwd=2)
cor.test(finalData$age, finalData$Total.Revenue)

#cor 
#-0.05106508

#####################################################



#PCA
final_s1= finalData[,-c(1,3,4,5,6,7,10,11,12,13,14,24,28)]
str(final_s1)
final_ss1 <- data.frame(scale(final_s1))
head(final_ss)
final_s1$age= as.numeric(final_s1$age)
fa.parallel(final_ss1, fa="both", n.iter=100, show.legend=F) # legend was in the way. now we see the "elbow" 
#Parallel analysis suggests that the number of factors =  8  and the number of components =  6 

#data_fa1 <- factanal(final_ss1, 6) 
#brandfa1$loadings


mod1 <- prcomp(final_ss1)
mod1 # note the "direction" as changes in different PCs
names(mod1)
summary(mod1)# for example, x is for rows  
mod1$x
print(mod1)
# there should be no correlation among PCs 
# is there in ours? 

pc1= princomp(final_ss1,cor=TRUE, score= TRUE)
attributes(pc1)
pc1$loadings

corrplot(cor(mod1$x)) 
plot(mod1, type="l", main="PCA and Captured Variance in the  Data", ylim=c(0,9), pch=20) 
abline(h=1, col="steelblue", lwd=2.5)

fa.parallel(final_ss1, fa="pc", n.iter=100)
abline(h=1, col="steelblue", lwd=2.5)


library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
pca.data <- data.frame(Sample=rownames(mod1$x),X=mod1$x[,1], Y=mod1$x[,2])
pca.data

#ggplot(data=pca.data, aes(x=X, y=Y,label=Sample)) + geom_text() + xlab(paste("PC1", pca.var.per[1],"%",sep=" ")) + ylab(paste("PC2", pca.var.per[2],"%",sep=" "))

pca2 = princomp(final_ss, cor = TRUE)
pca2$sdev
# loadings
#unclass(pca2$loadings)


#PLot the PCA findings
ggbiplot(mod1)
                                                     
######################################
#OLS regression modelling
modF <- lm(Total.Revenue~ WeekDay, data=finalData)
summary(modF)
#Multiple R-squared:  0.1691,	Adjusted R-squared:  0.1684

modF2 <- lm(Total.Revenue~ WeekDay+tripduration, data=finalData)
summary(modF2)
#Multiple R-squared:  0.1708,	Adjusted R-squared:   0.17

modF3 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode, data=finalData)
summary(modF3)
#Multiple R-squared:  0.1757,	Adjusted R-squared:  0.1711 

modF4 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`, data=finalData)
summary(modF4) 
#Multiple R-squared:  0.2385,	Adjusted R-squared:  0.1864

modF5 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender, data=finalData)
summary(modF5) 
#Multiple R-squared:  0.2387,	Adjusted R-squared:  0.1865

modF6 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot, data=finalData)
summary(modF6) 
#Multiple R-squared:  0.2407,	Adjusted R-squared:  0.1874 

modF7 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot+age, data=finalData)
summary(modF7) 
#Multiple R-squared:  0.2414,	Adjusted R-squared:  0.188


modF9 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot+age
            +X3.Day.Passes.Purchased..midnight.to.11.59.pm., data=finalData)
summary(modF9) 
#Multiple R-squared:  0.3018,	Adjusted R-squared:  0.2525 

modF10 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot+age
             +X3.Day.Passes.Purchased..midnight.to.11.59.pm. +X24.Hour.Passes.Purchased..midnight.to.11.59.pm., data=finalData)
summary(modF10) 
#Multiple R-squared:  0.932,	Adjusted R-squared:  0.9272 #Now that is an improvement!


modF11 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot
             +X3.Day.Passes.Purchased..midnight.to.11.59.pm. +X24.Hour.Passes.Purchased..midnight.to.11.59.pm.
             +Miles.traveled.today..midnight.to.11.59.pm., data=finalData)
summary(modF11) 

#Multiple R-squared:  0.9531,	Adjusted R-squared:  0.9498 #Important!

modF12 <- lm(Total.Revenue~ WeekDay+tripduration+zipcode+`start station name`+gender+Timeslot
             +X3.Day.Passes.Purchased..midnight.to.11.59.pm. +X24.Hour.Passes.Purchased..midnight.to.11.59.pm.
             +Miles.traveled.today..midnight.to.11.59.pm.+speedkmh, data=finalData)
summary(modF12)
#No change

#So we will go with the Model 11

##########################################################3


# Clustering with factors 
##############################

   

# Data wrangling (to convert to factors)


fs1 <- final_ss1 # use standardized variables 
fs11 = finalData[,c(1,3,4,5,6,7,10,11,12,13,14,24,28)]
fss= cbind(fs1,fs11)
ft= fss[,-c(16:20)]
#glimpse(mta)

# Distance matrix 
#mts = sample(1:nrow(mt), 10000, replace=TRUE)
#mta=mt[mts,]
#mt_gowers <- daisy(mta,metric = "gower")



# Model 
#mt_daisy <- hclust(mt_gowers, method="ward.D2")

#summary(mt_daisy)


str(final_s)
gower_dist <- daisy(ft[, -18],
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)


# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
final_fit <- pam(gower_dist, diss = TRUE, k = 6)


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data1 <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(final_fit$clustering),
         name = ft$`start station name`)


#plot the cluster
ggplot(aes(x = X, y = Y), data = tsne_data1) + geom_point(aes(color = cluster))



LES = as.numeric(geocode("Lower East Side"))
MANHATTAN = ggmap(get_googlemap(center=LES, scale=1, zoom=12), extent="normal")
MANHATTAN <- MANHATTAN + 
  geom_point(aes(x=lon, y=lat), data=trips.sum, col="red", alpha=.5, size=trips.sum$size) +
  ggtitle("Citi Bike Stations By Starting Point Popularity (Q1)") + 
  theme(plot.title=element_text(family="Times", face="bold", size=40))
print(MANHATTAN)
