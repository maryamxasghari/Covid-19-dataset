#                                      CS544 Final Project 
################################################################################################################

#Picking the Data Set 
#source: https://www.kaggle.com/imdevskp/corona-virus-report
#COVID-19 Dataset
#Number of Confirmed, Death and Recovered cases every day across the globe

#Context

#A new coronavirus designated 2019-nCoV was first identified in Wuhan, the capital of China's Hubei province
#People developed pneumonia without a clear cause and for which existing vaccines or treatments were not effective.
#The virus has shown evidence of human-to-human transmission
#Transmission rate (rate of infection) appeared to escalate in mid-January 2020
#As of 30 January 2020, approximately 8,243 cases have been confirmed

#Content

#covid19clean_complete.csv - Day to day country wise no. of cases (Doesn't have County/State/Province level data)
#countrywiselatest.csv - Latest country level no. of cases
#day_wise.csv - Day wise no. of cases (Doesn't have country level data)
#usacountywise.csv - Day to day county level no. of cases
#worldometer_data.csv - Latest data from https://www.worldometers.info/

################################################################################################################
#install.packages("ggplot2")
#install.packages("UsingR") 
#install.packages("tidyverse")
#install.packages("funModeling")
#install.packages("Hmisc")
#install.packages("DescTools")


library(UsingR)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(DescTools)
library(prob)
library(sampling)
################################################################################################################

#Preparing the data 
getwd()
setwd("/Users/maryamasghari/Desktop/Bu/summer2020/R codes")

covid_19 <- read.csv("covid_19_clean_complete.csv", header=TRUE)
country_wise_latest <- read.csv("country_wise_latest.csv", header=TRUE)
day_wise <- read.csv("day_wise.csv", header=TRUE)
usa_county <- read.csv("usa_county_wise.csv", header=TRUE)
worldometer_data <- read.csv("worldometer_data.csv", header=TRUE)

#First approach to data.
#Number of observations (rows) and variables, and a head of the first cases.

#1
glimpse(covid_19)
df_status(covid_19)

#2
glimpse(country_wise_latest)
df_status(country_wise_latest)

#3
glimpse(day_wise)
df_status(day_wise)

#4
glimpse(usa_county)
df_status(usa_county)

#5
glimpse(worldometer_data)
df_status(worldometer_data)

#I did not omit NA's from tables becuase some days some countries didn't reported one thing
#but they still reported the others, 
#so I will omit NA's for each col when I will use them 

#Here is an example of the data distribution that shows a lot of outliers:
#I tried to Winsorize the data by using diffrent pecantages
#Also I should the plot that I didn't Winsorize but just don't show the outliers

par(mfrow=c(1,2))

x0 <- covid_19$Confirmed
summary(x0)
length(x0)
sd(x0)

boxplot(x0, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x0), labels = TRUE)
hist(x0, prob = TRUE, main= "Original Data")

x1 <- covid_19$Confirmed[covid_19$Confirmed != 0]

summary(x1)
length(x1)
sd(x1)

boxplot(x1, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x1), labels = TRUE)
hist(x1, prob = TRUE, main= "Zero omited")

x2 <- Winsorize(x1, probs = c(0.05,0.95),na.rm = T)

summary(x2)
length(x2)
sd(x2)

boxplot(x2, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x2), labels = TRUE)
hist(x2, prob = TRUE, main= "probs = c(0.05,0.95)")

x3 <- Winsorize(x1, probs = c(0.1,0.90),na.rm = T)

summary(x3)
length(x3)
sd(x3)

boxplot(x3, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x3), labels = TRUE)

hist(x3, prob = TRUE, main= "probs = c(0.1,0.9)")

x4 <- Winsorize(x1, probs = c(0.2,0.80),na.rm = T)

summary(x4)
length(x4)
sd(x4)

boxplot(x4, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x4), labels = TRUE)

hist(x4, prob = TRUE, main= "probs = c(0.2,0.8)")


par(mfrow=c(1,3))


boxplot(x1, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x1), labels = TRUE)

boxplot(x1, xaxt = "n",
        outline=FALSE,horizontal = T)
axis(side = 1, at =fivenum(x1) , labels = TRUE, las=2)


boxplot(x4, xaxt = "n",horizontal = T)
axis(side = 1, at = fivenum(x4), labels = TRUE, las =2)


#comparing all together

par(mfrow=c(1,6))
boxplot(x0, xaxt = "n")
boxplot(x1, xaxt = "n")
boxplot(x2, xaxt = "n")
boxplot(x3, xaxt = "n")
boxplot(x4, xaxt = "n")
boxplot(x1, xaxt = "n",
        outline=FALSE)

par(mfrow=c(1,1))

################################################################################################################
#Do the analysis as in Module 3 for at least one categorical variable 
#and at least one numerical variable. 
#Show appropriate plots and properly label the plots. (10 points) 
################################################################################################################

#Analyzing categorical variables
################################################################################################################

#Country.Region
#Pie chart

glimpse(covid_19)
df_status(covid_19)

view(covid_19)

sub1 <- subset(covid_19, WHO.Region == "Africa")
c1 <- sum(sub1$Confirmed)

sub2 <- subset(covid_19, WHO.Region == "Americas")
c2 <- sum(sub2$Confirmed)

sub3 <- subset(covid_19, WHO.Region == "Eastern Mediterranean")
c3 <- sum(sub3$Confirmed)

sub4 <- subset(covid_19, WHO.Region == "Europe")
c4 <- sum(sub4$Confirmed)

sub5 <- subset(covid_19, WHO.Region == "South-East Asia")
c5 <- sum(sub5$Confirmed)

sub6 <- subset(covid_19, WHO.Region == "Western Pacific")
c6 <- sum(sub6$Confirmed)

region <- matrix(c(c1,c2,c3,c4,c5,c6), nrow=6 )
rownames(region) <- c("Africa", "Americas", "Eastern Mediterranean","Europe","South-East Asia","Western Pacific")
colnames(region) <-c("TotalCases")
region

slice.labels <- rownames(region)
slice.percents <- round(region/sum(region)*100)
slice.labels <- paste(slice.labels, slice.percents)
slice.labels <- paste(slice.labels, "%", sep="")
pie(region, main = "Confirmed Cases",labels = slice.labels,col=rainbow(6))

#Province_State
#Bar plot

length(unique(usa_county$Province_State)) 
#58 ( Includes :"American Samoa","Guam","Northern Mariana Islands", "Puerto Rico" , "Virgin Islands")

states <- 0
j <- 1
for (i in unique(usa_county$Province_State)){
  sub <- subset(usa_county, Province_State == i)
  states[i] <- sum(sub$Confirmed)/sum(usa_county$Confirmed)
  
  cat( i , " ", states[j],"\n")
  j <- j+1
}

barplot(states, col = rainbow(58),las=2,ylim=c(0,0.25),xlab = "",
        ylab = "Frequency", main = "Cases in each state ",cex.names=0.6)
title(xlab="States", line=0, cex.lab=1)

#Analyzing Numerical Variable
################################################################################################################

#day_wise$Confirmed

par(mfrow=c(1,3))

#day_wise$Confirmed is Cumulative

#bar plot
barplot(day_wise$Confirmed, main = "Cumulative Confirmed Cases per day",
        xlab="Day", ylab = "Confirmed Case",col = "blue", las=2)

#dot chart
dotchart(day_wise$Confirmed, main = "Cumulative Confirmed Cases per day", xlab="number of Cases",ylab = "day")

# Boxplot
summary(day_wise$New.cases)
x <- day_wise$New.cases

boxplot(x, yaxt = "n", main = "New Confirmed Cases each day",col = "sky blue")
axis(side = 2, at = fivenum(x), labels = TRUE)

par(mfrow=c(1,1))

#names(day_wise)
#head(day_wise)

par(mfrow=c(1,2))

boxplot(day_wise[2:5], col = rainbow(6), main = "Total Cases")
boxplot(day_wise[6:8], col = rainbow(6), main= "New Cases")

par(mfrow=c(1,1))

# day_wise$Recovered and day_wise$New.recovered have outliers 

#finding day_wise$Recovered outliers 
y <- day_wise$Recovered
f <- fivenum(y)
boxplot(y, xaxt = "n",
        xlab = "Recovered Cases", horizontal = TRUE)
axis(side = 1, at = fivenum(y) , labels = TRUE, las=2)
text(f, rep(1.2,5), srt=90, adj= 0,
     labels=c("Min","Q1", "Median", "Q3", "Max"))

# The lower and upper ends of the outlier ranges
c(f[2] - 1.5*(f[4] - f[2]), f[4] + 1.5*(f[4] - f[2]))

y[y<f[2] - 1.5*(f[4] - f[2]) ] 
y[y>f[4] + 1.5*(f[4] - f[2]) ] 

row <- rownames(day_wise)
row[day_wise$Recovered > f[4] + 1.5*(f[4] - f[2])]
day_wise$Recovered[day_wise$Recovered > f[4] + 1.5*(f[4] - f[2])]

#tail(day_wise)

#finding day_wise$New.recovered outliers 
y <- day_wise$New.recovered
f <- fivenum(y)

boxplot(y, xaxt = "n",
        xlab = "Recovered", horizontal = TRUE, col= "sky blue")
axis(side = 1, at = f , labels = TRUE, las=2)
text(f, rep(1.2,5), srt=90, adj=0,
     labels=c("Min","Lower Hinge", "Median", "Upper Hinge", "Max"))
# The lower and upper ends of the outlier ranges
c(f[2] - 1.5*(f[4] - f[2]), f[4] + 1.5*(f[4] - f[2])) # -70018 121986

y[y<f[2] - 1.5*(f[4] - f[2]) ] #-
y[y>f[4] + 1.5*(f[4] - f[2]) ] #150922

row <- rownames(day_wise)
row[day_wise$New.recovered > f[4] + 1.5*(f[4] - f[2])] #139
day_wise$New.recovered[day_wise$New.recovered > f[4] + 1.5*(f[4] - f[2])] #150922

#printing the outlier row
day_wise[139,1:8]

################################################################################################################
#Do the analysis as in Module 3 for at least one set of two or more variables.
#Show appropriate plots for your data. (10 points)
################################################################################################################

attach(worldometer_data)

#head(worldometer_data)
#tail(worldometer_data)

North_America <- subset(worldometer_data, Continent == 'North America')
South_America <- subset(worldometer_data, Continent == 'South America')
Europe <- subset(worldometer_data, Continent == 'Europe')
Asia <- subset(worldometer_data, Continent == 'Asia')
Africa <- subset(worldometer_data, Continent == 'Africa')
Australia_Oceania  <- subset(worldometer_data, Continent == 'Australia/Oceania')

detach(worldometer_data)

x1 <- sum(North_America$TotalCases)
y1 <- sum(North_America$TotalRecovered)
z1 <- sum(na.omit(North_America$TotalDeaths))
w1 <- sum(na.omit(North_America$ActiveCases))

x2 <- sum(South_America$TotalCases)
y2 <- sum(South_America$TotalRecovered)
z2 <- sum(na.omit(South_America$TotalDeaths))
w2 <- sum(na.omit(South_America$ActiveCases))

x3 <- sum(Europe$TotalCases)
y3 <- sum(na.omit(Europe$TotalRecovered))
z3 <- sum(na.omit(Europe$TotalDeaths))
w3 <- sum(na.omit(Europe$ActiveCases))

x4 <- sum(Asia$TotalCases)
y4 <- sum(Asia$TotalRecovered)
z4 <- sum(na.omit(Asia$TotalDeaths))
w4 <- sum(na.omit(Asia$ActiveCases))

x5 <- sum(Africa$TotalCases)
y5 <- sum(Africa$TotalRecovered)
z5 <- sum(na.omit(Africa$TotalDeaths))
w5 <- sum(na.omit(Africa$ActiveCases))

x6 <- sum(Australia_Oceania$TotalCases)
y6 <- sum(Australia_Oceania$TotalRecovered)
z6 <- sum(na.omit(Australia_Oceania$TotalDeaths))
w6 <- sum(na.omit(Australia_Oceania$ActiveCases))

x <- matrix(c(x1,x2,x3,x4,x5,x6,y1,y2,y3,y4,y5,y6,z1,z2,z3,z4,z5,z6,w1,w2,w3,w4,w5,w6), nrow=4 , byrow= TRUE)
rownames(x) <- c("TotalCases", "TotalRecovered","TotalDeaths","ActiveCases")
colnames(x) <- c("North_America", "South_America", "Europe","Asia","Africa","Australia_Oceania")
x

dim(x)
dimnames(x)

Condition <- margin.table(x,1)
mosaicplot(Condition, color=rainbow(4),main = "Cases all Around the World")

addmargins(x,2) # only need to add sum for the numbers for :TotalCases,TotalRecovered,TotalDeaths 

#Recoverd/death/Active in each continent 

y <- matrix(c(y1,y2,y3,y4,y5,y6,z1,z2,z3,z4,z5,z6,w1,w2,w3,w4,w5,w6), nrow=3 , byrow= TRUE)
rownames(y) <- c( "TotalRecovered","TotalDeaths","ActiveCases")
colnames(y) <- c("North_America", "South_America", "Europe","Asia","Africa","Australia_Oceania")
y

y
dim(y)
dimnames(y)

addmargins(y,c(1,2))

mosaicplot(y, col=rainbow(6) , main = "Mosaicplot - Recoverd/Died/Sick in each continent ",las=1) 

par(mfrow=c(1,2))

#by continents
y1 <- barplot(y, xlab = "Continents", ylab = "Number", 
              beside = TRUE, legend.text = TRUE,
              main = "By Continent",ylim = c(0,1400000),cex.names=0.6,
             col=rainbow(3))

#by condition
byCases <- margin.table(y,c(2,1))

y2 <- barplot(byCases, xlab = "Condition", ylab = "Number",
              beside = TRUE, legend.text = TRUE,ylim = c(0,1400000),
              main = "By Condition", col=rainbow(6))

#stack plot
barplot(y, main = "By Continent",
        ylim=c(0,3000000),col=c("red", "blue", "green"),legend.text = TRUE, las=2 )

barplot(t(y),legend.text = TRUE,
        main = "By Condition", col=rainbow(6))

par(mfrow=c(1,1))

################################################################################################################
#Draw various random samples (using at least 3 different sample sizes) of the data 
#and show the applicability of the Central Limit Theorem for at least one variable. (15 points) 
################################################################################################################
#names(covid_19)

#raw data
x1 <- covid_19$Confirmed[covid_19$Confirmed != 0]

#length(x1)

summary(x1)
sd(x1)

samples <- 5000

par(mfrow = c(2,2))
for (size in c(10, 20, 50, 100)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(x1, size = size, 
                           replace = TRUE))
  }
  
  hist(xbar, prob = TRUE, 
       breaks = 15,
       main = paste("Sample Size =", size))

  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

#checking the answers:
sd(x1) / sqrt(c(10, 20, 50, 100))


#Winsorized data 
x4 <- Winsorize(x1, probs = c(0.2,0.80),na.rm = T)

summary(x4)
sd(x4)

samples <- 5000

par(mfrow = c(2,2))
for (size in c(10, 20, 50, 100)) {
  for (i in 1:samples) {
    xbar[i] <- mean(sample(x4, size = size, 
                           replace = TRUE))
  }
  
  hist(xbar, prob = TRUE, 
       breaks = 15,
       main = paste("Sample Size =", size))
  
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

sd(x4) / sqrt(c(10, 20, 50, 100))

#Part of raw Data
x <- subset(covid_19, Confirmed > 0 & Confirmed < 4500 )
x2 <- x$Confirmed
boxplot(x2 , horizontal = TRUE)

boxplot(x2, xaxt = "n",
        xlab = "Confirmed", horizontal = TRUE,outline=FALSE)
axis(side = 1, at =fivenum(x2) , labels = TRUE, las=2)

summary(x2)
sd(x2)

f <- fivenum(x2)
# The lower and upper ends of the outlier ranges
c(f[2] - 1.5*(f[4] - f[2]), f[4] + 1.5*(f[4] - f[2]))

lower <- y[y<f[2] - 1.5*(f[4] - f[2]) ] 
upper <- y[y>f[4] + 1.5*(f[4] - f[2]) ] 
length(lower)
length(upper)

par(mfrow = c(1,2))
samples <- 1000
sample.size <- 5

xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(x2, size = sample.size, 
                         replace = TRUE))
}

hist(xbar, prob = TRUE,  main = "Sample Size = 5")
mean(xbar)
sd(xbar)
 
samples20 <- 1000
sample.size20 <- 20

xbar20 <- numeric(samples20)
for (i in 1:samples20) {
  xbar20[i] <- mean(sample(x2, size = sample.size20, 
                           replace = TRUE))
}

hist(xbar20, prob = TRUE,  main = "Sample Size = 20")
mean(xbar20)
sd(xbar20)
par(mfrow = c(1,1))

# Compare of means and standard deviations of the above three distributions.

cat(" Original Data    ", " Mean = ", mean(x2),
    " SD = ", sd(x2), "\n",
    "Sample size = 5  ", " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n",
    "Sample size = 20 ", " Mean = ", mean(xbar20),
    " SD = ", sd(xbar20), "\n")


sd(x2) / sqrt(c(5,20))

################################################################################################################
#Show how various sampling methods (using at least 3 sampling methods) can be applied on your data. 
#What are your conclusions if these samples are used instead of the whole dataset.  (15 points). 
################################################################################################################
par(mfrow=c(2,2))

original <- table(covid_19$WHO.Region[covid_19$Confirmed != 0])
original_prec <- prop.table(original)*100
original_prec

barplot(original,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Original data set",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

data_set <- subset(covid_19, Confirmed != 0 )
nrow(data_set) #29343

# simple random sampling with replacement
set.seed(144)
s1 <- srswr(60, nrow(data_set))

rows <- (1:nrow(data_set))[s1!=0]
rows <- rep(rows, s1[s1 != 0])
rows

sample.1 <- data_set[rows, ]

t1 <- table(sample.1$WHO.Region)

barplot(t1,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Simple random sampling w replacement",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

prec.1 <- prop.table(t1)*100
prec.1

#simple random sampling without replacement
set.seed(144)

s2 <- srswor(60, nrow(data_set))

sample.2 <- data_set[s2 != 0, ]
sample.2

t2 <- table(sample.2$WHO.Region)

prec.2 <- prop.table(t2)*100
prec.2

barplot(t2,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Simple random sampling w/o replacement",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

#Systematic Sampling
set.seed(113)

N <- nrow(data_set)
n <- 60

k <- floor(N / n)
r <- sample(k, 1)

# select every kth item
s <- seq(r, by = k, length = n)

sample.3 <- data_set[s, ]

t3 <- table(sample.3$WHO.Region)

prec.3 <- prop.table(t3)*100
prec.3

barplot(t3,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Systematic Sampling",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

par(mfrow=c(1,1))

#2nd Sampling set
par(mfrow=c(3,2))

#worldometer_data
#names(worldometer_data) #209

worldometer_data <- subset(worldometer_data, Continent != '')
nrow(worldometer_data) #207

tw <- table(worldometer_data$Continent)

prec.tw <- prop.table(tw)*100
prec.tw

barplot(tw,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Original Set",cex.names=0.6)
title(xlab="Continent", line=0, cex.lab=1)

#simple random sampling without replacement
set.seed(123)

s <- srswor(60, nrow(worldometer_data))

sample.4 <- worldometer_data[s != 0, ]
sample.4

t4 <- table(sample.4$Continent)

prec.4 <- prop.table(t4)*100
prec.4

barplot(t4,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Simple random sampling w/o replacement",cex.names=0.6)
title(xlab="Continent", line=0, cex.lab=1)

#Systematic Sampling
set.seed(123)

N <- nrow(worldometer_data)
n <- 60

k <- floor(N / n)
r <- sample(k, 1)

s <- seq(r, by = k, length = n)

sample.5 <- worldometer_data[s, ]

t5 <- table(sample.5$Continent)

prec.5 <- prop.table(t5)*100
prec.5

barplot(t5,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Systematic Sampling",cex.names=0.6)
title(xlab="Continent", line=0, cex.lab=1)

#Unequal Probabilities
# UPsystematic

pik <- inclusionprobabilities(na.omit(worldometer_data$Population), 60)
length(pik)

sum(pik)

s <- UPsystematic(pik)
sample.6 <- worldometer_data[s != 0, ]

t6 <- table(sample.6$Continent)

prec.6 <- prop.table(t6)*100
prec.6

barplot(t6,las=2,col=c("red","yellow","cyan","blue","magenta"),xlab = "",ylab = "Number", main = "Unequal Probabilities",cex.names=0.6)
title(xlab="Continent", line=0, cex.lab=1)

#Stratified, unequal sized strata

order.data <- order(worldometer_data$Continent)
data <- worldometer_data[order.data, ]

freq <- table(worldometer_data$Continent)
freq

sizes <- round(60 * freq / sum(freq))
sizes
sum(sizes)

st <- strata(data,stratanames = c("Continent"),size =sizes ,method = "srswr", description = TRUE)

sample.7 <- getdata(data,st )
sample.7

t7 <- table(sample.7$Continent)

prec.7 <- prop.table(t7)*100
prec.7

barplot(t7,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Stratified, unequal sized strata",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

# Cluster
set.seed(143)

table(worldometer_data$WHO.Region)

cl <- cluster(worldometer_data, c("WHO.Region"), 
              size = 6, method="srswor", description=TRUE)

sample.8 <- getdata(worldometer_data, cl)
sample.8

t8 <- table(sample.8$Continent)

prec.8 <- prop.table(t8)*100
prec.8

barplot(t8,las=2,col=rainbow(6),xlab = "",ylab = "Number", main = "Cluster",cex.names=0.6)
title(xlab="WHO.Region", line=0, cex.lab=1)

par(mfrow=c(1,1))

prec.tw
prec.4
prec.5
prec.6
prec.7
prec.8

################################################################################################################
#Extra work (20 points)
#Implementation of any feature(s) not mentioned above
################################################################################################################
#Cumulative graph
plot(cumsum(day_wise$New.cases),ylab = "Number",xlab ="2020-01-22  to 2020-06-20 ",
     type = "l" ,col= "red", lwd = 2 , main = "Cumulative graph ,Global Day Wise Confirmed Cases")
lines(cumsum(day_wise$New.deaths),col= "blue", lwd = 2)
lines(cumsum(day_wise$New.recovered),col= "green", lwd = 2)
legend(1,8e+06,legend = c("Cases","deaths","recovered"), col = c("red","blue","green"),lty = 1,lwd=2,ncol = 1)

plot(girls$height, girls$weight, ylim=c(0,160))

plot(day_wise$New.cases,ylab = "Number",xlab ="2020-01-22  to 2020-06-20 ",
    , main = "Global Day Wise New Confirmed Cases")
lines(day_wise$New.cases,
    col= "red", lwd = 2 )
lines(day_wise$New.deaths,col= "blue", lwd = 2)
lines(day_wise$New.recovered,col= "green", lwd = 2)
legend(1,150000,legend = c("Cases","deaths","recovered"), col = c("red","blue","green"),lty = 1,lwd=2,ncol = 1)



names(worldometer_data )

t_worldometer_data <- as_tibble(worldometer_data)
dim(t_worldometer_data)
t_worldometer_data

y <- select(t_worldometer_data,Country.Region, 
            Population, TotalCases ,TotalDeaths )
dim(y)
y

#finding some information from the data: 

t_worldometer_data %>%
  transmute(cases = (TotalCases / Population)*100,
         Death = (TotalDeaths / Population) * 100)  

t_worldometer_data %>%
  summarise(delay = mean((TotalCases / Population)*100, na.rm = TRUE))
#On average %0.184 of population of countries got covid-19

t_worldometer_data %>%
  summarise(delay = mean((TotalDeaths / Population)*100, na.rm = TRUE))
#On average %0.00727 of population of countries who got covid-19 died


#Comparing Bar plot,Histogram and box plot of Number of countries per day with positive cases

barplot(day_wise$No..of.countries, main = "Number of countries per day",
        xlab="Date : 2020-01-22 to 2020-06-20", ylab = "Number of countries",col = rainbow(151), las=2, ylim=c(0,200))

x <- hist(day_wise$No..of.countries, col=hcl(0), ylim=c(0,80))

names(x)
x$breaks
x$counts

par(mfrow=c(1,2))

x1 <- hist(day_wise$No..of.countries, breaks=seq(0,200,2), 
           col=rainbow(100), ylim=c(0,40),main = "100 breaks",xlab = "Number of countries")
x2 <- hist(day_wise$No..of.countries, breaks=10, 
           col=rainbow(10), ylim=c(0,80),main = "10 breaks",xlab = "Number of countries")

par(mfrow=c(1,1))

nc <- day_wise$No..of.countries
boxplot(nc,xaxt = "n",horizontal = TRUE,xlab = "Number of countries")
axis(side = 1, at =fivenum(nc) , labels = TRUE, las=2)

summary(nc)

#finding some information from the data: 

worldometer_data[c(2,3,4,6,8)]

options(digits=2)

#% of population got Covid _19
for (i in 1:length(unique(worldometer_data$Continent))){
  x <- (sum(worldometer_data$TotalCases[i])/sum(worldometer_data$Population[i]))*100
  y <- unique(worldometer_data$Continent)[i]
  cat( x, "% of ", y , "poulation tested positive for covid_19;","\n")
}

#% of population got Covid _19
x1 <- barplot(m[1,], ylab = "%", las=2,
              main = "% of population got Covid _19",col=rainbow(7))

#%Got Covid", "% of them died", "% got recovered", "% are still sick
for (i in 1:length(unique(worldometer_data$Continent))){
  x <- (sum(worldometer_data$TotalDeaths[i])/sum(worldometer_data$TotalCases[i]))*100
  y <- (sum(na.omit(worldometer_data$TotalRecovered)[i])/sum(worldometer_data$TotalCases[i]))*100
  k <- (sum(na.omit(worldometer_data$ActiveCases)[i])/sum(worldometer_data$TotalCases[i]))*100
  z <- unique(worldometer_data$Continent)[i]
  w <- 100 -(x+y+k)
  cat( "In", z,",", x, "% of people who got covid_19 died,", y,"% recovered,",k,"% are still sick, and condition of",w,"% are unknown." ,"\n")
}

m = matrix(data = NA , nrow = 5, ncol = 7)
rownames(m) <- c("%Got Covid", "% Died", "% Recovered", "% Still sick" , "% Unknown")
colnames(m) <- c("World", "North_America", "South_America", "Europe","Asia","Africa","Australia_Oceania")
m

#calculating data for the world 
head(worldometer_data)
m[1,1] <- (sum(worldometer_data$TotalCases)/sum(worldometer_data$Population))*100
m[2,1] <- (sum(na.omit(worldometer_data$TotalDeaths))/sum(na.omit(worldometer_data$TotalCases)))*100
m[3,1] <- (sum(na.omit(worldometer_data$TotalRecovered))/sum(na.omit(worldometer_data$TotalCases)))*100
m[4,1] <- (sum(na.omit(worldometer_data$ActiveCases)[i])/sum(worldometer_data$TotalCases[i]))*100
m[5,1] <- 100 -(sum(m[2:4,1]))
m

#calculating data for each continent
for (i in 1:length(unique(worldometer_data$Continent))){
  x <- (sum(worldometer_data$TotalCases[i])/sum(worldometer_data$Population[i]))*100
  y <- (sum(worldometer_data$TotalDeaths[i])/sum(worldometer_data$TotalCases[i]))*100
  z <- (sum(na.omit(worldometer_data$TotalRecovered)[i])/sum(worldometer_data$TotalCases[i]))*100
 k <-  (sum(na.omit(worldometer_data$ActiveCases)[i])/sum(worldometer_data$TotalCases[i]))*100
  w <- 100 -(y+z+k)
  m[1,i+1] <- x
  m[2,i+1] <- y
  m[3,i+1] <- z
  m[4,i+1] <- k
  m[5,i+1] <- w
}
m

# "% of people with covid died", "% got recovered", "% are still sick
par(mfrow=c(1,3))

x2 <- barplot(m[2,], ylab = "%",las=2,
              main = "%of who got Covid Died", col=rainbow(7),ylim = c(0,60))

x3 <- barplot(m[3,], ylab = "%",las=2,
              main = "%of who got Covid Recoverd", col=rainbow(7),ylim = c(0,60))

x4 <- barplot(m[4,], ylab = "%",las=2,
              main = "%of who got Covid are still sick", col=rainbow(7),ylim = c(0,60))

par(mfrow=c(1,1))

# min and max ratio of death
max <- max(m[2,])
min <- min(m[2,])

for (i in 1:7){
  if (m[2,i] == max){
  cat (colnames(m)[i], "with ",max,"% has the most death rate between who got Covid_19")}
}

for (i in 1:7){
  if (m[2,i] == min){
    cat (colnames(m)[i], "with ",min,"% has the least death rate between who got Covid_19")}
}

#pie chart for above calculations: 
slice.labels <- row.names(m)[2:5]
slice.percents <- round(m[2:5,1])
slice.labels <- paste(slice.percents, slice.labels)
slice.labels
pie(m[2:5,1],main = "World",  labels = slice.labels, col=rainbow(4))

par(mfrow=c(2,3))
slice.labels <- row.names(m)[2:5]

for (i in 2:7){
  pie(m[2:5,i],main = colnames(m)[i],  labels =( paste(round(m[2:5,i]), slice.labels)), col=rainbow(4))  
}
par(mfrow=c(1,1))

#Date which has the maximum number of new reported cases during studied time period

for ( i in 1:nrow(day_wise)){
  if (day_wise$New.cases[i] == max(day_wise$New.cases)){
    cat("Maximum number of new reported cases during studied time period was on" , day_wise$Date[i], " which was",day_wise$New.cases[i] ,"cases.")
  }
}

barplot(day_wise$New.cases, main = "Number of reported New cases",
        xlab="Date : 2020-01-22 to 2020-06-20", ylab = "Number of cases",col = rainbow(151), las=2,ylim = c(0,200000))

#Date which has the maximum number of new reported death during studied time period
for ( i in 1:nrow(day_wise)){
  if (day_wise$New.deaths[i] == max(na.omit(day_wise$New.deaths))){
    cat("Maximum number of new reported death during studied time period was on" , day_wise$Date[i], " which was",day_wise$New.deaths[i] ,"cases.")
  
    }
}

barplot(day_wise$New.deaths, main = "Number of reported New Deaths",
        xlab="Date : 2020-01-22 to 2020-06-20", ylab = "Number of Deaths",col = rainbow(151), las=2,ylim = c(0,10000))

#names(country_wise_latest)

# country with largest number of cases:
for ( i in 1:nrow(country_wise_latest )){
  if (country_wise_latest $Confirmed[i] == max(country_wise_latest $Confirmed)){
    cat(country_wise_latest$Country.Region[i], "had the largest number of cases which is :",country_wise_latest$Confirmed[i] ,"reported cases.")
  }
}

# country with largest number of death:
for ( i in 1:nrow(country_wise_latest)){
  if (country_wise_latest$Deaths[i] == max(country_wise_latest$Deaths)){
    cat(country_wise_latest$Country.Region[i], "had the largest number of death which is :",country_wise_latest$Deaths[i] ,"reported deaths.")
  }
}

# country with highest ratio of death per 100 case:
for ( i in 1:nrow(country_wise_latest)){
  if (country_wise_latest$Deaths...100.Cases[i] == max(country_wise_latest$Deaths...100.Cases)){
    cat(country_wise_latest$Country.Region[i], "has the largest number of death:",country_wise_latest$Deaths...100.Cases[i] ,"per 100 cases.")
  }
}

par(mfrow=c(4,1))

#Countries with largest number of cases
decreasing <- country_wise_latest[order(country_wise_latest$Confirmed, decreasing = TRUE), ]
boxplot(country_wise_latest$Confirmed,horizontal = T, main = "Country wise - Number of Confirmed cases", col = "red")

summary(country_wise_latest$Confirmed)
#Top 10 countries  with largest number of cases
top.10 <- subset(decreasing[c(1:10),])
boxplot(top.10$Deaths,horizontal = T, main = "Top 10", col = "red")

top.10 [ , c(1,2,15)]

#Top 100 countrieswith largest number of confiermed cases
top.100 <- subset(decreasing[c(1:100),])
boxplot(top.100$Deaths,horizontal = T, main = "Top 100", col = "red")

#Bottom 10 countries  with lowest number of confiermed cases
bottom.10 <- subset(decreasing[c(177:187),])
boxplot(bottom.10$Confirmed,horizontal = T, main = "Bottom 10", col = "red")

bottom.10[ , c(1,2,15)]

par(mfrow=c(1,1))

#Top 10 countries with largest number of Deaths
decreasing2 <- country_wise_latest[order(country_wise_latest$Deaths, decreasing = TRUE), ]

top.10.Deaths <- subset(decreasing2[c(1:10),])

top.10.Deaths[ , c(1,3,15)]

#Top 10 countries with largest number of Deaths per 100
decreasing3 <- country_wise_latest[order(country_wise_latest$Deaths...100.Cases, decreasing = TRUE), ]

top.10.Deaths.per100 <- subset(decreasing3[c(1:10),])

top.10.Deaths.per100[ , c(1,9,15)]

#Top 10 countries with largest number of Deaths
m2 = matrix(data = NA , nrow = 2, ncol = 10)
rownames(m2) <- c("TotalCases","TotalDeaths")
colnames(m2) <- top.10.TotalDeaths$Country.Region

m2[1,] <- top.10.TotalDeaths$TotalCases
m2[2,] <- top.10.TotalDeaths$TotalDeaths

m2

par(mfrow=c(1,3))

barplot( m2[2,] , ylab = "Count" ,
         las=2, main = "TotalDeaths",
         col="blue")

barplot(m2, ylab = "Count", 
        beside = TRUE, legend.text = TRUE,las=2,
        main = "Total Cases / Total Deaths",sub = "Top 10 countries with largest number of Deaths",
        col=c("red", "blue"), ylim = c(0,2500000))

mosaicplot(m2,las = 1,col=rainbow(10),main = "Total Cases / Total Deaths" )

par(mfrow=c(1,1))


