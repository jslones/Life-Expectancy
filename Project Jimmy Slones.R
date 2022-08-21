#Set seed and import required packages
set.seed(100)
library(moments)
library(andrews)
#Import data (How many people survive to next 5 years given they survived last 5 years) for each country
#Transform data to represent probability of dying before each 5 year age range (0,1,5,10,15,..,100)
#Generate a set of random uniform numbers (0,1) to represent random people from the population
#Perform a Monte Carlo Simulation to fill a new matrix with ages of death from randomly generated population
MadL<-c(100000,96322.67,94550.262,93552.218,92782.983,91731.45,90540.705,89255.716,87649.263,85665.432,83222.244,80167.605,76311.484,71229.306,64081.308,53830.995,40753.487,26422.411,13627.361,5088.3389,1239.2539,180.91311)
A1<-1-(MadL/100000)
x<-runif(2000)
B1<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A1[i] & x[j]<=A1[i+1]){B1[j]<-((i-1)*5)}
    if(x[j]>=A1[22]){B1[j]<-105}
  }
}

USAL<-c(100000,99404.046,99302.631,99244.896,99175.404,98944.273,98523.154,98038.038,97474.169,96780.095,95809.183,94305.008,92001.067,88723.103,84287.886,78226.308,69719.539,58028.696,42739.245,25309.687,9979.7147,2239.2832)
A2<-1-(USAL/100000)
B2<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A2[i] & x[j]<=A2[i+1]){B2[j]<-((i-1)*5)}
    if(x[j]>=A2[22]){B2[j]<-105}
  }
}

CANL<-c(100000,99535.141,99471.836,99432.152,99381.964,99205.917,98955.969,98712.66,98435.35,98058.863,97477.832,96546.074,95084.414,92904.884,89607.091,84582.516,77047.13,65902.041,50156.746,30127.605,12035.797,2572.851)
A3<-1-(CANL/100000)
B3<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A3[i] & x[j]<=A3[i+1]){B3[j]<-((i-1)*5)}
    if(x[j]>=A3[22]){B3[j]<-105}
  }
}

FRNL<-c(100000,99650.697,99584.633,99543.953,99498.537,99366.242,99147.062,98888.239,98577.081,98125.902,97408.375,96234.797,94371.588,91631.497,87978.434,83215.607,76695.451,67110.055,52480.54,32477.683,13402.821,2927.1131)
A4<-1-(FRNL/100000)
B4<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A4[i] & x[j]<=A4[i+1]){B4[j]<-((i-1)*5)}
    if(x[j]>=A4[22]){B4[j]<-105}
  }
}

AFGL<-c(100000,93986.219,91892.123,91148.906,90574.222,89678.235,88449.1,87123.974,85631.868,83840.842,81575.918,78576.34,74437.32,68760.298,60946.866,50612.016,37619.474,23395.687,10939.22,3370.9523,610.136,60.636851)
A5<-1-(AFGL/100000)
B5<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A5[i] & x[j]<=A5[i+1]){B5[j]<-((i-1)*5)}
    if(x[j]>=A5[22]){B5[j]<-105}
  }
}

SOML<-c(100000,92054.999,86877.951,84866.254,83458.865,82000.046,80121.003,77915.105,75405.699,72444.664,69245.334,65757.799,61688.411,56641.657,50033.959,41384.522,30728.618,19232.263,9291.8064,3115.6802,647.22791,77.311648)
A6<-1-(SOML/100000)
B6<-matrix()
for(j in 1:length(x)){
  for(i in 1:21){
    if(x[j]>=A6[i] & x[j]<=A6[i+1]){B6[j]<-((i-1)*5)}
    if(x[j]>=A6[22]){B6[j]<-105}
  }
}

#Create Probability histograms using simulation data to visualize the life expectancies of the countries
hist(B1,breaks=20,prob=TRUE,main="Life Expectancy Madagascar",xlab="Age")
hist(B5,breaks=20,prob=TRUE,main="Life Expectancy Afghanistan",xlab="Age")
hist(B6,breaks=20,prob=TRUE,main="Life Expectancy Somalia",xlab="Age")
hist(B2,breaks=20,prob=TRUE,main="Life Expectancy USA",xlab="Age")
hist(B3,breaks=20,prob=TRUE,main="Life Expectancy Canada",xlab="Age")
hist(B4,breaks=20,prob=TRUE,main="Life Expectancy France",xlab="Age")

#Create a Matrix that holds the values for the descriptive statistics of each country that can be easily updated for adding new countries
BL<-rbind(B1,B5,B6,B2,B3,B4)
Dstat<-matrix(nrow=nrow(BL)+2,ncol=4)
for(i in 1:nrow(BL)){
  Dstat[i,1:4]<-c(mean(BL[i,]),sd(BL[i,]),skewness(BL[i,]),kurtosis(BL[i,]))
}
for(i in 1:4){
  Dstat[nrow(BL)+1,i]<-mean(Dstat[1:(nrow(BL)/2),i])
}
for(i in 1:4){
  Dstat[nrow(BL)+2,i]<-mean(Dstat[((nrow(BL)/2)+1):nrow(BL),i])
}
rownames(Dstat)<-c("Madagascar","Afghanistan","Somalia","USA","Canada","France","Under Developed","Developed")
colnames(Dstat)<-c("Mean","SD","Skewness","Kurtosis")
Dstat

#Create a matrix that averages together the death ages of Under-developed and Developed countries respectivley
UD<-matrix()
for(i in 1:length(x)){
  UD[i]<-mean(BL[1:(nrow(BL)/2),i])
}

DEV<-matrix()
for(i in 1:length(x)){
  DEV[i]<-mean(BL[((nrow(BL)/2)+1):nrow(BL),i])
}

#Create a histogram of the under-developed and developed countries life expectancy 
#Create a kernel density that estimates probability of death at each age
#Estimate age of death using a Weibull density function
d1<-density(UD,bw=3, kernel="gaussian",from=0)
d2<-density(DEV,bw=3, kernel="gaussian",from=0)
hist(UD,breaks=20,prob=TRUE,main="Life Expectancy of Under Developed Country",xlab="Age")
lines(d1$x,d1$y)
hist(DEV,breaks=20,prob=TRUE,main="Life Expectancy of Developed Country",xlab= "Age")
lines(d2$x,d2$y)

s=seq(0,110,.001)
hist(UD,breaks=20,prob=TRUE,main="Life Expectancy of Under Developed Country",xlab="Age")
lines(s,dweibull(s,6.2,82))

hist(DEV,breaks=20,prob=TRUE,main="Life Expectancy of Developed Country",xlab="Age")
lines(s,dweibull(s,9.7,91))

boxplot(UD,main="UnderDeveloped")
boxplot(DEV,main="Developed")

#Create a matrix that contains development metrics from each country
#life expectancy,phys per cap, literacy, GDP per cap, Development Status
MADR<-c(47,35,28,9,1)
AFGR<-c(37,46,8,15,1)
SOMR<-c(6,1,7,4,1)
USAR<-c(160,134,157,189,2)
CANR<-c(189,126,162,172,2)
FRNR<-c(192,190,161,174,2)
CountryR<-rbind(MADR,AFGR,SOMR,USAR,CANR,FRNR)
colnames(CountryR)<-c("Life Rank","Phys Rank","Literacy Rank","GDP per Capita Rank","Type")
rownames(CountryR)<-c("Madagascar","Afghanistan","Somalia","USA","Canada","France")

#Create plots that show correlation between previous metrics and life expectancy in a country
plot(CountryR[1:6,1],CountryR[1:6,2],xlab="Life Rank",ylab="Phys Rank",main="Life vs Physicians per capita",col=CountryR[1:6,5],pch=16)
plot(CountryR[1:6,1],CountryR[1:6,3],xlab="Life Rank",ylab="Literacy Rank",main="Life vs Literacy rate",col=CountryR[1:6,5],pch=16)
plot(CountryR[1:6,1],CountryR[1:6,4],xlab="Life Rank",ylab="GDP per capita rank",main="Life vs GDP per capita",col=CountryR[1:6,5],pch=16)

#Create andrews curve to show that machine learning may be applicable to predict life expectanies
andrews(CountryR,ymax=3.5,clr=5,main="Developed(Red) Under Developed(Black)")



