#Use read.table to read the data into R
storm.data <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA3/STAT0030_Lab8&9_GLM/nstorms.dat",header = T)
nino.data <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA3/STAT0030_Lab8&9_GLM/nino3.dat",header = T)

#By adding 1 to the years in nino.data frame before merging, we match the storm data.
nino.data$Year <- nino.data$Year + 1
storm.data <- merge(storm.data,nino.data)

#scatterplot
plot(storm.data$Storms,storm.data$N3.m09)

#storm.model1
storm.model1 <-glm(Storms ~ N3.m09,family=poisson(link="log"),data=storm.data)
summary(storm.model1)
#see the correlation be- tween the estimated coefficients
summary(storm.model1,correlation=TRUE)
