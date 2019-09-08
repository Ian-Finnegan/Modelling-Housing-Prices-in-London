---
output: pdf_document
---

library(MASS)
library(rgdal)
library(rgeos)
library(corrplot)
library(classInt)
library(stats)
library(GWmodel)

# The raw data
LondonData <- read.csv("DataScienceProj.csv",stringsAsFactors=FALSE)

# Function to convert dummy variables to factor variable
Dummy2Factor <- function(mat,lev1="Level1") {
      mat <- as.matrix(mat)
      factor((mat %*% (1:ncol(mat))) + 1,
          labels = c(lev1, colnames(mat)))
}

# Converting dummy variable to factors
Age <- Dummy2Factor(LondonData[,5:9],"PreWW1")
Type <- Dummy2Factor(LondonData[,10:12],"Others")
Garage <- Dummy2Factor(LondonData[,13:14],"HardStnd")
Bedrooms <- Dummy2Factor(LondonData[,18:21],"BedOne")

par(mfrow=c(1,2))
LondonDataNew <- data.frame(LondonData[,c(2:4,15:17,22,23:31)],Age,Type,Garage,Bedrooms) # New data
corrplot::corrplot(cor(LondonDataNew[,-c(17:20)])) # Plot of correlation matrix of new data

LondonDataNew <- data.frame(LondonData[,c(2:4,15:17,22,23:24,26:28,30:31)],Age,Type,Garage,Bedrooms) # Removing collinearity
corrplot::corrplot(cor(LondonDataNew[,-c(15:18)])) # Plot of new correlation matrix
par(mfrow=c(1,1))

summary(LondonDataNew)

# Boxplots of data
par(mfrow=c(2,3))
boxplot(Purprice~CenHeat,data=LondonDataNew, main="Central Heating")
boxplot(Purprice~BathTwo,data=LondonDataNew, main="2 or More Bathrooms")
boxplot(Purprice~Age,data=LondonDataNew, main="Built Period",las=2)
boxplot(Purprice~Type,data=LondonDataNew, main="House Type",las=2)
boxplot(Purprice~Garage,data=LondonDataNew, main="Garage Type",las=2)
boxplot(Purprice~Bedrooms,data=LondonDataNew, main="Bedrooms",las=2)
par(mfrow=c(1,1))

# Boxplot of house price data
LondonDataNew <- LondonDataNew[LondonDataNew$Purprice < 600000,]
boxplot(LondonDataNew$Purprice, main="House Prices")

# Hosues price vs Floor Area plot
plot(LondonDataNew[,c("FlorArea","Purprice")],pch=16,cex=0.5, main="House Price Vs Floor Area", xlab="Floor Area (m^2)", ylab="House Purchase Price (GBP)")
lines(lowess(LondonDataNew[,c("FlorArea","Purprice")]),col="red")

# Map of house prices on by eastings and northings
Classes <- classIntervals(LondonDataNew$Purprice,10,"quantile")
Colours <- findColours(Classes,palette())
plot(LondonDataNew$Easting,LondonDataNew$Northing,pch=16,cex=0.5,col=Colours)

# Coordinate trends
CoordMod <- lm(Purprice~Easting+Northing+(Easting^2)+(Northing^2)+(Easting*Northing),
            data=LondonDataNew)
summary(CoordMod)

# The Model
price.lm   <- lm(Purprice~., data=LondonDataNew[,-c(1,2)]) # Making model without easting and northing
invisible(capture.output(price.step <- stepAIC(price.lm))) # Determining which variables to keep 
summary(price.step) 
price.step$anova # Model comparison

par(mfrow=c(2,2))
plot(price.step)
par(mfrow=c(1,1))

quickMap2 <- function(Var,nClass=9,dp=0,plotNames=FALSE){
   require(classInt)
   require(RColorBrewer)
   Classes <- classIntervals(Var,nClass,method="quantile",dataPrecision=dp)
   Palette <- brewer.pal(nClass,"Reds")
   Colours <- findColours(Classes,Palette)
   Bname <- gsub(" London Boro","",LB$NAME)
   plot(LB,col=Colours)
   legend("bottomright",
      legend=names(attr(Colours,"table")),
      fill=attr(Colours,"palette"),
      cex=0.75,bty="n")
   box()
   if(plotNames) {
      xy <- coordinates(LB)
      text(xy[,1],xy[,2],Bname,col="black",cex=0.5)
   }
}

# Variation by borough 
invisible(capture.output(LB <- readOGR(dsn=".",layer="LondonBoroughs",stringsAsFactors=FALSE)))  # Loading borough data
LH <- SpatialPointsDataFrame(LondonDataNew[,1:2],LondonDataNew) # Makeing SPDF
proj4string(LH) <- CRS(proj4string(LB)) # copy CRS
LHLB <- over(LH,LB)   # spatial joining points and polygons
LondonDataNew$Borough <- gsub(" London Boro","",LHLB$NAME)  # add borough names only to data
Boroughs <- names(table(LondonDataNew$Borough)) # borough names

# Price by borough
b.order <- rank(tapply(LondonDataNew$Purprice+runif(nrow(LondonDataNew)),
                       LondonDataNew$Borough,median)) # ranking boroughs by the median of prices

boxplot(log(Purprice)~Borough,data=LondonDataNew,xaxt="n", at=b.order) 
axis(1,labels=Boroughs,at=b.order,las=2)
title("Log of Price by Borough")

quickMap2(tapply(LondonDataNew$Purprice,LondonDataNew$Borough,median),plotNames=TRUE,dp=3)

# Floor area by borough
b.order.floor <- rank(tapply(LondonDataNew$FlorArea+runif(nrow(LondonDataNew)),
                       LondonDataNew$Borough,median)) # ranking boroughs by the median of floor area

boxplot(FlorArea~Borough,data=LondonDataNew,xaxt="n", at=b.order.floor) 
axis(1,labels=Boroughs,at=b.order.floor,las=2)
title("Floor Area by Borough")

quickMap2(tapply(LondonDataNew$FlorArea,LondonDataNew$Borough,median),plotNames=TRUE,dp=3)

# Residuals by borough
LondonDataNew$stdres.price.step <- stdres(price.step)
b.order.price.step <- rank(tapply(LondonDataNew$stdres.price.step+
                                    runif(nrow(LondonDataNew))*0.0001,LondonDataNew$Borough,median))

boxplot(stdres.price.step~Borough, data=LondonDataNew, xaxt="n", at=b.order.price.step)
axis(1,labels=Boroughs,at=b.order.price.step,las=2)
title("Standardised Residual by Borough")

quickMap2(tapply(LondonDataNew$stdres.price.step,LondonDataNew$Borough,median),plotNames=TRUE,dp=3)

# GWR
set.seed(1)
s <- sample(nrow(LondonDataNew), round(.3*nrow(LondonDataNew))) # Splitting data
LondonDataTrain <- LondonDataNew[s,] # Training set 
LondonDataTest <- LondonDataNew[-s,] # Testing set

LondonDataTrain <- LondonDataTrain[,-c(19:20)] # Removing un-needed variable
LondonDataTrain <- SpatialPointsDataFrame(cbind(LondonDataTrain[,1:2]),LondonDataTrain) # Making SPDF 
gwr<-gwr.basic(Purprice ~ Tenfree + CenHeat + BathTwo + FlorArea + ProfPct + RetiPct + 
                 Unemploy + Age + Type + Garage + Bedrooms, 
               data=LondonDataTrain, bw=250, adaptive=T) 
gwr
