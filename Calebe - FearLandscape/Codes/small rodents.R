
#This script replicate the GLM and the figures of the manuscript "Landscape of human fear in Neotropical rainforest mammals"
# DOI: https://doi.org/10.1016/j.biocon.2019.108257
# For more information, contact the author.
# Calebe Pereira Mendes - calebepm3@hotmail.com

#Species: small rodents
#Night lights best scale: 5 Km

rm(list = ls())
setwd("C:\\Users\\Calebe\\Desktop\\FearLandscape (git)\\Data") #Remover isso


### Load the data
Data = read.csv("Day-Night_records.csv", header = TRUE)



# Select the small rodent data
Data = Data[,c("Site", "NightLight_5km", "Rat_Day", "Rat_Night")]
colnames(Data) = c("Site", "Disturbance", "Day", "Night")

Data = Data[Data$Day != 0 | Data$Night != 0,] #remove sites without any records
Data$Disturbance = log(Data$Disturbance) #Calculate the log of the Disturbance variable



### fit the model
Model = glm(cbind(Day,Night) ~ Disturbance , data = Data ,family = binomial)
summary(Model)





### Create the tables needed to Plot the data

#for the line
plot_X <- data.frame(Disturbance = seq(min(Data$Disturbance), max(Data$Disturbance), len = 100))
prd.model <- predict(Model, newdata = plot_X, type = "r", se.fit = T)


#used for plot the outer points (aesthetic only)
OuterPoints = data.frame(matrix(NA, ncol = 2, nrow = 0)) 
colnames(OuterPoints) = c("X", "Y")

for(a in Data$Site){ #create coordinates for the points
  new = data.frame(X = rep(Data$Disturbance[Data$Site == a], sum(Data$Day[Data$Site == a], Data$Night[Data$Site == a])),
                  Y = c(rep(1.05, Data$Day[Data$Site == a]), rep(-0.05, Data$Night[Data$Site == a])))
  OuterPoints = rbind(OuterPoints, new)
}; rm(new, a)

#undo the log values in the X labels
Xticks = seq(min(Data$Disturbance), max(Data$Disturbance),length.out = 6)
Xlab = as.numeric(substr(as.character(exp(1)^Xticks), 0, 5))




### Scatter PLOT
require(scales)

plot(plot_X$Disturbance, prd.model$fit, type = "l",xlab = "Night time light radiance (microflicks)", ylab = "Day time activity ratio",
     ylim = c(-0.1,1.1), yaxt = "n", xaxt = "n",lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit - 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
lines(plot_X$Disturbance, prd.model$fit + 1.96 * prd.model$se.fit, lty = 2, col = "darkgray", lwd = 2) 
axis(side = 2, at= seq(0,1,0.25), labels = seq(0,1,0.25), cex.axis=0.7)
axis(side = 1, at= Xticks, labels = Xlab, cex.axis=0.7)
points(jitter(OuterPoints$Y,0.05) ~ jitter(OuterPoints$X,50), cex = 0.5, col = alpha("darkblue", 0.2))

rm(plot_X,OuterPoints, prd.model,Xlab,Xticks)




### Circular plot

require(circular)

#Load the data
CData = read.csv("Dial_Activity.csv", header = TRUE) 


# Select the small rodent data
CData = CData[CData$Species == "Rat",]


# Create a decimal hour variable (useful for the circular plot)
CData$Hour.Dec = CData$Hour + (CData$Minutes/60)


# Circular Plot
plotdata = circular(CData$Hour.Dec, type="angles",units="hours",template="clock24")
rose.diag(plotdata, bins=24, col = "gray50", prop=1.8, cex = 0.7)

rm(plotdata)



### Dividing the sites between the 50% more disturbed and 50% less disturbed.
#  In the case the number of sites being odd, the median site will be consedered
#  more or less disturbed if its radiance is above or below the mean, respectively.

threshold = sort(unique(CData$NightLight_5K))

if((length(threshold) %% 2) == 0){
  threshold = median(threshold)
} else {
  if(median(threshold) > mean(threshold)){
    threshold = median(threshold) - 0.0000001
  }else{
    threshold = median(threshold) + 0.0000001
  }
}



# Plotdata
less = CData$Hour.Dec[CData$NightLight_5K < threshold]
less = circular(less, type="angles",units="hours",template="clock24")

more = CData$Hour.Dec[CData$NightLight_5K > threshold]
more = circular(more, type="angles",units="hours",template="clock24")


# Plot less disturbed areas in gray and more disturbed areas in translucid white
rose.diag(less, bins=24, col = "gray50", prop=1.8, cex = 0.7)
rose.diag(more, bins=24, col = alpha("white", 0.5), prop=1.8, cex = 0.7, add = T)


