#############################################################################################
#
# Mirror, mirror ... best batsman of them all
# Designed and developed by: Tinniam V Ganesh    
# Date: 24 March 2015
# More details - https://gigadom.wordpress.com
# 
##############################################################################################

#Install package plotrix for pie3D
library(ggplot2)
library(e1071)

setwd("C:\\software\\R\\batsman")
source("battingperf.R")

# Create an output directory if it does not exist
if(!file.exists("output")) {
  dir.create("output")
}

# Create a directory to store plots
if(!file.exists("plots")) {
  dir.create("plots")
}

#################################################################
# The following section compares 3 Indian icons of cricket
# 1. Sachin Tendulkar
# 2. Rahul Dravid
# 3. Sunil Gavaskar
#################################################################
# Sachin Tendulkar's batting performance
tendulkar = read.csv("tendulkar.csv")
name <- 'Sachin Tendulkar'
boxplotHist(tendulkar,name)
perfplot(tendulkar,name)
meanStrikeRate(tendulkar,name)

# Sunil Gavaskar's batting performance
gavaskar = read.csv("gavaskar.csv")
name <- 'Sunil Gavaskar'
boxplotHist(gavaskar,name)
perfplot(gavaskar,name)
meanStrikeRate(gavaskar,name)

# Rahul Dravid's batting performance
dravid = read.csv("dravid.csv")
name <- 'Rahul Dravid'
boxplotHist(dravid,name)
perfplot(dravid,name)
meanStrikeRate(dravid,name)

#Compute and plot the relative performance
name <-"ABC"
setwd("./plots")
relPerf <- "indian-batsman-relPerf.png"
png(relPerf)
relativePerf(tendulkar,name,"red",flag=TRUE,18)
relativePerf(gavaskar,name,"blue",19)
relativePerf(dravid,name,"green",20)
legend(x="topright",c("Tendulkar","Gavaskar","Dravid"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5),col=c("red", "blue","green"),bty="n")
dev.off()
setwd("..")

#Compute and plot the relative mean strike rate of the batsman
name <-"ABC"
setwd("./plots")
relSR <- "indian-batsman-relSR.png"
png(relSR)
relativeSR(tendulkar,name,"red",flag=TRUE,18)
relativeSR(gavaskar,name,"blue",19)
relativeSR(dravid,name,"green",20)
legend(x="bottomright",c("Tendulkar","Gavaskar","Dravid"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5),col=c("red", "blue","green"),bty="n")
dev.off()
setwd("..")

#################################################################
# The following section compares 4 international  icons of cricket
# 1. Sachin Tendulkar
# 2. A B De Villiers
# 3. Ricky Ponting
# 4. Brian Lara
#################################################################

# A B DeVilliers batting performance
villiers = read.csv("villiers.csv")
name <- 'AB De Villiers'
boxplotHist(villiers,name)
perfplot(villiers,name)
meanStrikeRate(villiers,name)

# Brian Lara's  batting performance
lara = read.csv("lara.csv")
name <- 'Brian Lara'
boxplotHist(lara,name)
perfplot(lara,name)
meanStrikeRate(lara,name)


# Ricky Ponting's batting performance
ponting = read.csv("ponting.csv")
name <- 'Ricky Ponting'
boxplotHist(ponting,name)
perfplot(ponting,name)
meanStrikeRate(ponting,name)

#Compute and plot the relative performance
name <-"ABC"
setwd("./plots")
relPerf <- "intl-batsman-relPerf.png"
png(relPerf)
relativePerf(tendulkar,name,"red",flag=TRUE,18)
relativePerf(ponting,name,"blue",)
relativePerf(lara,name,"orange")
relativePerf(villiers,name,"green")
legend(x="topright",c("Tendulkar","Ponting","Lara", "De Villiers"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5,2.5),col=c("red", "blue","orange", "green"),bty="n")
dev.off()
setwd("..")

#Compute and plot the relative  mean strike rate of the batsman
name <-"ABC"
setwd("./plots")
relSR <- "intl-batsman-relSR.png"
png(relSR)
relativeSR(tendulkar,name,"red",flag=TRUE,18)
relativeSR(ponting,name,"blue",)
relativeSR(lara,name,"orange")
relativeSR(villiers,name,"green")
legend(x="bottomright",c("Tendulkar","Ponting","Lara", "De Villiers"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5,2.5),col=c("red", "blue","orange", "green"), bty="n")
dev.off()
setwd("..")





