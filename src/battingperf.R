#############################################################################################
#
# Mirror, mirror ... best batsman of them all
# Designed and developed by: Tinniam V Ganesh    
# Date: 24 March 2015
# More details - https://gigadom.wordpress.com
# 
##############################################################################################

# Plot the batting performance as a combined box plot and histogram
boxplotHist <- function(df, name) {
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]

  # Remove rows with 'TDNB' - Team did not bat
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]

  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove '*' character in Run denoting not out
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  
  # Set the directory to plot
  setwd("./plots")
  runfreqplot <- paste(name,"-runs-frequency.png")
  png(runfreqplot)
  
  atitle <- paste(name,"'s", " - Runs Frequency vs Runs")
   
  # Set the layout and the margins. 
  nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
  par(mar=c(2,2,1,1))
  
  # Draw the boxplot
  boxplot(batsman$Runs, horizontal=TRUE,  outline=TRUE,ylim=c(0,max(batsman$Runs)), 
          frame=F, col = "green1")
  
  # Draw lines showing the mean and meadian
  abline(v=median(batsman$Runs),col="blue",lwd=3.0)
  abline(v=mean(batsman$Runs),col="red",lwd=3.0)
  
  # Create a vector from 0 with intervals of 10 for the intervals
  maxi <- (max(batsman$Runs/10) + 1) *10
  v <- seq(0,maxi,by=10)
  
  # Draw a histogram
  hist(batsman$Runs,breaks=v,xlab="Runs",ylab="Runs frequency", 
          main = atitle,labels=TRUE,col="grey")
  
  # Draw the meadian, mean, 1st and 3rd quantiles
  abline(v=median(batsman$Runs),col="blue",lwd=3.0)
  abline(v=mean(batsman$Runs),col="red",lwd=3.0)
  abline(v=quantile(batsman$Runs,.25),col="black",lwd=3.0,lty=2)
  abline(v=quantile(batsman$Runs,.75),col="black",lwd=3.0,lty=2)
  
  # Draw a rug below the histogram
  rug(batsman$Runs,col="blue",lwd=2)
  dev.off()
  setwd("..")
  
  # Write the summary performances to output directory
  setwd("./output")
  row <- NULL
  
  # Compute the summary of the runs
  perf <- summary(batsman$Runs)
  
  # Compute the length of data set, skew, kurtosis, 1st and 3rd percentile
  r <- length(batsman$Runs)
  s <- skewness(batsman$Runs)
  s <- format(s,digits=3,nsmall=2)
  t <- kurtosis(batsman$Runs)
  t <- format(t,digits=3,nsmall=2)
  
  
  # Create a vector with all the values
  row <- c(name, r,as.vector(perf),s,t,"\n")
 
  # Write the values to a file
  cat(row, file="perf-summary.txt", sep=",",append=TRUE)
  setwd("..")
  # reset the layout
  par(mfrow=c(1,1))
  
}

# Create a performance plot between Runs and RunsFrequency 
perfplot <- function(df, name) {
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove '*' character in Run denoting not out
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs)) 
  
  # Create breaks in intervals of 10
  maxi <- (max(batsman$Runs/10) + 1) *10
  v <- seq(0,maxi,by=10)
  a <- hist(batsman$Runs,breaks=v,plot=FALSE)
  Runs <- a$mids
  RunFrequency <- a$counts
  df1 <- data.frame(Runs,RunFrequency)
  
  # Create and save the plot
  setwd("./plots")
  perfplot <- paste(name,"-performance.png")
  
  # Create a ggplot
  atitle <- paste(name,"'s", " batting performance")
  g <- qplot(df1$Runs,df1$RunFrequency, data=df1,geom=c("point","smooth"),
             xlab="Runs",ylab="Run Frequency")
  p <-g + ggtitle(atitle)
  print(p)
  
  # Save the plot
  ggsave(filename=perfplot, plot=p)
 
  setwd("..")
}

# Plot the performance of the batsman as a continous graph
meanStrikeRate <- function(df, name){
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
 
  # Remove rows with * (not out)
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs)) 
 
  # Remove rows with "- in Strike rate
  d <- batsman$SR != "-"
  batsman <- batsman[d,]
  
  # Create a vector of runs with intervals of 15
  maxi <- (max(batsman$Runs/15) + 1) *15
  v <- seq(0,maxi,by=15)
  a <- hist(batsman$Runs,breaks=v,plot=FALSE)
 
  
  # Compute the Mean Strike Rate for each run range
  SR <- NULL
  for(i in 2:length(a$breaks))  {
    b <- batsman$Runs > a$breaks[i-1] & batsman$Runs <= a$breaks[i] 
    c <- batsman[b,]
    SR[i-1] <- mean(as.numeric(as.character(c$SR)))
  }
  
  # Find all intervals where there is no data i.e. NA
  b <- !is.na(SR)
  
  #Subset and remove the NAs for counts
  c <- a$mid[b]
  
  #Subset and remove the NAs for Strike Rate
  SR <- SR[b]
  
  # Change directory and save plot
  setwd("./plots")
  atitle <- paste(name,"'s", " - Mean Strike Rate vs Runs scored")
  strikerateplot <- paste(name,"-sr.png")
  png(strikerateplot)
  par(mar=c(4,4,1,1))
  plot(c,SR,pch=12,xlab="Runs",ylab="Mean Strike Rate",ylim=c(0,90), main=atitle)
  lines(c,predict(loess(SR~c)),col="blue",lwd=3)
  dev.off()
  setwd("..")

}


# Compute and plot the relative performances of batsmen
relativePerf <- function(df, name,color,flag=FALSE,sym) {
  
  # Remove the DNB rows
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove the symbol '*'
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs)) 
  
  # Create intervals of 10
  maxi <- (max(batsman$Runs/10) + 1) *10
  v <- seq(0,maxi,by=10)
  a <- hist(batsman$Runs,breaks=v,plot=FALSE)
  
  print(length(a$mids))
  
  # Compute the Runs percentage for each run range
  runP <- NULL
  
  for(i in 2:length(a$breaks))  {
    # Compute the runs by the batsman in the range
    b <- batsman$Runs > a$breaks[i-1] & batsman$Runs <= a$breaks[i] 
    c <- batsman[b,]
    
    #Calculate the percentage runs in the interval
    runP[i-1] <- (sum(c$Runs)/sum(batsman$Runs))*100
  }
  
  # Get a logical vector where runPercentage != 0
  b <- runP != 0
  
  #Subset the mid points
  mid <- a$mid[b]
  
  #Subset the runPercentage
  runPercentage <- runP[b]
  print(length(runPercentage))
  
  par(mar=c(4,4,1,1))
 
  # Fit a loess model on runsPercentages
  if(flag == TRUE) {  
      plot(mid,predict(loess(runPercentage~mid)),col=color,lwd=3,type="l",
                lty=1,xlim=c(0,400), ylim=c(0,12),xlab="Runs",ylab="Percentage runs in range (%)",
           main="Relative Run percentages vs Runs")
  } else  {
      lines(mid,predict(loess(runPercentage~mid)),col=color,lwd=3)
  }
  
}

# Compute and plot relative Strike Rates of batsmen
relativeSR <- function(df, name,color,flag=FALSE,sym) {
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  #
  # Remove rows with * (not out)
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs)) 
  
  # Remove rows with "- in Strike rate
  d <- batsman$SR != "-"
  batsman <- batsman[d,]
  
  # Create a vector of runs with intervals of 15
  maxi <- (max(batsman$Runs/15) + 1) *15
  v <- seq(0,maxi,by=15)
  a <- hist(batsman$Runs,breaks=v,plot=FALSE)
  
  
  # Compute the Mean Strike Rate for each run range
  SR <- NULL
  for(i in 2:length(a$breaks))  {
    b <- batsman$Runs > a$breaks[i-1] & batsman$Runs <= a$breaks[i] 
    c <- batsman[b,]
    SR[i-1] <- mean(as.numeric(as.character(c$SR)))
  }
  
  # Find all intervals where there is no data i.e. NA
  b <- !is.na(SR)
  
  #Subset and remove the NAs for counts
  c <- a$mid[b]
  
  #Subset and remove the NAs for Strike Rate
  SR <- SR[b]

  par(mar=c(4,4,1,1))
  if(flag == TRUE) {
     plot(c,predict(loess(SR~c)),xlab="Runs",ylab="Mean Strike Rate",
            xlim=c(0,400), ylim=c(0,90), type="l",lty=1,lwd=3, col=color,
           main="Relative Mean Strike Rate")
  } else {
     lines(c,predict(loess(SR~c)),col=color,lwd=3)
  }
  
}