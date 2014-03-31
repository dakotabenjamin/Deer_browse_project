# This is the Final draft of the R-Script for my BIOL 396 Capstone Project
# R version 3.0.2 (2013-09-25) -- "Frisbee Sailing"

# Set working Directory for the Project
  setwd("/Users/tommyschafer/Dropbox/CWRU Fall 2013/BIOL Capstone/Capstone/Capstone Data/PCAP")

# Install Libraries for Visual Use later
  library(Hmisc)
  library(scales)
  library(png)
  
# Import Files 
  cover10 <- read.csv("pcap-cover-2010.csv")
  cover11 <- read.csv("pcap-cover-2011.csv")
  gis <- read.csv("pcap-gis-data.csv")
  species <- read.csv("species-list-PCAP.csv")
  
# Bind Both years cover into one file called allcover
  allcover <- rbind(cover10, cover11)
  summary(allcover)
# Merge Species with allcover to have a full data file from hich to calculate richness of adventive, native, and total 
  alldata <- merge(allcover, species, by.x = "SPCODE", by.y = "SPCODE")
  # Then merge the same data file with GIS data to add in the lat/longs
  alldata <- merge(alldata, gis, by.x = "plot", by.y = "plot")
  summary(alldata)
  length(unique(alldata$plot))
  
# Create the experimental vector we are we need to experiment
  # Calculate Distances from the GIS data and the point in cleveland we will be testing (This point is known as dead man's curve, an historical spot for cleveland)
  # Use the reference GPS point (N 41.517880, W -81.674046)
  # Make a new vector with the distance to the reference point of each plot using the Pythagorean Distance formula
    alldata$distance <- (sqrt(((alldata$long - -81.674046))^2 + (alldata$lat - 41.517880)^2))
    summary(alldata$distance)
  
# Now calculate the richness for each plot and dilute the reference frame to a plot based one (for native/adventive/total)
  # Adventive and Native richness frame
    attach(alldata)
    richness <- aggregate(alldata$cover.mn, list(plot = plot, native = native, distance = distance, lat = lat, long = long), FUN = length)
    detach(alldata)
    summary(richness)
  
    # rename the x columns created by aggregate
      names(richness)[names(richness) == "x"] <- "rich"
      summary(richness)
  
  # Total Richness data frame
      attach(alldata)
      rich.total <- aggregate(alldata$cover.mn, list(plot = plot, distance = distance, lat = lat, long = long), FUN = length)
      detach(alldata)
      summary(rich.total)
  
    # rename the x columns created by aggregate
      names(rich.total)[names(rich.total) == "x"] <- "rich"
      summary(rich.total)

  ############ BE WARY OF THIS METHOD #############
  ### >>>Write rich.adventive and reimport so that I can add in the extra values that are missing without writing a complex Loop
  # --> only use this line of code for writing out the file before it if you ened to add extra vectors to the new "richness" dataframe
  # --> Dont fall asleep while codingcxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  #write.csv(richness, "Capstone-richness-20131010.csv") #commented out so that I dont overwrite the file that I altered
  ########################################################################################
  
# Read the File Back in after changes have been made
  richness <- read.csv("Capstone-richness-20131010.csv")
  
  
# Split the richnesses into native and adventive with the subset function
  rich.adventive <- subset(richness, native == "adventive") 
  rich.native <- subset(richness, native == "native") 
  
# Build models for all 3 variables
  
  # Check normality of each vector
  hist(rich.native$rich)
  hist(rich.adventive$rich)
  hist(rich.total$rich)
  hist(rich.native$distance) # This is good
  
  # Native and total are both normal, let's see how log of adventive works out
  hist(log(rich.adventive$rich))  # This is much better. We should be able to do accurate comparisons now
    # make a new vector for this so we dont have to remember to log everything
      rich.adventive$ln.rich <- log(rich.adventive$rich + .001)
  
  # Look at graphs of the following interactions of species richness with distance
  plot(rich.adventive$distance, log(rich.adventive$rich), main="Adventive SR vs. Distance", ylab="Adventive Richness", xlab="Distance from Center of Urban Matrix")
  abline(lm(log(rich.adventive$rich+.001)~rich.adventive$distance), col="red")
  
  plot(rich.native$distance, rich.native$rich, main="Native SR vs. Distance", ylab="Native Richness", xlab="Distance from Center of Urban Matrix")
  abline(lm(rich.native$rich~rich.native$distance), col="green")
  
  plot(rich.total$distance, rich.total$rich, main="Total SR vs. Distance", ylab="Total Richness", xlab="Distance from Center of Urban Matrix")
  abline(lm(rich.total$rich~rich.total$distance), col="blue")  
  
# Create linear models and check that the significances are important
  lm_native.distance <- lm(rich.native$rich ~ rich.native$distance)
  lm_adventive.distance <- lm(rich.adventive$rich ~ rich.adventive$distance)
  lm_total.distance <- lm(rich.total$rich ~ rich.adventive$distance)
  lm_native.adventive <- lm(rich.native$rich ~ rich.adventive$rich)
  
  anova(lm_native.distance) # Significant P < 0.0004674
  anova(lm_adventive.distance)
  anova(lm_total.distance)
  anova(lm_native.adventive)  # Significant P < 0.0000000001942
  summary(lm_native.distance) # F: 12.75
  summary(lm_adventive.distance) # F:0.1836
  summary(lm_total.distance) # F: 1.357
  summary(lm_native.adventive) # F: 46.12
  
  # Setup graphing area for anova plot checks
  par(mfcol=c(2,2))
  
  plot(lm_native.distance)
  plot(lm_adventive.distance)
  plot(lm_total.distance)
  plot(lm_native.adventive)

  

  # THUS LET US BUILD GRAPHS!
  
##########################################################################################################
#### PLOT 1 Comparision of total/adventive/native
  par(mfcol=c(3,1))
  
  plot(rich.total$distance, rich.total$rich, main="Total Richness vs. Distance from Center of Cleveland", ylab = "Total Species Richness", xlab = "Distance from the Center of the Cleveland Urban Matrix (degrees)")
  arrows(0.1,0,.30,0, xpd=TRUE, col="red", lwd=2)
  text(0.1,0, "Urban", pos=2, xpd=TRUE)
  text(0.3,0, "Rural", pos=4, xpd=TRUE)
  # No abline because not significant
  
  plot(rich.adventive$distance, rich.adventive$rich, main="Adventive Richness vs. Distance from Center of Cleveland", ylab="Adventive Richness", xlab="Distance from Center of Urban Matrix (degrees)")
  arrows(0.1,-3,.30,-3, xpd=TRUE, col="red", lwd=2)
  text(0.1,-3, "Urban", pos=2, xpd=TRUE)
  text(0.3,-3, "Rural", pos=4, xpd=TRUE)
  # no abline because not significant
  
  plot(rich.native$distance, rich.native$rich, main="Native Richness vs. Distance from Center of Cleveland", ylab="Native Richness", xlab="Distance from Center of Urban Matrix (degrees)")
  abline(lm_native.distance, col="blue")
  arrows(0.1,2,.30,2, xpd=TRUE, col="red", lwd=2)
  text(0.1,2, "Urban", pos=2, xpd=TRUE)
  text(0.3,2, "Rural", pos=4, xpd=TRUE)
  
#### PLOT 2 - The Graph of Adventive & Native Species Richness with the Cleveland Metro Parks Map
  par(mfcol = c(1,1))
  # Replace the directory and file information with your info
    ima <- readPNG("/Users/tommyschafer/Dropbox/CWRU Fall 2013/BIOL Capstone/Capstone/Cleveland_Metroparks_Map.png")
  
  #Get the plot information so the image will fill the plot box, and draw it
    lim <- par()
    plot(rich.native$long, rich.native$lat, main="Native Species Richness in the Cleveland Metroparks", col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich), xlab = "Longitude", ylab = "Latitude") 
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    points(rich.native$long+.01, rich.native$lat-.015,  col=alpha("blue", (rich.native$rich/186)),  pch=19, cex=.15*(richness$rich)) 
    points(-81.674046+.01, 41.517880-.015, col="firebrick1", cex=2, pch=19)
    points(-81.55, 41.22, col="orange", pch = 21 , cex = 30, lwd=3)  
    text(-81.55, 41.22, "\"Mainland\"")
    text(-81.674046+.01, 41.517880-.015,"Dead Man's Curve",pos = 2)
    segments(-81.674046+.01, 41.517880-.015, rich.native$long +.01, rich.native$lat-.015, col=alpha("black",.2), lwd=1)    
    points(-81.85, 41.42,pch= 21,col = "firebrick1", cex=25, lwd=3)
    
# Build Scale Based top point for 10 mile range in degrees
  # top = (x,y) ot top point, bot = (x,y) of bottom point, mid = self calculated midpoint w = width of marker lines
    top <- c(-81.42,41.38)
    bot <- c(top[1], top[2]-0.1449275362319)
    mid <- bot[2] + .5*(top[2]-bot[2])
    w <- .01
    topval <- 10
    midval <- .5*topval # dont change
    botval <- 0 # dont change
    unit <- "miles"
  # build the actual scale base don previous values
    segments(top[1], top[2], bot[1], bot[2], lwd = 2, col="black")
    segments(top[1]-w, top[2], top[1]+w, top[2], lwd=2)
    segments(bot[1]-w, bot[2], bot[1]+w, bot[2], lwd=2)
    segments(top[1]-.5*w, mid, top[1]+.5*w, mid, lwd=2)  
    text(top[1]+w, top[2], topval, pos=4)
    text(top[1]+w, mid, midval,  pos=4)
    text(bot[1]+w, bot[2], botval,  pos=4)
    text(top[1], mid, unit, pos = 2)

#### PLOT 3 the Native and Adventive species against their associated N.tot
  # Adventive (shows a negative linear relationship"
  par(mfcol=c(1,1))
  plot(soildata.adventive$N.tot, soildata.adventive$rich, main="Species Richness vs. Available Nitrogen", xlab="Available N (%)", ylab = "Species Richness", col = "blue", ylim =c(0,90))
  points(soildata.native$N.tot, soildata.native$rich, col = "green")
  legend(.6,80, c("adventive", "native"), pch = 21, col = c("blue", "green")) #BUILD A LEGEND HERE
  
#### PLOT 4 - The Native richness versus adventive richness.
  plot(rich.adventive$rich,rich.native$rich, main = "Comparison of Adventive and Native Richnesses Per Plot", ylab="Native Richness", xlab="Adventive Richness")
  abline(lm(rich.native$rich~rich.adventive$rich), col = "blue")
  
  
  