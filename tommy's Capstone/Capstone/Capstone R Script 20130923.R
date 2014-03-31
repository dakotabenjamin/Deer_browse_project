# R version 3.0.1 (2013-05-16) -- "Good Sport"
# Script for William T Schafer's Capstone Project Started: September 23, 2013

# PROJECT: Analyze the PCAP data to see if a relationship exists between distance from a point in the center of downtown Cleveland and the species richness of Adventive, Native, and total in the Cleveland Metroparks

# Install libraries for use
    library(Hmisc)
    library(scales)
    library(png)
    
# Construct files that will be necessary
  
    # Set working directory
      setwd("/Users/tommyschafer/Dropbox/R/PCAP/")
  # Import Files 
      cover10 <- read.csv("pcap-cover-2010.csv")
      cover11 <- read.csv("pcap-cover-2011.csv")
      gis <- read.csv("pcap-gis-data.csv")
      species <- read.csv("species-list-PCAP.csv")
      
  # Bind Both years cover into one file called allcover
      allcover <- rbind(cover10, cover11)
      summary(allcover)
  # Merge Species with allcover to have a full data file from hich to calculate richness
      alldata <- merge(allcover, species, by.x = "SPCODE", by.y = "SPCODE")
      alldata <- merge(alldata, gis, by.x = "plot", by.y = "plot")
      summary(alldata)
      unique(alldata$plot)
  
# Create the Extra predictors we need to experiment
  # Calculate Distances from the GIS data and the point in cleveland we will be testing
  # Use the reference GPS point (N 41.517880, W -81.674046)
  # Make a new vector with the distance to the reference point of each plot using the Pythagorean Distance formula
    alldata$distance <- (sqrt(((alldata$long - -81.674046))^2 + (alldata$lat - 41.517880)^2))
    summary(alldata)
    
  # Now calculate the richness for each plot and dilute the reference frame to a plot based one
    attach(alldata)
    richness <- aggregate(alldata$cover.mn, list(plot = plot, native = native, distance = distance, lat = lat, long = long), FUN = length)
    detach(alldata)
    
    # rename the x columns created by aggregate
      names(richness)[names(richness) == "x"] <- "rich"
      summary(richness)

  # Write rich.adventive and reimport so that I can add in the extra values that are missing without writing a complex Loop
      # --> only use this line of code for writing out the file before it if you ened to add extra vectors to the new "richness" dataframe
    # --> Dont fall asleep while codingcxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
        #write.csv(richness, "Capstone-richness-20131010.csv") #commented out so that I dont overwrite the file that I altered
    ########################################################################################
    # Read the File Back in after changes have been made
      richness <- read.csv("Capstone-richness-20131010.csv")
      
    # Split the richnesses into native and adventive with the subset function
      rich.adventive <- subset(richness, native == "adventive") 
      rich.native <- subset(richness, native == "native") 
      
# Correlate the Variables
  # Look at the plots of both adventive and native species richness as graphed against the distance
      plot(rich.adventive$distance,rich.adventive$rich, main="Adventive Richness vs. Distance from \nCenter of Cleveland", ylab="Adventive Richness", xlab="Distance from Center of Urban Matrix Distance")
      abline(lm(rich.adventive$rich~rich.adventive$distance), col="red")
      plot(rich.native$distance,rich.native$rich, main="Native SR vs. Distance", ylim=c(0,80)) #, ylim=c(0,max(rich.native$distance)))
      abline(lm(rich.native$rich~rich.native$distance), col="red")

    # Try this (This is a test of the log transformation of the adventive richnesses because something does not seem to fit with its linear model)
    # plot(rich.adventive$distance,log(rich.adventive$rich), main="Adventive Richness vs. Distance from \nCenter of Cleveland", ylab="Adventive Richness", xlab="Distance from Center of Urban Matrix Distance")
    # abline(lm(log(rich.adventive$rich)~rich.adventive$distance), col="red")
      
# Perform 2-way anova on Native and nonnative species richness
  # First check the normality of the data
      hist(rich.native$rich)
      hist(rich.adventive$rich)
      hist(log(rich.native$rich))
      hist(log(rich.adventive$rich))
      
# Log the Data here and use those values to create the model instead because normality was achieved after a log transformation
    # Make sure to use log in the calculations with the native richness and the ANOVA and all other statstical calculation
    # First add some small value to the Vector "rich.adventive$rich" so that we can avoid infinity before we get to log the data
      rich.adventive$rich <- (rich.adventive$rich + 0.0001)
    
    # Create log values in adjacent vectors
      rich.native$ln.rich <- log(rich.native$rich)
      
  # Create model of Plot Species Richness as predicted by distance from Cleveland Urban Matrix
      model_native.distance <- lm(rich.native$ln.rich ~ rich.native$distance)
      model_adventive.distance <- lm(rich.adventive$rich ~ rich.adventive$distance)
      model_rich.tot.distance <- lm(rich.native$ln.rich ~ rich.adventive$rich + rich.adventive$distance)
    
    # Check Statistics of the model
      anova(model_native.distance)
      anova(model_adventive.distance)
      anova(model_rich.tot.distance)
      summary(model_native.distance)
      summary(model_adventive.distance)
      summary(model_rich.tot.distance)
    
    # Check Assumptions of the model
      par(mfcol = c(2,2))
      plot(model_native.distance)
      plot(model_adventive.distance)
      plot(model_rich.tot.distance)
    
# Make Relationship graphs with linear models plotted 
    par(mfcol=c(1,2))
    plot(rich.native$distance, rich.native$ln.rich, main="Native Richness vs. Distance from Urban Matrix", xlab="Distance from Urban Matrix", ylab ="Log of Richness", pch=21, col="blue")
    abline(model_native.distance, col="red")
      
    
    plot(rich.adventive$distance, rich.adventive$rich, main="Adventive Richness vs. Distance from Urban Matrix", xlab="Distance from Urban Matrix", ylab ="Richness", pch=21, col="red")
    #abline(model_adventive.distance, col="blue")
    
    
# Anlyze the Covariate of NO in soil to see if that is a driver for the positive relationship between natiev and adventive species richness
    setwd("/Users/tommyschafer/Dropbox/R/PCAP/")
    soil <- read.csv("pcap-soil-chemistry.csv")
    summary(soil)
    
    # Add in values of N.tot into the already created richness matrix so that we have a dataframe containing all variables
    soildata <- merge(richness, soil)
    summary(soildata)
    
    # Separate out the soil data based on native and adventive plants so that we can see how the soil characteristics affect both
    soildata.adventive <- subset(soildata, native == "adventive")
    soildata.native <- subset(soildata, native == "native") 
    summary(soildata.native)
    summary(soildata.adventive)
    
# Check on the normality of NO Data
    # These should have the same distributions because they are filtered out by plot number and separated only by nativity status
    hist(log(soildata.native$N.tot))
    hist(log(soildata.adventive$N.tot))
    
# Plot the Native and Adventive species against their associated N.tot
    # Adventive (shows a negative linear relationship"
    plot(soildata.adventive$N.tot, soildata.adventive$rich, main="Adventive Species Richness vs. Available Nitrogen", xlab="Available N", ylab = "Adventive Species Richness")
    #abline(lm(soildata.adventive$rich~soildata.adventive$N.tot), col="blue")    
    
    # Create Models to determine if our fit is good enough
    model_N.adventive <- lm(soildata.adventive$rich~soildata.adventive$N.tot)
    anova(model_N.adventive)
    summary(model_N.adventive)
    
    # Native
    plot(soildata.native$N.tot, log(soildata.native$rich), main="Native Species Richness vs. Available Nitrogen", xlab="Available N", ylab = "Native Species Richness")
    abline(lm(log(soildata.native$rich)~soildata.native$N.tot), col="blue")    
    
    # Create models to determine if our fit is good enough
    model_N.native <- lm(log(soildata.native$rich)~soildata.native$N.tot)
    anova(model_N.native)
    summary(model_N.native)
    
# Create Map of the Cleveland Metroparks with Richness Sizes represented by Larger circles on the graph
    
    # Determine the Range of which colors fall into which color categories
      # boxplot.stats(rich.native$rich)
    
    # Use this For-loop to set colors of points in the Cleveland Metroparks Richness Density graph
#    for( i in 1:length(richness$col_choice)) {
 #     if ( richness$rich[i] < 9) {
#        richness$col_choice[i] = "firebrick1"
#      } else if ((22 <= richness$rich[i]) & (30 > richness$rich[i])) {
#        richness$col_choice[i] = "firebrick1"
#      } else if ((30 <= richness$rich[i]) & (41 > richness$rich[i])) {
#        richness$col_choice[i] = "firebrick1"
#      } else 
#        richness$col_choice[i] = "firebrick1"
#    }
#    richness$col_choice
    
    # Plot the result
      plot(rich.native$long, rich.native$lat, main="Cleveland Metroparks PCAP Sampling", col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich), xlab = "Longitude", ylab = "Latitude") 
    # Add the reference point and text indicating it as such
      points(-81.674046, 41.517880, col="firebrick1", cex=2, pch=19)
      text(-81.674046, 41.517880,"Reference Point",pos = 2)
      segments(-81.674046, 41.517880, rich.native$long, rich.native$lat, col=alpha("black",.2), lwd=1)    

    ########## Plot an image behind the plot
    
    # Replace the directory and file information with your info
    ima <- readPNG("/Users/tommyschafer/Desktop/Cleveland_Metroparks_Map.png")
    
    #Get the plot information so the image will fill the plot box, and draw it
    ima <- readPNG("/Users/tommyschafer/Desktop/map2.png")
    lim <- par()
    plot(rich.native$long, rich.native$lat, main="Cleveland Metroparks PCAP Sampling", col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich), xlab = "Longitude", ylab = "Latitude") 
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    points(rich.native$long, rich.native$lat,  col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich)) 
    points(-81.674046, 41.517880, col="firebrick1", cex=2, pch=19)
    text(-81.674046, 41.517880,"Reference Point",pos = 2)
    segments(-81.674046, 41.517880, rich.native$long, rich.native$lat, col=alpha("black",.2), lwd=1)    
    grid()
    lines(c(1, 1.2, 1.4, 1.6, 1.8, 2.0), c(1, 1.3, 1.7, 1.6, 1.7, 1.0), type="b", lwd=5, col="white")
    
    
    ##############################
    # plotting the Soildata along with the Abline of lm(rich.adventive$rich ~ rich.native$rich)
    par(mfcol=c(3,4))
    plot(rich.adventive$rich, rich.native$rich, main="A/N with P1", pch=21, cex=(.5*soildata$P1))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with P2", pch=21, cex=(.2*soildata$P2))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with Bic", pch=21, cex=(.2*soildata$Bic))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with Mg", pch=21, c9=(.03*soildata$Mg))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with Ca.sat", pch=21, cex=(.04*soildata$Ca.sat))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with pH", pch=21, cex=(.5*soildata$pH))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with H.sat", pch=21, cex=(.08*soildata$H.sat))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with CEC", pch=21, cex=(.2*soildata$CEC))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with NO3-N", pch=21, cex=(.5*soildata$NO3))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with S", pch=21, cex=(.5*soildata$S))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with C.tot", pch=21, cex=(.5*soildata$C.tot))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")
    plot(rich.adventive$rich, rich.native$rich, main="A/N with OM", pch=21, cex=(.5*soildata$OM))
    abline(lm(rich.native$rich~rich.adventive$rich), col="blue")

# Subset the Native and Adventive Data frames for side by side comparison
    soil.adventive <- subset(soildata, native == "adventive") 
    soil.native <- subset(soildata, native == "native") 
    
# Test if other varables of soil composition explain the interaction between Natve and Adventive Species Richness    
    anova(lm(soil.adventive$P1~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$P2~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$Bic~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$Mg~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$Ca.sat~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$pH~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$H.sat~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$CEC~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$NO3~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$S~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$C.tot~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$OM~soil.adventive$rich*soil.native$rich))
    anova(lm(soil.adventive$N.tot~soil.adventive$rich*soil.native$rich))
    
    ###################### Build Graphs again here ######################
    
    # Heat Map of Native Richness plots over the Map of the Metroparks

      # Get the plot information so the image will fill the plot box, and draw it
    ima <- readPNG("/Users/tommyschafer/Desktop/Cleveland_Metroparks_Map.png")
    lim <- par()
    plot(rich.native$long, rich.native$lat, main="Cleveland Metroparks PCAP Sampling", col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich), xlab = "Longitude", ylab = "Latitude") 
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    points(rich.native$long, rich.native$lat,  col=alpha("blue", (rich.native$rich/86)),  pch=19, cex=.15*(richness$rich)) 
    points(-81.674046, 41.517880, col="firebrick1", cex=2, pch=19)
    text(-81.674046, 41.517880,"Center of Cleveland",pos = 2)
    segments(-81.674046, 41.517880, rich.native$long, rich.native$lat, col=alpha("black",.2), lwd=1)    
    grid()
    lines(c(1, 1.2, 1.4, 1.6, 1.8, 2.0), c(1, 1.3, 1.7, 1.6, 1.7, 1.0), type="b", lwd=5, col="white")
    
    # Make the Chart with Native and Adventive Species Richness
    par(mfcol=c(2,1))
    plot(rich.native$distance, rich.native$ln.rich, main="Native Richness vs. Distance from Urban Matrix", xlab="Distance from Urban Matrix", ylab ="Log of Richness", pch=21, col="blue")
    abline(model_native.distance, col="red")
    
    
    plot(rich.adventive$distance, rich.adventive$rich, main="Adventive Richness vs. Distance from Urban Matrix", xlab="Distance from Urban Matrix", ylab ="Richness", pch=21, col="red")
    #abline(model_adventive.distance, col="blue")
    
    
    
    
    
    ######################   TEST CODE   #############################
    
# Use this For-loop to set colors of points in the Cleveland Metroparks Richness Density graph
for( i in 1:length(richness$col_choice)) {
  if ( richness$rich[i] < 5) {
    richness$col_choice[i] = "firebrick1"
  } else if ((5 <= richness$rich[i]) & (15 > richness$rich[i])) {
    richness$col_choice[i] = "chartreuse2"
  } else if ((15 <= richness$rich[i]) & (30 > richness$rich[i])) {
    richness$col_choice[i] = "chartreuse3"
  } else 
    richness$col_choice[i] = "chartreuse4"
}
    richness$col_choice
    
    ############
    
    # Total Richness Test
    attach(alldata)
    rich_tot <- aggregate(alldata$cover.mn, list(plot = plot, distance = distance, lat = lat, long = long), FUN = length)
    detach(alldata)
    summary(rich_tot)
    
    # rename the x columns created by aggregate
    names(rich_tot)[names(rich_tot) == "x"] <- "rich"
    summary(rich_tot)
    
    par(mfcol=c(1,1))
    plot(rich_tot$distance, rich_tot$rich, main="Total richness vs Distance from Cleveand Urban Matrix")
    rich_tot_lm <- lm(rich_tot$rich~rich_tot$distance)
    summary(rich_tot_lm)
    anova(rich_tot_lm)
    abline(rich_tot_lm, col = "blue")