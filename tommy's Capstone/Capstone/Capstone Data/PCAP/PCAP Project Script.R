# PCAP Experiment Script

setwd("/Users/tommyschafer/Desktop/R/PCAP/")

# Read in PCAP information
species <- read.csv("species-list-PCAP.csv")
substrate <-read.csv("pcap-substrate-depth.csv")
soil <-read.csv("pcap-soil-chemistry.csv")
plot <- read.csv("pcap-plot-info.csv")
groundcover <- read.csv("pcap-groundcover-data.csv")
gis <- read.csv("pcap-gis-data.csv")
disturbance <- read.csv("pcap-disturbance.csv")
cover10 <- read.csv("pcap-cover-2010.csv")
cover11 <- read.csv("pcap-cover-2011.csv")
canopy <- read.csv("pcap-canopy-cover.csv")

# Look at what is containe din each
names(species)
names(substrate)
names(coil)
names(plot)
names(groundcover)
names(gis)
names(disturbance)
names(cover10)
names(cover11)
names(canopy)

# Let's use cover10, cover11, and species to make a richness comparison between native and nonnative species in the Cleveland Metroparks
 
# Use rbind to combine cover10 and cover11 first and foremost
allcover <-rbind(cover10, cover11)
summary(allcover)
# Use cover.mn  for our cover data

# let's look at combining species data with the cover data using merge by species Code of "SPCODE"
alldata <- merge(species, cover10, by.x = c("SPCODE"), by.y = c("SPCODE"))
summary(alldata)

#Write out this datafile for further use
write.csv(alldata, "PCAP-alldata-2010-2011.csv")

#Calculate Richness for the plots as whole. We will later separate it out by plot.
richness <- aggregate(alldata$COVER, list(plot = PLOT, year = YEAR, native = NATIVE), FUN = length)
detach(alldata)
summary(richness)

# We then rename the data accoridng to its plot (x) with the "names" command and summarize the results
names(richness)[names(richness) == "x"] <- "richness.plot"
summary(richness)

# Install the "Hmisc" library so that we can graph the data
library("Hmisc")

# Do standard error calculations on the data (x). This has to be done by hand because there is no Standard Error built into R. Therefore we create data called "StdError"
# The calculation for standard error is the square root (sqrt) of variance (var) divided by the totaly number of values (length)
StdErr <- function(x) sqrt(var(x) / length(x))

# We then split the native and nonnative subsets of species into 2 separate data frames "rich.native" and "rich.nonnnative" using the "subset" command and set each to NATIVE or NONNATIVE using Logical values (TRUE/FALSE)
rich.native <- subset(richness, native == TRUE)
rich.nonnative <- subset(richness, native == FALSE)

# "par" sets up the parameters in which our graph will occur. "plot" sets up the scale and names axes using x, y arguments.
par(cex.axis = 1.5, cex.lab = 1.75, las = 1, bty = "l")
plot(c(2004, 2010), c(0,8), type = "n", xlab = "year", ylab = "species richness")

# We then set which vector applies to the x value. We offse x-values using "unique" and adding 0.1 to each value
# We then calculate a series of means that we will plot and then we create a series of standard errors to plot
# We then add error bars to closed circles of the graphed means
# And use "lines" to attach lines between the plotted values
x <- unique(rich.native$year) + 0.1
y <- tapply(rich.native$richness.plot, rich.native$year, mean)
ysem <- tapply(rich.native$richness.plot, rich.native$year, StdErr)
errbar(x, y, y + ysem, y - ysem, add = TRUE, pch = 19, cap = 0.02, cex = 2)
lines (x, y, lty = 1)

# We then do something similar for the nonnative species
x <- unique(rich.nonnative$year) - 0.1
y <- tapply(rich.nonnative$richness.plot, rich.nonnative$year, mean)
ysem <- tapply(rich.nonnative$richness.plot, rich.nonnative$year, StdErr)
errbar(x, y, y + ysem, y - ysem, add = TRUE, pch = 21, cap = 0.02, cex = 2)
lines (x, y, lty = 2)

# Use the "legend command to make a legend describing what each value means
legend(2008, 2, c("native","nonnative"), bty="n", pch=c(19, 21), lty = c(1,2), cex=1.5)

# This results in a clear and finished graph of the calculations we made.