#--------------------------------------------------------------
#Dakota Benjamin
#2014.03.19
#Script for 315/415 Lab 4
#Statistical Analysis of Data 
#--------------------------------------------------------------

rm(list=ls())

library(reshape2)
library(sp)
library(rgdal)

setwd("C:/Users/Dakota/Dropbox/_CASE/_Spring2014/315/Deer browse project/")

# Look at reservations with the highest animal disturbance
# potentially reservations that are more round in shape and more long in shape
# spcatial autocorrelation

#read in the plot data
plots <- read.csv("PCAP Data/pcap-plot-info.csv")

#Read in the distance data
distance <- read.csv("Dakota_Benjamin_distance.csv")

#Read in the dist data
disturbance <- read.csv("PCAP Data/pcap-disturbance.csv")[1:4]

#----
# #Lets manipulate the deer browse so that the ranks are easier to look at
#----
# dist.deerbrowse <- colsplit(distance$Deer.Browse.Ratings, "=", c("DBCode", "DBLabel"))
# distance <- cbind(distance, dist.deerbrowse)
# 
# distance$deerbrowse <- 0
# distance$deerbrowse[distance$DBCode == "None recorded"] <- 7
# distance$deerbrowse[distance$DBCode == "VL"] <- 0
# distance$deerbrowse[distance$DBCode == "L"] <- 1
# distance$deerbrowse[distance$DBCode == "ML"] <- 2
# distance$deerbrowse[distance$DBCode == "M"] <- 3
# distance$deerbrowse[distance$DBCode == "MH"] <- 4
# distance$deerbrowse[distance$DBCode == "H"] <- 6
# distance$deerbrowse[distance$DBCode == "VH"] <- 7
# 
# hist(distance$deerbrowse, breaks = -0.5+0:8, ylim=c(0,120), xlab="Deer Browse Rating")

#----
#A basic histogram of the deer browse rankings
#-----
# Read in the PCAP non-vegetative summary with deer browse data 
pcapsummary <- read.csv("PCAP Data/PCAP_4YR_SUMMARY.csv")
pcapsummary$site.name <- as.factor(pcapsummary$site.name)

deerb <- subset(pcapsummary, !is.na(site.name))
deerb <- subset(deerb, !is.na(deerbrowse))

names(deerb)[names(deerb)=="site.name"] <- "SITE_NAME"
names(deerb)[names(deerb)=="Deer.Browse.Ratings"] <- "deerbrowse"

deerb$deerbrowse[deerb$deerbrowse == 7] <- 0

hist(deerb$deerbrowse, breaks = -0.5+0:8)

#----
#Play with the spatial data
#----
spa_data <- readOGR("pcap_da_join_subset", "pcap_da_join_subset")
spa_data$RES[spa_data$RES == "Euclis Creek"] <- "Euclid Creek"

# SITE_NAME: plot number
# PCAP_ID: Unique id?
# POLYGON_ID: probably refers to other metroparks data polygons

# Distance-to-Edge:
# Edges are comprised of roads/RR, APT's, streams, use areas (mowed areas/picnic areas/parking lots/buildings) 
# 
# "edge_dist": distance to nearest edge (including edges which are streams)
# "dev_dist": distance to nearest developed edge (not including streams)
# 
# Distance-to-Trails:
# 
# "trail_dist": distance to nearest trail (sanctioned or bootleg)
# "sanct_dist": distance to nearest sanctioned (marked) trail
# "boot_dist": distance to nearest bootleg trail



distance <- merge(deerb, spa_data)

# A quick analysis of a model shows that there is significance in the DEV_DIST data on the deer browse. 
mod1 <- aov(deerbrowse ~ DEV_DIST, data=distance)
summary(mod1)

#               Df Sum Sq Mean Sq F value Pr(>F)  
# EDGE_DIST     1    3.3   3.316   1.379 0.2410  
# DEV_DIST      1    9.3   9.345   3.887 0.0494 *
#   Residuals   395  949.8   2.404                 

par(mfcol = c(2,2))
plot(mod1)

#The plots
par(mfcol = c(1,1))
plot(spa_data)

#----
#Mapping the reservations
#--------
# Load the Reservation map
reservations <- readOGR("reservations", "reservation_boundaries_public_private_cm")
plot(reservations)


rocky.river <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Rocky River Reservation")
plot(rocky.river)

#----
# A Linear Regression Model of deer browse level by distance from nearest developed edge
#------

#scatter plot of deer browse level to distance by plot then plot a linear fit
par(mfcol=c(1,1))
plot(distance$EDGE_DIST, distance$deerbrowse, main = "Deer Browse by Distance to Nearest Developed Edge", xlab="Distance to Nearest Developed Edge (m)", ylab="Deer Browse Rating")

#regression model

reg1 <- lm(distance$deerbrowse ~ distance$DEV_DIST)
summary(reg1)
abline(reg1, col="red")
anova(reg1)
par(mfcol=c(2,2))
plot(reg1)

#----
# A Linear Regression Model of deer browse by average nearest developed edge by reservation
#-----
#load HMisc
library(Hmisc)
#A function for standard error
StdErr <- function(x) sqrt(var(x) / length(x))

distance <- subset(distance, !is.na(distance$deerbrowse))
mean.dist <- aggregate(distance$EDGE_DIST, list(distance$RES), mean)
mean.disterr <- aggregate(distance$EDGE_DIST, list(distance$RES), StdErr)
mean.deer <-aggregate(distance$deerbrowse, list(distance$RES), mean)
mean.deererr <-aggregate(distance$deerbrowse, list(distance$RES), StdErr)

mean.all <- subset(merge(mean.dist, mean.deer, by = "Group.1")) #, x.x<300 & x.y>0
names(mean.all) <- c("Reservation", "dist", "browse")
#log scale on the mean
mean.all$logdeer <- log(mean.all$browse)

par(mfcol=c(1,1))
#plot(mean.all$dist, mean.all$browse)
errbar(mean.all$dist, mean.all$browse, mean.all$browse + mean.deererr$x, mean.all$browse - mean.deererr$x, xlab="Distance to Edge",ylab="Deer Browse Rating", main="Mean Deer Browse by Mean Distance to Edge")
text(mean.all$dist, mean.all$browse, labels=mean.all$Reservation, pos=3, cex=0.6)
reg2 <- lm(mean.all$browse ~ mean.all$dist)
summary(reg2)
abline(reg2, col="red")
anova(reg2)
par(mfcol = c(2,2))
plot(reg2)

# lowess1 <- loess(mean.all$browse ~ mean.all$dist)
# lines(lowess1)
# summary(lowess1)

#----
#Calculating Moran's I
#------
library(ape)
gisdata <- read.csv("PCAP Data/pcap-gis-data.csv")
moran.dists <- as.matrix(dist(cbind(gisdata$long, gisdata$lat)))
moran.dists.inv <- 1/moran.dists
diag(moran.dists.inv) <- 0
Moran.I(distance$deerbrowse, moran.dists.inv, na.rm=T)
