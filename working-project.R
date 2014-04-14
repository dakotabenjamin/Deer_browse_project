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



all.distance <- merge(deerb, spa_data)
distance <- subset(all.distance, Community.type > 41 | deerbrowse != 0)
distance <- subset(distance, !is.na(distance$deerbrowse))
distance <- subset(distance, RES == "South Chagrin" | RES == "North Chagrin" | RES == "Bedford" | RES == "Brecksville" | RES == "Rocky River" | RES == "Hinckley" | RES == "Mill Stream Run")


# A quick analysis of a model shows that there is significance in the DEV_DIST data on the deer browse. 
aovmod <- aov(deerbrowse ~ DEV_DIST * EDGE_DIST, data=distance)
summary(aovmod)                

par(mfcol = c(2,2))
plot(aovmod)

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
plot(distance$DEV_DIST, distance$deerbrowse, main = "Deer Browse by Distance to Nearest Developed Edge", xlab="Distance to Nearest Developed Edge (m)", ylab="Deer Browse Rating")

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

mean.dist <- aggregate(distance$DEV_DIST, list(distance$RES), mean)
mean.disterr <- aggregate(distance$DEV_DIST, list(distance$RES), StdErr)
mean.deer <-aggregate(distance$deerbrowse, list(distance$RES), mean)
mean.deererr <-aggregate(distance$deerbrowse, list(distance$RES), StdErr)

mean.all <- merge(mean.dist, mean.deer, by = "Group.1") #, subset x.x<300 & x.y>0
names(mean.all) <- c("Reservation", "dist", "browse")
#log scale on the mean
#mean.all$logdeer <- log(mean.all$browse)

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

#distlst <- mean.all$dist[distance$deerbrowse]
#plot(distlst, distance$deerbrowse)
     
#-----
# Scatter plot of deer browse ratings by distance to edge with coloration to distiguish each plot
clrs <- c("red", "orange2", "gold", "green", "blue", "violet", "brown4", "black", "pink", "navy", "darkred", "coral", "cyan", "palegreen", "peachpuff", "yellowgreen", "tan")
clrslst <- clrs[distance$Reservation]
par(mfcol=c(1,1))
plot(distance$DEV_DIST, deerbrowse, pch=20, col=clrslst, main = "Deer Browse by Distance to Nearest Developed Edge", xlab="Distance to Nearest Developed Edge (m)", ylab="Deer Browse Rating")
tot.reg <- lm(deerbrowse ~ DEV_DIST, data=distance)
abline(tot.reg, col="red")
summary(tot.reg)

#----
#read in the reservation map data and do the regressions
rocky.river <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Rocky River Reservation")
s.chagrin <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__South Chagrin Reservation")
n.chagrin <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__North Chagrin Reservation")
bedford <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Bedford Reservation")
brecksville <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Brecksville Reservation")
millstreamrun <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Mill Stream Run Reservation")
hinckley <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Hinckley Reservation")

rr.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Rocky River"))
sch.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="South Chagrin"))
nch.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="North Chagrin"))
bdf.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Bedford"))
bre.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Brecksville"))
msr.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Mill Stream Run"))
hink.reg <- lm(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Hinckley"))

#----
#plot each reservation
#-----
par(mfrow=c(2,7))

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Rocky River"), main = "Rocky River", ylim=c(0,6))
abline(rr.reg)


plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="South Chagrin"), main = "South Chagrin", ylim=c(0,6))
abline(sch.reg)

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="North Chagrin"), main = "North Chagrin", ylim=c(0,6))
abline(nch.reg)
# summary(nch.reg)

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Bedford"), main = "Bedford", ylim=c(0,6))
abline(bdf.reg)

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Brecksville"), main = "Brecksville", ylim=c(0,6))
abline(bre.reg)

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Mill Stream Run"), main = "Mill Stream Run", ylim=c(0,6))
abline(msr.reg)

plot(deerbrowse ~ DEV_DIST, data=subset(distance, RES=="Hinckley"), main = "Hinckley", ylim=c(0,6))
abline(hink.reg)

plot(rocky.river)
plot(s.chagrin)
plot(n.chagrin)
plot(bedford)
plot(brecksville)
plot(millstreamrun)
plot(hinckley)



#-----
# Do it with VIBI Scores
#-----

par(mfcol=c(1,1))
plot(distance$DEV_DIST, as.numeric(distance$VIBI), pch=20, col=clrslst, main = "VIBI by Distance to Nearest Developed Edge", xlab="Distance to Nearest Developed Edge (m)", ylab="VIBI Score")
tot.reg <- lm(as.numeric(VIBI) ~ DEV_DIST, data=distance)
abline(tot.reg, col="red")

rr.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Rocky River"))
sch.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="South Chagrin"))
nch.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="North Chagrin"))
bdf.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Bedford"))
bre.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Brecksville"))
msr.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Mill Stream Run"))
hink.regVIBI <- lm(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Hinckley"))


#plot each reservation

par(mfrow=c(1,7))

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Rocky River"), main = "Rocky River")
abline(rr.regVIBI)


plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="South Chagrin"), main = "South Chagrin")
abline(sch.regVIBI)

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="North Chagrin"), main = "North Chagrin")
abline(nch.regVIBI)
# summary(nch.reg)

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Bedford"), main = "Bedford")
abline(bdf.regVIBI)

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Brecksville"), main = "Brecksville")
abline(bre.regVIBI)

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Mill Stream Run"), main = "Mill Stream Run")
abline(msr.regVIBI)

plot(as.numeric(VIBI) ~ DEV_DIST, data=subset(distance, RES=="Hinckley"), main = "Hinckley")
abline(hink.regVIBI)

plot(rocky.river)
plot(s.chagrin)
plot(n.chagrin)
plot(bedford)
plot(brecksville)
plot(millstreamrun)
plot(hinckley)

#----
#Calculating Moran's I
#------
# library(ape)
# gisdata <- read.csv("PCAP Data/pcap-gis-data.csv")
# plots <- distance$SITE_NAME
# plots1 <- match(gisdata$plot, plots, nomatch=0)
# moran.dists <- as.matrix(dist(cbind(gisdata$long, gisdata$lat)))
# moran.dists.inv <- 1/moran.dists
# diag(moran.dists.inv) <- 0
# Moran.I(distance$deerbrowse, moran.dists.inv, na.rm=T)
# 
# PCA Analysis
# First put the data we want in a matrix
