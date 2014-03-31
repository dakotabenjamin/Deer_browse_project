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

#Read in the Species data
species <- read.csv("PCAP Data/species-list-PCAP.csv")

#read in the plot data
plots <- read.csv("PCAP Data/pcap-plot-info.csv")
plots.RR <- subset(plots, reservation.code=="RR")

#Read in the cover data
cover2010 <- read.csv("PCAP Data/pcap-cover-2010.csv")
cover2011 <- read.csv("PCAP Data/pcap-cover-2011.csv")
allcover <- rbind(cover2010,cover2011)
cover.RR <- merge(allcover, plots.RR)

#Read in the distance data
distance <- read.csv("Dakota_Benjamin_distance.csv")

#Read in the dist data
disturbance <- read.csv("PCAP Data/pcap-disturbance.csv")[1:4]

#Lets manipulate the deer browse so that the ranks are easier to look at
dist.deerbrowse <- colsplit(distance$Deer.Browse.Ratings, "=", c("DBCode", "DBLabel"))
distance <- cbind(distance, dist.deerbrowse)

distance$deerbrowse <- 0
distance$deerbrowse[distance$DBCode == "None recorded"] <- 0
distance$deerbrowse[distance$DBCode == "VL"] <- 1
distance$deerbrowse[distance$DBCode == "L"] <- 2
distance$deerbrowse[distance$DBCode == "ML"] <- 3
distance$deerbrowse[distance$DBCode == "M"] <- 4
distance$deerbrowse[distance$DBCode == "MH"] <- 5
distance$deerbrowse[distance$DBCode == "H"] <- 6
distance$deerbrowse[distance$DBCode == "VH"] <- 7

hist(distance$deerbrowse, 7)


#Play with the spatial data
spa_data <- readOGR("pcap_da_join_subset", "pcap_da_join_subset")

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

names(distance)[names(distance)=="site.name"] <- "SITE_NAME"
distance <- merge(distance, spa_data)

# Load the Reservation map
reservations <- readOGR("reservations", "reservation_boundaries_public_private_cm")
plot(reservations)

rocky.river <- readOGR("reservations", "reservation_boundaries_public_private_cm_RES__Rocky River Reservation")
plot(rocky.river)
