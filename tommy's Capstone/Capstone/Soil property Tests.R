# Soil property Testing
# Must have run through Capstone Script to have the proper data generated

# Get the Data First
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

#Test normality

hist(soildata$K)
hist(log(soildata$Ca)) #log
hist(log(soildata$pH)) #log
hist(log(soildata$N.tot)) #log

# Build models For Native

N.K <- lm(soildata.native$rich~soildata.native$K)
N.Ca <- lm(soildata.native$rich~log(soildata.native$Ca))
N.pH <- lm(soildata.native$rich~log(soildata.native$pH))
N.N.tot <- lm(soildata.native$rich~log(soildata.native$N.tot))

# Build models for Adventive
A.K <- lm(soildata.adventive$rich~soildata.adventive$K)
A.Ca <- lm(soildata.adventive$rich~log(soildata.adventive$Ca))
A.pH <- lm(soildata.adventive$rich~log(soildata.adventive$pH))
A.N.tot <- lm(soildata.adventive$rich~log(soildata.adventive$N.tot))

# Look at ANOVAs of each model

anova(N.K)
anova(N.Ca) # ***
anova(N.pH) # ***
anova(N.N.tot)
######################################################
anova(A.K)
anova(A.Ca) # ***
anova(A.pH) # ***
anova(A.N.tot) # *

# Set Par
par(mfcol=c(1,3))
# Ca graph
plot(log(soildata.native$Ca), soildata.native$rich, col = "green", xlab= "Log of Calcium Concentration in PPM", ylab="Species Richness", main= "Calcium and Species Richness")
points(log(soildata.adventive$Ca),soildata.adventive$rich, col="blue")
abline(A.Ca, col = "blue")
abline(N.Ca, col = "green")
legend(7.25,80, c("adventive", "native"), pch = 21, col = c("blue", "green")) #BUILD A LEGEND HERE

# pH graph
plot(log(soildata.native$pH), soildata.native$rich, col = "green", xlab= "Log of pH Concentration in PPM", ylab="Species Richness", main= "pH and Species Richness")
points(log(soildata.adventive$pH),soildata.adventive$rich, col="blue")
abline(A.pH, col = "blue")
abline(N.pH, col = "green")
legend(1.9,80, c("adventive", "native"), pch = 21, col = c("blue", "green")) #BUILD A LEGEND HERE

# N.tot graph
plot(log(soildata.native$N.tot), soildata.native$rich, col = "green", xlab= "Log of Nitrogen in % Concentration", ylab="Species Richness", main= "Nitrogen and Species Richness")
points(log(soildata.adventive$N.tot),soildata.adventive$rich, col="blue")
abline(A.N.tot, col = "blue")
# NOT SIGNIFICANT abline(N.N.tot, col = "green")
legend(-.75,80, c("adventive", "native"), pch = 21, col = c("blue", "green")) #BUILD A LEGEND HERE

# WELL FUCK

# Look at the distance and its relationship toCa, pH, and N.tot
dist.Ca <- lm(soildata$distance~log(soildata$Ca))
dist.pH <- lm(soildata$distance~log(soildata$pH))
dist.N.tot <- lm(soildata$distance~log(soildata$N.tot))

anova(dist.Ca)
anova(dist.pH)
anova(dist.N.tot)

par(mfcol=c(1,3))
plot(log(soildata$Ca),soildata$distance, col="green", main="Calcium and relationship to Distance", xlab="Log of Calcium Concentration in PPM", ylab="Distance from Dean Man's Curve (degrees)")
abline(dist.Ca)
plot(log(soildata$pH),soildata$distance, col="blue", main="pH and relationship to Distance", xlab="Log of pH in PPM", ylab="Distance from Dean Man's Curve (degrees)")
abline(dist.pH)
plot(log(soildata$N.tot),soildata$distance,col="red", main="Nitrogen and relationship to Distance", xlab="Log of Nitrogen in % Concentration", ylab="Distance from Dean Man's Curve (degrees)")
abline(dist.N.tot)



