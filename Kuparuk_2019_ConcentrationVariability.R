# Caroline Weidner
# Carbonsheds
# 8/17/2020

# Version 1 - Discharge Binning and Concentration variability
# Description: This code uses data for discharge (Q) and concentration (DOC) from the Kuparuk in 2019.
# This code bins discharge based on baseflow and high flow and creates plots of DOC variability for these discharge bands

# Remove all previous variables in R (clean slate)
rm(list=ls(all=T))

# Load Hydrostats package
library(hydrostats)

# Read in data file
data_Q <- read.csv(file= "Kuparuk_Q_2019.csv", sep= ",",header= T)
data_chem <- read.csv(file= "Kuparuk_Chem_2017_2019.csv", sep= ",",header= T)

# Pulling out discharge from data
Q <- data_Q$Discharge_m3s

# Converting date
data_Q$Date = as.POSIXct(data_Q$Date, format="%m/%d/%Y %H:%M")

#Set a start time to convert time stamp into minutes since start of the year#
start.time = as.POSIXct("2019-01-01", format="%Y-%m-%d", tz="UTC")

#Convert timestamp to minutes since start of the year
data_Q$min.since = as.numeric(difftime(data_Q$Date, start.time, units="mins"))

#Interpolation function for Q~time#
Q.fun.2019 = approxfun(x=data_Q$min.since, y=data_Q$Discharge_m3s)

#Read water chemistry data and processing time stamp#
chem.data = read.csv("Kuparuk_Chem_2017_2019.csv")
chem.data$DateTime = as.POSIXct(chem.data$DateTime, format="%Y-%m-%d %H:%M", tz="UTC")
chem.data$min.since = as.numeric(difftime(chem.data$DateTime, start.time, units="mins"))

#Selection 2019 data#
chem.2019 = with(chem.data, chem.data[min.since>0 & min.since<365*24*60 & DOC_mgL>0,])
#Calculate discharge at the time of water chemistry measurement#
chem.2019$Q = Q.fun.2019(chem.2019$min.since)
chem.2019 = with(chem.2019, chem.2019[!is.na(Q),])

# Change DateTime column name to Date
names(chem.2019)[names(chem.2019) == "DateTime"] <- "Date"

# Calculate Baseflow for chem.2019 using hydrostats package
baseflow <- baseflows(chem.2019, 0.975, n.reflected = 30, ts = "mean")

# Create variable for mean baseflow
mean.bf <- baseflow$mean.bf

# Calculate high flow for chem.2019 using hydrostats
highspells <- high.spells(chem.2019,quant=0.9,threshold=NULL,volume=TRUE)

# Create variable for high glow
highflow <- highspells$high.spell.threshold

# Sort chem.2019 from low to high Q, only selecting columns for date,Q, and DOC
chem.2019_sort <- chem.2019[order(chem.2019$Q),c(1,3,5)]

# Plotting
# Ploting Q over time
plot(chem.2019$Date,chem.2019$Q,type="l",ylab="Discharge (m3/s)",xlab="Date")

# Plotting DOC vs Q for sorted data
plot(chem.2019_sort$DOC_mgL,chem.2019_sort$Q,xlab="Discharge (m3/s)",ylab="DOC (mg/L)")

# Creating bins of discharge
Q_bin_1 <- subset(chem.2019_sort, Q > 0 & Q < mean.bf) # bin for baseflow
Q_bin_2 <- subset(chem.2019_sort, Q >= mean.bf & Q < highflow) # bin between baseflow and high flow
Q_bin_3 <- subset(chem.2019_sort, Q >= highflow) # bin for high flow

# Making plot window for 3 plots
par(mfrow=c(1,3), mar=c(4,4,1.5,1), cex=0.85)

# Plotting Q vs. DOC based on discharge bins
plot(Q_bin_1$Q,Q_bin_1$DOC_mgL,xlab="Discharge (m3/s)",ylab="DOC (mg/L)",pch=20,type="p",main="Baseflow",cex.main=1,log="xy",ylim=c(2,8))
plot(Q_bin_2$Q,Q_bin_2$DOC_mgL,xlab="Discharge (m3/s)",ylab="DOC (mg/L)",pch=20,type="p",main="Moderate Flow",cex.main=1,log="xy",ylim=c(2,8))
plot(Q_bin_3$Q,Q_bin_3$DOC_mgL,xlab="Discharge (m3/s)",ylab="DOC (mg/L)",pch=20,type="p",main="High Flow",cex.main=1,log="xy",ylim=c(2,8))
