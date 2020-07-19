#################################################################################
#                             yildizanil.github.io                              #
#                                Blog post 2                                    #
#                       written by Anil Yildiz, Dr. sc.                         #
#      Temperature contour plots showing variation in depth and time            #
#                         Created on 19.07.2020                                 #
#                       Last edited on 19.07.2020                               #
#################################################################################
#-------------------------------------------------------------------------------#
#                       Data from 03.05.2019 until 18.07.2019                   #
#-------------------------------------------------------------------------------#
startdate <- as.POSIXct("2019-05-03 00:00:00",tz="UTC")
enddate <- as.POSIXct("2019-07-18 00:00:00",tz="UTC")
#-------------------------------------------------------------------------------#
#sets the working directory for home
#-------------------------------------------------------------------------------#
setwd("C:/Users/Anil/Desktop/Meteo/SoilTemperature")

soiltemp <- read.csv("SoilTemperature_15Min.csv",header=T,stringsAsFactors=F)
airtemp <- read.csv("AirTemp_15Min.csv",header=T,stringsAsFactors=F)

soiltemp <- soiltemp[which(as.POSIXct(soiltemp[,1],"UTC")>startdate-1&as.POSIXct(soiltemp[,1],"UTC")<enddate-1),]
airtemp <- airtemp[which(as.POSIXct(airtemp[,1],"UTC")>startdate-1&as.POSIXct(airtemp[,1],"UTC")<enddate-1),]

summary(airtemp)
summary(soiltemp)

axis_seq <- as.character(seq.Date(as.Date(startdate+1)-2,as.Date(enddate+1),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)

depth <- as.numeric(substr(colnames(soiltemp[2:14]),start=2,stop=4))
time_steps <- seq(startdate,enddate,60*60*0.25)
temp_steps <- seq(2,26,1)

heat.colors(length(temp_steps))[(length(temp_steps)+1)-findInterval(soiltemp[i,j],temp_steps)]