#################################################################################
#                             yildizanil.github.io                              #
#                                Blog post 1                                    #
#                       written by Anil Yildiz, Dr. sc.                         #
#      Calculating monthly, daily, and hourly means of time series data         #
#                         Created on 12.07.2020                                 #
#                       Last edited on 12.07.2020                               #
#################################################################################
#-------------------------------------------------------------------------------#
#                   Link to download the dataset from NGIF API                  #
#-------------------------------------------------------------------------------#
link <- "https://api.ngif.urbanobservatory.ac.uk/api/v2/sensors/timeseries/54780465-b11a-4d6c-8465-d82db47d69df/historic?startTime=2020-07-10T00:00:01.000Z&endTime=2020-07-11T23:59:59.000Z&outputAs=csv"
#-------------------------------------------------------------------------------#
#                              Importing the dataset                            #
#-------------------------------------------------------------------------------#
airtemp_raw <- read.csv(link,skip=9,header=T,stringsAsFactors=F)
#-------------------------------------------------------------------------------#
#                 Defining the start and end of the time steps                  #
#-------------------------------------------------------------------------------#
start <- as.POSIXct("2020-07-10 00:00:00","UTC")
end <- as.POSIXct("2020-07-12 00:00:00","UTC")
time_steps_1h <- seq(start,end,60*60)
time_steps_3h <- seq(start,end,60*60*3)
time_steps_6h <- seq(start,end,60*60*6)
#-------------------------------------------------------------------------------#
#                       Calculating the mean value for 1 hour                   #
#-------------------------------------------------------------------------------#
which_step_1h <- findInterval(as.POSIXct(airtemp_raw[,1],tz="UTC"),time_steps)
airtemp_1h <- aggregate(airtemp_raw[,2],list(time_steps_1h[which_step_1h]),mean)
#-------------------------------------------------------------------------------#
#                       Calculating the mean value for 3 hours                  #
#-------------------------------------------------------------------------------#
which_step_3h <- findInterval(as.POSIXct(airtemp_raw[,1],tz="UTC"),time_steps_3h)
airtemp_3h <- aggregate(airtemp_raw[,2],list(time_steps_3h[which_step_3h]),mean)
#-------------------------------------------------------------------------------#
#                       Calculating the mean value for 6 hours                  #
#-------------------------------------------------------------------------------#
which_step_6h <- findInterval(as.POSIXct(airtemp_raw[,1],tz="UTC"),time_steps_6h)
airtemp_6h <- aggregate(airtemp_raw[,2],list(time_steps_6h[which_step_6h]),mean)
#-------------------------------------------------------------------------------#
# Defining a time vector for each mean value to plot at increments of half of   #
# the time step                                                                 #
#-------------------------------------------------------------------------------#
time_1h <- (as.POSIXct(airtemp_1h[,1],"UTC")+60*30)
time_3h <- (as.POSIXct(airtemp_3h[,1],"UTC")+60*60*1.5)
time_6h <- (as.POSIXct(airtemp_6h[,1],"UTC")+60*60*3)
#-------------------------------------------------------------------------------#
#                                 Figure 1                                      #
#-------------------------------------------------------------------------------#
png("Airtemp_mean.png",width=120,height=60,units="mm",res=500)
layout(mat=matrix(c(1,2),nrow=2,ncol=1),heights=c(1,8))
par(mar=c(0.25,0.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,las=1,family="mono",cex=1,cex.main=1)
plot(0,0,xlab=NA,ylab=NA,pch="",axes=F)
legend("center",c("Raw","1-hour","3-hour","6-hour"),hor=T,lwd=2,col=c(1,2,3,4),bty="n")

par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),ps=10,las=1,family="mono")
plot(0,0,xlab=NA,ylab=NA,xlim=c(time_steps[1],tail(time_steps,1)),ylim=c(8,20),axes=F)
segments(x0=time_steps,y0=8,x1=time_steps,y1=20,col=rgb(0,0,0,0.1))
segments(x0=time_steps[1],y0=seq(8,20,1),x1=tail(time_steps,1),y1=seq(8,20,1),col=rgb(0,0,0,0.1))
axis(2,tck=0.02)
axis(1,tck=0.01,at=time_steps[seq(1,49,6)],labels=substr(time_steps[seq(1,49,6)],12,16))
box()
lines(airtemp_raw[,2]~as.POSIXct(airtemp_raw[,1],"UTC"),lwd=2)
lines(airtemp_1h[,2]~time_1h,lwd=2,col=2)
lines(airtemp_3h[,2]~time_3h,lwd=2,col=3)
lines(airtemp_6h[,2]~time_6h,lwd=2,col=4)
par(las=0)
mtext("Time [hh:mm]",side=1,line=1)
mtext("Air temperature [°C]",side=2,line=1.25)
rect(xleft=start,ybottom=8,xright=start+60*60*24,ytop=20,border=NA,col=rgb(0,0,0,0.1))
text(start,20,"2020-07-10",adj=c(0,1))
text(start+60*60*24,20,"2020-07-11",adj=c(0,1))
dev.off()