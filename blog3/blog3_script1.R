#################################################################################
#                             yildizanil.github.io                              #
#                                Blog post 3                                    #
#                       written by Anil Yildiz, Dr. sc.                         #
#                           Complex panel plots in R                            #
#                         Created on 26.07.2020                                 #
#                       Last edited on 26.07.2020                               #
#################################################################################
#-------------------------------------------------------------------------------#
# panel
#-------------------------------------------------------------------------------#
panel <- matrix(NA,6,6)
#-------------------------------------------------------------------------------#
# figs 1a-1d
#-------------------------------------------------------------------------------#
panel[2,2:3] <- c(1,1)
panel[2,4:5] <- c(2,2)
panel[3,2:3] <- c(3,3)
panel[3,4:5] <- c(4,4)
#-------------------------------------------------------------------------------#
# figs 1e-1h
#-------------------------------------------------------------------------------#
panel[5,2:5] <- c(5,6,7,8)
#-------------------------------------------------------------------------------#
# spaces at rows 1, 4 and 6 for legend and common x-axis labels
#-------------------------------------------------------------------------------#
panel[1,] <- rep(9,6)
panel[4,] <- rep(10,6)
panel[6,] <- rep(11,6)
#-------------------------------------------------------------------------------#
# spaces for common y-axis labels
#-------------------------------------------------------------------------------#
panel[2:3,1] <- c(12,12)
panel[5,1] <- c(13)
panel[2:3,6] <- c(14,14)
panel[5,6] <- c(15)
#-------------------------------------------------------------------------------#
# setting the layout
#-------------------------------------------------------------------------------#
layout(panel,heights=c(5,25,25,5,35,5),widths=c(5,35,35,35,35,5))
#-------------------------------------------------------------------------------#
# figures 1a-1d
#-------------------------------------------------------------------------------#
for(i in 4:1)
{
   par(mar=c(1,1.25,0.25,1.25),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1)
   plot(0,0,type="l",lwd=3,ylim=c(0.6,1.6),xlim=c(startdate,enddate),axes=F,xlab=NA,ylab=NA,pch="")
   segments(x0=seq(startdate,enddate,60*60*24),y0=0.6,x1=seq(startdate,enddate,60*60*24),y1=1.6,col=rgb(0,0,0,0.1),lty=1)
   segments(x0=startdate,y0=seq(0.6,1.6,0.1),x1=enddate,y1=seq(0.6,1.6,0.1),col=rgb(0,0,0,0.1),lty=1)
   axis(2,tck=0.02)     
   axis(1,tck=0.02,at=axis_days,labels=NA)
   axis(1,tck=0.04,at=axis_months,labels=axis_names)
   box()
   
   par(new=T)
   plot(0,0,type="l",lwd=3,ylim=c(0,30),xlim=c(startdate-60*60*48,enddate),axes=F,xlab=NA,ylab=NA,pch="")
   axis(4,tck=0.02)
   
   par(las=0)
   text(enddate,30,paste0("(",letters[5-i],")"),adj=c(1,1))
}
#-------------------------------------------------------------------------------#
# figures 1e-1h
#-------------------------------------------------------------------------------#
breaks <- seq(0.6,1.5,0.05)
for(i in 4:1)
{
   freq <- aggregate(tc[,i+1],list(breaks[findInterval(tc[,i+1],breaks)]),length)
   par(las=1,mar=c(1,1.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=10,pty="s")
   plot(0,0,ylim=c(0,450),xlim=c(0.6,1.5),axes=F,pch="",xlab=NA,ylab=NA)
   segments(x0=seq(0.6,1.50,0.05),y0=0,x1=seq(0.6,1.50,0.05),y1=450,col=rgb(0,0,0,0.1))
   segments(x0=0.6,y0=seq(0,450,50),x1=1.5,y1=seq(0,450,50),col=rgb(0,0,0,0.1))
   axis(1,tck=0.02,at=seq(0.6,1.4,0.2))
   axis(2,tck=0.02)
   par(new=T)
   box()
   
   segments(x0=0.6,y0=0,x1=1.5,y1=0,col=1)
   text(0.6,450,paste0("(",letters[9-i],")"),adj=c(0,1))
   rm(list="freq")
}
#-------------------------------------------------------------------------------#
# legend
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
legend("center",c("Thermal conductivity","Volumetric water content"),
       col=c(green,blue),lty=c(3,1),lwd=c(2,2),hor=T,bty="n")
#-------------------------------------------------------------------------------#
# Figs 1a-1d x-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
text(0,0,"Time [dd-mm-2019]",adj=c(0.5,0.5))
#-------------------------------------------------------------------------------#
# Figs 1e-1h x-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
text(0,0,expression(paste("Thermal conductivity, "," ",lambda," ","[W/mK]")),adj=c(0.5,0.5))
#-------------------------------------------------------------------------------#
# Figs 1a-1d first y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,expression(paste("Thermal conductivity, "," ",lambda," ","[W/mK]")),
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# Figs 1e-1h first y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,"No. of observations [-]",
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# Figs 1a-1d second y-axis label
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
par(las=0)
text(0,0,expression(paste("Volumetric water content, "," ",theta," ","[%]")),
     adj=c(0.5,0.5),srt=90)
#-------------------------------------------------------------------------------#
# empty plot
#-------------------------------------------------------------------------------#
par(mar=c(0,0,0,0),mgp=c(0.1,0.1,0),family="serif",ps=10,cex=1,cex.main=1,las=1,pty="m")
plot(0,0,axes=F,xlab=NA,ylab=NA,pch="")
dev.off()

