#################################################################################
#                             yildizanil.github.io                              #
#                                Blog post 2                                    #
#                       written by Anil Yildiz, Dr. sc.                         #
#      Temperature contour plots showing variation in depth and time            #
#                         Created on 19.07.2020                                 #
#                       Last edited on 19.07.2020                               #
#################################################################################
depth <- as.numeric(substr(colnames(soiltemp[2:14]),start=2,stop=4))
time_steps <- seq(startdate,enddate,60*60*0.25)
temp_steps <- seq(2,26,1)

axis_seq <- as.character(seq.Date(as.Date(startdate+1)-2,as.Date(enddate+1),"month"))
axis_names <- paste0("01-",substr(axis_seq,start=6,stop=7))
axis_months <- as.POSIXct(axis_seq,"UTC")
axis_days <- seq(startdate,enddate,60*60*24)

png("blog2_fig1.png",width=120,height=80,units="mm",res=500)
par(mar=c(2,2.25,0.25,0.25),mgp=c(0.1,0.1,0),family="serif",ps=8,cex=1,cex.main=1,las=1)
plot(0,0,xlim=c(startdate,enddate),ylim=c(950,0),type="l",axes=F,xlab=NA,ylab=NA)
segments(x0=seq(startdate,enddate,60*60*24),y0=0,x1=seq(startdate,enddate,60*60*24),y1=950,col=rgb(0,0,0,0.1))

axis(1,tck=0.02,labels=NA,at=axis_days)
axis(1,tck=0.03,labels=axis_names,at=axis_months)
axis(2,tck=0.02,at=depth,labels=depth)
box()
for(j in 1:length(depth))
{
   for(i in 1:nrow(soiltemp))
   {
      rect(xleft=as.POSIXct(soiltemp[i,1],"UTC"),ybottom=depth[j]-20,
           xright=as.POSIXct(soiltemp[i+1,1],"UTC"),ytop=depth[j]+20,
           col=heat.colors(length(temp_steps))[(length(temp_steps)+1)-findInterval(soiltemp[i,j+1],temp_steps)],
           border=NA)
   }
}
rect(xleft=startdate,ybottom=950,xright=enddate,ytop=150,border = T)
rect(xleft=startdate,ybottom=150,xright=enddate,ytop=0,border = T)
par(las=0)
mtext("Depth [mm]",side=2,line=1.25)
mtext("Time [dd-mm-2019]",side=1,line=1)
dev.off()

getwd()
