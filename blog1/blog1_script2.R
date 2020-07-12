#################################################################################
#                             yildizanil.github.io                              #
#                                Blog post 1                                    #
#                       written by Anil Yildiz, Dr. sc.                         #
#      Calculating monthly, daily, and hourly means of time series data         #
#                         Created on 12.07.2020                                 #
#                       Last edited on 12.07.2020                               #
#################################################################################
#-------------------------------------------------------------------------------#
#                              Importing the dataset                            #
#-------------------------------------------------------------------------------#
Z250_raw <- readRDS("Z250.rds")
Z250_raw[,2:4] <- sapply(Z250_raw[,2:4],as.numeric)
Z250_raw[which(Z250_raw[,2]=="NaN"),2] <- NA
Z250_raw[which(Z250_raw[,3]=="NaN"),3] <- NA
Z250_raw[which(Z250_raw[,4]=="NaN"),4] <- NA
#-------------------------------------------------------------------------------#
#                 Defining the start and end of the time steps                  #
#-------------------------------------------------------------------------------#
start <- as.POSIXct("2019-05-01 00:00:00","UTC")
end <- as.POSIXct("2019-11-01 00:00:00","UTC")-1
daily_steps <- seq(start,end,60*60*24)
monthly_steps <- seq.Date(as.Date(start),as.Date(end),"month")
#-------------------------------------------------------------------------------#
#                       Calculating the daily mean value                        #
#-------------------------------------------------------------------------------#
which_daily_step <- findInterval(as.POSIXct(Z250_raw[,1],tz="UTC"),daily_steps)
Z250_daily <- aggregate(Z250_raw$VWC,list(daily_steps[which_daily_step]),mean,na.rm=T)
#-------------------------------------------------------------------------------#
#                     Calculating the monthly mean value                        #
#-------------------------------------------------------------------------------#
which_monthly_step <- findInterval(as.POSIXct(Z250_raw[,1],tz="UTC"),as.POSIXct(monthly_steps))
Z250_monthly <- aggregate(Z250_raw$VWC,list(monthly_steps[which_monthly_step]),mean,na.rm=T)
