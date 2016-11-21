# Code for processing ROV nav, event, CTD, and photo data generated during ROV transects
# Applies to WinFrog v2 .DAT files, .LOG files, CTD files, and photo exif data

# Kevin L. Stierhoff

# TO DO:
#

# Script setup #####
# clear system memory
rm(list=ls())
# Set time zone to GMT
Sys.setenv(TZ="GMT")
# load libraries
library(RODBC);library(reshape2);library(plyr);library(ggplot2);library(forecast);
library(stringr);library(surveyR);library(scales);library(grid);library(gridExtra);
suppressPackageStartupMessages(library(cowplot));

# You are about to process DAT, LOG, CTD and PHOTO data for a series of ROV transects
# You will need to supply some information below

# User-defined variables ####
# Enter the directory of the ROV database
# db.dir <- "C:/DATA/ROV_AtSea_20160906.accdb"  # on ROV laptop
db.dir <- "D:/DATA/rov_data/ROV_Master.accdb"  # on KLS desktop

# Enter the start and end dive names (if only processing one dive, make these the same)
start.dir 	<- "15-160A"
end.dir 	  <- "16-264A"
# Is the CTD present (this will almost always be TRUE)
ctd.on <- TRUE
# Which ROV was used (e.g., HDHV or Phantom)
ROV <- "HDHV"
# If the camera time was not set correctly, apply the following time fix
# If the time was set correctly, all numerics in the string below should be zeros
timefix <- '+="0:0:0 0:0:0"' #e.g., -=Y:M:D h:m:s (- or + to subtract or add date/time)
# timefix <- '+="0:0:0 0:0:0"' #e.g., -=Y:M:D h:m:s (- or + to subtract or add date/time)
nav.smoother <- 15
# path to R processing code
# proc.file <- "C:/PROJECTS/dive_proc/dive_proc.R" # on ROV laptop
proc.file <- "D:/CODE/R/KLS_packages/dive_proc/dive_proc.R" # on KLS desktop

# Query starting IDs for all database tables ####
channel <- odbcConnectAccess2007(db.dir)
nav.seed    <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_NAV_DATA.nav_id) AS nav_seed
                                   FROM dbo_tbl_NAV_DATA;")[1]+1)
event.seed  <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_DIVE_EVENTS.event_id) AS event_seed
                                   FROM dbo_tbl_DIVE_EVENTS;")[1]+1)
photo.seed  <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_PHOTO_INFO.photo_id) AS photo_seed
                                   FROM dbo_tbl_PHOTO_INFO;")[1]+1)
ctd.seed    <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_CTD_CASTS.ctd_id) AS ctd_seed
                                   FROM dbo_tbl_CTD_CASTS;")[1]+1)
close(channel)

# Create temporary data frames for storing processing results ####
DAT.temp 	<- data.frame()
PHOTO.temp	<- data.frame()
LOG.temp 	<- data.frame()
CTD.temp 	<- data.frame()

# Process NAV date from DAT files ####
if(.Platform$OS.type == "unix") { 
  # if working on a Mac
  dat.root 	<- "/Users/kevinstierhoff/NAVDATA" 	# This is the WinFrog project directory
  photo.root 	<- "/Users/kevinstierhoff/PHOTOS" # This is the photo directory
} else { 
  # if working on a Windows PC
  dat.root 	<- "C:/NAVDATA" # This is the WinFrog project directory
  photo.root 	<- "C:/PHOTOS"#  This is the photo directory
}
# get a list of WinFrog project directories that contain NAV data
d <- sort(dir(dat.root,recursive=FALSE,pattern='\\d{2}-\\d{3}\\w',full.names=TRUE))
# dir.list <- d[which(d == start.dir):which(d == end.dir)]
dir.list <- d[grep(start.dir,d):grep(end.dir,d)]

# create status bar
pb <- winProgressBar(title="ROV File Processing Progress", label="0% done", min=0, max=100, initial=0)
# set initial variable for counter
j <- 1
for (i in dir.list){
  # Copy this script and the functions source code to the DAT directory
  invisible(file.copy(proc.file,i,overwrite = TRUE))
  # extract dive name from directory path
  dive.name <- 	str_extract(i,'\\d{2}-\\d{3}\\w')
  # list DAT files in directory
  dat.files <- list.files(i,pattern='\\d{2}-\\d{3}\\w{1}(\\(\\d{3}-\\d{6}\\))?.DAT',full.names=TRUE)
  DAT <- data.frame()
  for(ii in dat.files){
    # read DAT file and append to previous
    dat <- read.csv(ii,header=FALSE)
    DAT <- rbind(DAT,dat)
  }
  # subset columns
  DAT <- DAT[ ,c(1:47,65)]
  # add variable names to data frame
  names(DAT) <- c("oid","blank","date.time","lat.r","lon.r","depth.r","n.r","e.r","blank","blank","blank","hdg.r","cmg.r","speed.r",
                  "blank","blank","blank","blank","blank","blank","blank","blank","pitch","roll","temp","cond","press","sal","sound.vel","oxy.conc",
                  "oxy.sat","alt","blank","blank","lat.s","lon.s","depth.s","n.s","e.s","blank","blank","blank","hdg.s","cmg.s","speed.s","blank",
                  "blank","blank")
  # remove variables with no data
  DAT <- DAT[ ,names(DAT)!="blank"]
  
  # Process DAT file ####
  # create nav_id and object_id
  DAT$nid <- seq(nav.seed,nav.seed+dim(DAT)[1]-1,1)
  DAT$oid <- DAT$oid + 1
  # add dive name variable to data frame
  DAT$dive.name <- as.factor(rep(dive.name,dim(DAT)[1]))
  # convert date.time to POSIXct
  DAT$date.time <- as.POSIXct(DAT$date.time,format = "%m-%d-%y %H:%M:%OS") 	# 04-17-12 21:45:57.2
  # convert lat/lon from factor to character
  options(digits=9)
  # convert ROV lat/lon to decimal degrees
  DAT$lat.r <- winfrog2dd(DAT$lat.r)
  DAT$lon.r <- winfrog2dd(DAT$lon.r)
  # convert ship lat/lon to decimal degrees
  DAT$lat.s <- winfrog2dd(DAT$lat.s)
  DAT$lon.s <- winfrog2dd(DAT$lon.s)
  # calculate time interval between nav records
  length.DAT <- dim(DAT)[1]
  time.int <- as.numeric(DAT$date.time[2:length.DAT] - DAT$date.time[1:length.DAT-1])
  time.int <- c(time.int,0)
  # calculate ship distances (m)
  DAT$disp.s <- DAT$speed.s * 0.514444444 * time.int
  DAT$disp.s.cum <- cumsum(DAT$disp.s)
  # calculate ROV distances (m)
  DAT$disp.r <- DAT$speed.r * 0.514444444 * time.int
  DAT$disp.r.cum <- cumsum(DAT$disp.r)
  
  # Add DVL pitch/roll to position sensor to get the actual pitch/roll of the camera ####
  # get directory of WinFrog RAW files to read
  raw.files <- list.files(i,pattern = "*.RAW",full.names=TRUE)
  pr.data <- data.frame()
  for (k in raw.files){
    pr.temp <- readLines(k)
    # extract only PASHR sentences
    pr.temp <- pr.temp[grep("413-004-W,DVL [on HDHV]*",pr.temp)]
    # split strings on commas
    pr.temp <- strsplit(pr.temp,",")
    # convert list to data frame
    pr.temp <- do.call(rbind.data.frame, pr.temp)
    # subset only columns with DVL data
    pr.temp <- pr.temp[ ,1:5]
    # add names to data frame
    names(pr.temp) <- c("code","name","time","pitch","roll")
    # convert time to POSIXct
    t.seed <- as.POSIXct(strptime('1970-01-01 00:00:00', '%Y-%m-%d %H:%M:%S'))
    pr.temp$date.time <- t.seed + as.numeric(as.character(pr.temp$time))
    # rbind all dataframes together
    pr.data <- rbind(pr.data,pr.temp)
  }
  # convert pitch and roll to numeric
  pr.data$pitch <- as.numeric(as.character(pr.data$pitch))
  pr.data$roll <- as.numeric(as.character(pr.data$roll))
  
  # match pitch and roll data to nav 
  pid <- numeric()
  pid.lag <- numeric()
  # (if RAW data are logged 'With Events')
  if(dim(pr.data)[1]!= dim(DAT)[1]){ 
    # match P/R and nav datetimes
    for (jj in 1:dim(DAT)[1]){
      # calculate the time difference between jth video obs and each nav record
      time.diff	  <- abs(difftime(DAT$date.time[jj],pr.data$date.time,units = "secs"))
      # get min index for merging later
      pid <- c(pid,which.min(time.diff))
      # get time lag
      pid.lag <- c(pid.lag,as.numeric(time.diff[which.min(time.diff)]))
    }
    # add DVL pitch to the pitch measured by the tilt tray position sensor
    DAT$pitch <- DAT$pitch + round(pr.data$pitch[pid])
  } else { # (if RAW data are logged 'With Events')
    # add DVL pitch to the pitch measured by the tilt tray position sensor
    DAT$pitch <- DAT$pitch + round(pr.data$pitch)
  }

  # Write DVL pitch/roll data to CSV
  write.csv(pr.data,file=file.path(i,paste(dive.name,"PitchRollData.txt", sep = "_")),row.names = FALSE,quote=FALSE,na="-999")
  
  # Process CTD data ####
  if(ctd.on == FALSE){  # If CTD not present, make data -999
    DAT$sal      	  <- rep(-999,length.DAT);
    DAT$conductivity  <- sal;
    DAT$oxygen_conc   <- sal;
    DAT$oxygen_sat    <- sal;
    DAT$depth_p_r     <- DAT$depth.r;
    DAT$pressure      <- sal;
    DAT$sound_vel     <- sal;  
    DAT$depth_msw_r   <- depth_r;
  } else {  		  # Else, process CTD data and calculate oxygen saturation
    # Calculate depth from pressure in SEAWATER (factors-in gravity and latitude)
    DAT$depth.r <- calc_depth(DAT$lat.s,DAT$press)
    # calculate the seawater depth (m) from ROV depth and altitude 
    DAT$depth.msw	<- DAT$depth.r - DAT$alt   
    # calculate oxygen saturation during transect
    DAT$oxy.sat <- calc_sat(DAT$sal,DAT$temp,DAT$oxy.conc) 
  }
  
  # Smooth ROV roll, pitch and altitude for width/area calculations ####
  # replace NAs with raw (unsmoothed) data
  # set negative (bad) altitude data to NA
  DAT$alt[which(DAT$alt<0)] <- NA
  # if altitude data is present (not all values > 0)
  if(length(which(is.na(DAT$alt)==FALSE))>0){
    # use linear interpolation to replace NAs
    DAT$alt <- as.numeric(na.interp(DAT$alt))
    # smooth altitude data
    DAT$alt_sm <- as.numeric(ma(DAT$alt,order=nav.smoother))
    # replace NA data with non-smoothed data
    isna <- which(is.na(DAT$alt_sm)==TRUE)
    DAT$alt_sm[isna] <- DAT$alt[isna]  
  }else{
    DAT$alt_sm <- as.numeric(NA) 
  }
  # smooth pitch data
  # set negative (bad) altitude data to NA
  DAT$pitch[which(DAT$pitch>=0)] <- NA
  # use linear interpolation to replace NAs
  DAT$pitch <- as.numeric(na.interp(DAT$pitch))
  DAT$pitch_sm <- as.numeric(ma(DAT$pitch,order=nav.smoother))
  # replace NAs with non-smoothed data
  isna <- which(is.na(DAT$pitch_sm)==TRUE)
  DAT$pitch_sm[isna] <- DAT$pitch[isna]
  DAT$pitch[which(DAT$pitch>0)] <- NA
  # smooth roll data
  DAT$roll_sm <- as.numeric(ma(DAT$roll,order=nav.smoother))
  # replace NAs with non-smoothed data
  isna <- which(is.na(DAT$roll_sm)==TRUE)
  DAT$roll_sm[isna] <- DAT$roll[isna]
  
  # Calculate width and area ####
  # uses the method described in Stierhoff et al. (in review) and calc_width function in surveyR package
  width.df <- calc_width(DAT$pitch_sm,DAT$alt_sm,ROV=ROV)
  DAT$camera_alt    <- width.df$camera_alt   # camera altitude (m)  
  DAT$slant_range   <- width.df$slant_range  # slant range (m)
  DAT$center_width  <- width.df$center_width # center width (m)
  DAT$area          <- DAT$disp.r * DAT$center_width # area (sq.m)
  DAT$cum_area      <- cumsum(DAT$area)      # cumuilative area (sq.m)
  
  # Select nav data for database export ####
  DAT.output <- DAT[ ,c("nid","oid","dive.name","date.time",
                        "lat.s","lon.s","n.s","e.s","hdg.s","cmg.s","speed.s","disp.s","disp.s.cum","lat.r","lon.r","depth.r",
                        "n.r","e.r","hdg.r","cmg.r","speed.r","pitch","roll","temp","cond","press","sal","sound.vel",
                        "oxy.conc","oxy.sat","alt","disp.r","disp.r.cum",
                        "camera_alt","slant_range","center_width","area","cum_area")]
  names(DAT.output) <- c("nav_id","object_id","dive_name","date_time",
                         "lat_dd_s","lon_dd_s","northing_s","easting_s","heading_s","cmg_s","speed_s","disp_s","cum_disp_s","lat_dd_r","lon_dd_r","depth_r",
                         "northing_r","easting_r","heading_r","cmg_r","speed_r","pitch","roll","temperature","conductivity","pressure","salinity","sound_vel",
                         "oxygen_conc","oxygen_sat","altitude","disp_r","cum_disp_r",
                         "camera_altitude","slant_range","center_width","area_r","cum_area_r")
  
  # write DAT data frame to CSV file for import into the database
  DAT.write <- DAT.output
  DAT.write$date_time <- format(DAT.write$date_time,format="%m/%d/%Y %H:%M:%S")
  write.csv(DAT.write,file = file.path(i,paste(dive.name,"NavData.txt",sep = "_")),quote = FALSE,row.names = FALSE,na = "-999")
  # add results to DAT.temp
  DAT.temp <- rbind(DAT.temp,DAT.write)
  
  # write nav data to .CSV file for 3Beam analysis
  DAT.fov <- DAT.output
  # create zero vectors for speed_fa and speed_ps
  DAT.fov$speed_fa <- rep(0,dim(DAT)[1])
  DAT.fov$speed_ps <- rep(0,dim(DAT)[1])
  DAT.fov$date <- strptime(as.character(DAT.fov$date_time),"%m/%d/%Y")
  DAT.fov$date <- format(DAT.fov$date_time,"%m/%d/%Y")
  DAT.fov$time <- format(DAT.fov$date_time,"%H:%M:%S")
  
  # select data for export
  DAT.fov <- DAT.fov[ ,c("dive_name","date","time","heading_r","pitch","roll","altitude","disp_r",
                         "cum_disp_r","speed_fa","speed_ps","lat_dd_r","lon_dd_r","depth_r")]	
  # rename columns
  names(DAT.fov) <- c("dive_name","date","time","heading","pitch","roll","altitude","disp",
                      "total_disp","speed_fa","speed_ps","lat_dd","lon_dd","depth")
  # write to file
  write.csv(DAT.fov,file = file.path(i,paste(dive.name,"3BeamNav.csv",sep="_")),quote=FALSE, row.names=FALSE)
  
  # Plot CTD data and save ####
  ctd.gg 	<- melt(DAT[ ,c("date.time","depth.r","depth.msw","sal","cond","temp","press",
                          "sound.vel","oxy.conc","oxy.sat")],id=c("date.time"))
  ctd.p 	<- ggplot(ctd.gg,aes(x=date.time,y=value,group=variable)) + 
    geom_line(colour="blue") + scale_x_datetime() + facet_wrap(~variable, scales="free") + 
    labs(title = paste("CTD Data from transect ",dive.name,"\n",sep="")) + xlab("\nTime of Day (GMT)") + ylab("Sensor Value\n") +
    theme_bw() +
    theme(strip.text.x = element_text(size=12, face="bold"),
          strip.background = element_rect(fill="white"),
          panel.margin = unit(1, "lines"))
  ggsave(ctd.p,filename = file.path(i,paste(dive.name,"PhysData.png",sep="_")),height=9.8,width=13.3,units="in")
  
  # Plot results from center width and area calculations ####
  # plot pitch data
  p.gg	<- melt(DAT[ ,c("date.time","pitch","pitch_sm")],id=c("date.time"))
  p.p	<- ggplot(p.gg,aes(x=date.time,y=value,colour=variable)) + 
    geom_line() + theme_bw() + labs(title = paste("ROV data from ",dive.name,"\n",sep="")) +
    xlab("") + ylab("Pitch (degrees)\n") + scale_x_datetime() + scale_colour_manual("Pitch",values=c("black","green"))
  # plot roll data
  r.gg	<- melt(DAT[ ,c("date.time","roll","roll_sm")],id=c("date.time"))
  r.p	<- ggplot(r.gg,aes(x=date.time,y=value,colour=variable)) + 
    geom_line() + theme_bw() + 
    xlab("") + ylab("Roll (degrees)\n") + scale_x_datetime() + scale_colour_manual("Roll",values=c("black","blue"))
  # plot altitude data
  a.gg	<- melt(DAT[ ,c("date.time","alt","alt_sm","camera_alt")],id=c("date.time"))
  if(length(which(is.na(a.gg$value)==FALSE))>0){
  a.p	<- ggplot(a.gg,aes(x=date.time,y=value,colour=variable)) + 
    geom_line() + theme_bw() + 
    xlab("") + ylab("Altitude (meters)\n") + scale_x_datetime() + scale_colour_manual("Altitude",values=c("black","red","blue"))
  }
  # plot of ROV depth and altitude data
  z.gg	<- melt(DAT[ ,c("date.time","depth.r","depth.msw")],id=c("date.time"))
  z.p		<- ggplot(z.gg,aes(x=date.time,y=value,colour=variable)) + 
    geom_line() + theme_bw() + scale_colour_manual("Depth",values=c("green","black")) + 
    xlab("\nTime of Day") + ylab("Depth (m)\n") +	scale_x_datetime()
  
  # save the plot grid
  if(length(which(is.na(a.gg$value)==FALSE))>0){
    nav.grid <- plot_grid(p.p,r.p,a.p,z.p,nrow = 4, align = "v")
    save_plot(file.path(i,paste(dive.name,"NavData.png",sep="_")),nav.grid, ncol = 1,nrow = 4, base_aspect_ratio = 4)
  }else{
    nav.grid <- plot_grid(p.p,r.p,z.p,nrow = 3, align = "v")
    save_plot(file.path(i,paste(dive.name,"NavData.png",sep="_")),nav.grid, ncol = 1,nrow = 3, base_aspect_ratio = 4)
  }
  
  # Plot camera data ####
  # if camera altitude data is not all NA
  if(length(which(is.na(width.df$camera_altitude)==FALSE))>0){
    width.df$date_time <- DAT$date.time
    w.gg <-  melt(width.df,id=c("date_time"))
    w.p <- ggplot(w.gg,aes(x=date_time,y=value,colour=variable)) + 
      geom_line() + theme_bw() + scale_colour_manual("Variable",values=c("green","black","blue")) + 
      xlab("\nDate/time") + ylab("Variable (m)\n") +	scale_x_datetime() +	
      labs(title = paste("Camera data from ",dive.name,"\n",sep=""))
    ggsave(w.p,filename = file.path(i,paste(dive.name,"CameraData.png",sep="_")),height=5,width=10,units="in")
  }
 
  # Process WinFrog events from LOG file ####
  log.info <- file.info(file.path(i,'logs.LOG'))
  
  if (log.info$size == 0){
    # If the LOG file is empty, do nothing
    cat(paste("Log file for",dive.name,"is empty.\n"))
  } else{
    # Else, process the LOG file
    LOG <- read.csv(file.path(i,"logs.LOG"),header=FALSE)
    LOG <- LOG[ ,c(1,3:11,46:53)]
    names(LOG) <- c("comment","date.time","lat.r","lon.r","depth.r","hdg.r","cmg.r","speed.r","n.r","e.r",
                    "lat.s","lon.s","depth.s","hdg.s","cmg.s","speed.s","n.s","e.s")
    
    # create nav_id and object_id
    LOG$eid <- seq(event.seed,event.seed+dim(LOG)[1]-1,1)
    LOG$oid <- seq(1,dim(LOG)[1],1)
    # add dive name variable to data frame
    LOG$dive.name <- as.factor(rep(dive.name,dim(LOG)[1]))
    # # add nav_id and lag_s variable to data frame
    # LOG$nav.id <- as.numeric(rep(NA,dim(LOG)[1]))
    # LOG$lag.s <- LOG$nav.id
    
    # convert date.time to POSIXct
    LOG$date.time <- as.POSIXct(LOG$date.time,format = "%m-%d-%y %H:%M:%OS") 	# 04-17-12 21:45:57.2
    # sync log and nav datetimes
    for (kk in 1:nrow(LOG)){
      # calculate the time difference between jth video obs and each nav record
      time.diff	<- abs(difftime(LOG$date.time[kk],DAT$date.time,units = "secs"))
      LOG$nav.id[kk]	<- DAT$nid[which.min(time.diff)]
      LOG$lag.s[kk] 	<- min(time.diff)
    }
    # convert ROV lat/lon to decimal degrees
    LOG$lat.r <- winfrog2dd(LOG$lat.r)
    LOG$lon.r <- winfrog2dd(LOG$lon.r)
    # convert ship lat/lon to decimal degrees
    LOG$lat.s <- winfrog2dd(LOG$lat.s)
    LOG$lon.s <- winfrog2dd(LOG$lon.s)
    
    # select nav data to output to the database
    LOG.output <- LOG[ ,c("eid","nav.id","lag.s","dive.name","date.time","comment","lat.s","lon.s","n.s","e.s","hdg.s","cmg.s",
                          "speed.s","lat.r","lon.r","depth.r","n.r","e.r","hdg.r","cmg.r","speed.r")]
    names(LOG.output) <- c("event_id","nav_id","lag_s","dive_name","date_time","comment","lat_dd_s","lon_dd_s","northing_s","easting_s","heading_s",
                           "cmg_s","speed_s","lat_dd_r","lon_dd_r","depth_r","northing_r","easting_r","heading_r","cmg_r","speed_r")
    LOG.write <- LOG.output 
    LOG.write$date_time <- format(LOG.output$date_time,format="%m/%d/%Y %H:%M:%S")
    # write dive events to text file
    write.csv(LOG.write,file=file.path(i,paste(dive.name,"DiveEvents.txt", sep = "_")),row.names = FALSE,quote=FALSE,na="-999")
    # add results to LOG.temp
    LOG.temp <- rbind(LOG.temp,LOG.write)
  }
  
  # Process photos to get metadata and sync with nav data ####
  photo.list <- dir(photo.root,recursive=FALSE,pattern='\\d{2}-\\d{3}\\w',full.names=TRUE)
  p <- photo.list[grep(dive.name,photo.list)]
  
  if(length(grep(dive.name,p))>0){ # if photos were taken, then the photo directory exists in photo.root
    if(.Platform$OS.type == "unix") {
      # apply time fix to each photo using ExifTool
      # add or substract time from EXIF tags. e.g., -=Y:M:D h:m:s. Sign dictates plus or minus
      system(paste("exiftool -AllDates",timefix," ",p, sep=""),intern=TRUE) 
      # get photo metadata using ExifTool
      exifData <- system(paste("exiftool -T -filename -createdate ", p, sep=" "),intern=TRUE)
    } else {
      # apply time fix to each photo using ExifTool
      # add or substract time from EXIF tags. e.g., -=Y:M:D h:m:s. Sign dictates plus or minus
      system(paste("C:/exiftool.exe -AllDates",timefix," ",p,sep=""),intern=TRUE)
      # get photo metadata using ExifTool
      exifData <- system(paste("C:/exiftool.exe -T -filename -createdate",p,sep=" "),intern=TRUE)
    }
    # format results
    exifData <- unlist(strsplit(exifData,"\t"))
    exif.filename <- exifData[grep("\\w{4}\\d{4}.\\w{3}",exifData)]
    exif.datetime <- as.POSIXct(exifData[grep("\\d{4}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}",exifData)],format = "%Y:%m:%d %H:%M:%S") # 2009:10:13 01:34:15
    PHOTO <- data.frame(exif.filename,exif.datetime)
    names(PHOTO) <- c("filename","date_time")
    # create other variables
    PHOTO$photo_id 	 <- seq(photo.seed,photo.seed+dim(PHOTO)[1]-1,1)
    PHOTO$dive_name  <- rep(dive.name,dim(PHOTO)[1])
    PHOTO$filepath 	 <- paste("\\\\swc-storage1\\ROV\\PHOTOS",PHOTO$dive_name,PHOTO$filename, sep = "\\")
    PHOTO$nav_id 	 <- rep(NA,dim(PHOTO)[1])
    PHOTO$lag_s	 	 <- rep(NA,dim(PHOTO)[1])
    PHOTO$comment 	 <- rep(NA,dim(PHOTO)[1])
    
    # sync photo and nav datetimes
    for (jj in 1:dim(PHOTO)[1]){
      # calculate the time difference between jth video obs and each nav record
      time.diff	<- abs(difftime(PHOTO$date_time[jj],DAT$date.time,units = "secs"))
      PHOTO$nav_id[jj]	<- DAT$nid[which.min(time.diff)]
      PHOTO$lag_s[jj] 	<- min(time.diff)
    }
    
    # write PHOTO_INFO to a text file
    PHOTO.output <- PHOTO[ ,c("photo_id","nav_id","lag_s","dive_name","date_time","filename","filepath","comment")]
    # create a duplicate df for writing to the database
    PHOTO.write <- PHOTO.output
    # format date/time to a database compatible format
    PHOTO.write$date_time <- format(PHOTO.output$date_time,format="%m/%d/%Y %H:%M:%S")
    write.csv(PHOTO.write,	file = file.path(i,paste(dive.name,"_PhotoInfo.txt",sep = "")),quote = FALSE,row.names = FALSE,na = "-999")
    # add results to PHOTO.temp
    PHOTO.temp <- rbind(PHOTO.temp,PHOTO.write)
  } else {
    cat(paste("No photos to process for",dive.name,".\n"))
  }
  
  # Plot lat/lon data of the ROV, ship, and photos ####
  # subset nav data for ship
  ship <- DAT[ ,c("lat.s","lon.s")] 
  ship$name <- rep("ship",dim(ship)[1])
  names(ship) <- c("lat","lon","vessel")
  # subset nav data for ROV
  rov <-  DAT[ ,c("lat.r","lon.r")]
  rov$name <- rep("ROV",dim(ship)[1])
  names(rov) <- c("lat","lon","vessel")
  # subset and merge photo info with nav data
  # photo <- PHOTO[ ,c("nav_id","lag_s")]
  # photo <- merge(photo,DAT[ ,c("nav_id","lat_dd_r","lon_dd_r")])
  ll.gg	<- rbind(ship,rov)
  ll.p	<- ggplot(ll.gg,aes(x=lon,y=lat,colour = vessel)) +
    geom_path() + coord_map() + theme_bw() + labs(title = paste("Lat/Lon data from ",dive.name,"\n",sep="")) +
    # geom_point(data = PHOTO,aes(),shape=21,fill='white',colour='black')
    xlab("\nLongitude") + ylab("Latitude\n") + scale_colour_manual("Vessel",values=c("green","black"))
  ggsave(ll.p,filename = file.path(i,paste(dive.name,"LatLonData.png",sep="_")))
  
  
  # Process CTD cast data ####
  # list CTD files in directory
  CTD.dir <- list.files(i,pattern='\\d{2}-\\d{3}\\w{1}_(CTD.)(\\(\\d{3}-\\d{6}\\))?.DAT',full.names=TRUE)
  if(length(CTD.dir)==0){
    # If no CTD files are present, do nothing
    cat(paste("No CTD casts to process for ",dive.name,".\n",sep=""))
  } else {
    # else process each CTD file
    for (ii in CTD.dir){
      CTD <- read.csv(ii,header=FALSE)
      CTD <- CTD[ ,c(1:47,65)]
      # add variable names to data frame
      names(CTD) <- c("oid","blank","date.time","lat.r","lon.r","depth.r","n.r","e.r","blank","blank","blank","hdg.r","cmg.r","speed.r",
                      "blank","blank","blank","blank","blank","blank","blank","blank","pitch","roll","temp","cond","press","sal","sound.vel","oxy.conc",
                      "oxy.sat","alt","blank","blank","lat.s","lon.s","depth.s","n.s","e.s","blank","blank","blank","hdg.s","cmg.s","speed.s","blank",
                      "blank","blank")
      # remove variables with no data
      CTD <- CTD[ ,names(CTD)!="blank"]
      # create nav_id and object_id
      CTD$cid <- seq(ctd.seed,ctd.seed+dim(CTD)[1]-1,1)
      CTD$oid <- seq(1,dim(CTD)[1],1)
      # add CTD name variable to data frame
      CTD.name <- str_extract(ii,'\\d{2}-\\d{3}\\w{1}_(CTD.)')
      CTD$ctd.name <- rep(CTD.name,dim(CTD)[1])
      # add dive name variable to data frame
      CTD$dive.name <- as.factor(rep(dive.name,dim(CTD)[1]))
      # convert date.time to POSIXct
      CTD$date.time <- as.POSIXct(CTD$date.time,format = "%m-%d-%y %H:%M:%OS") 	# 04-17-12 21:45:57.2
      # convert CTD lat/lon to decimal degrees
      CTD$lat.r <- winfrog2dd(CTD$lat.r)
      CTD$lon.r <- winfrog2dd(CTD$lon.r)
      # convert ship lat/lon to decimal degrees
      CTD$lat.s <- winfrog2dd(CTD$lat.s)
      CTD$lon.s <- winfrog2dd(CTD$lon.s)
      # calculate time interval between nav records
      length.CTD <- dim(CTD)[1]
      time.int <- CTD$CTDe.time[2:length.CTD] - CTD$CTDe.time[1:length.CTD-1]
      time.int <- c(time.int,0)
      # process CTD data
      if(ctd.on == FALSE){  # If CTD is not installed, make data -999
        CTD$sal      	  <- rep(-999,length.CTD);
        CTD$conductivity  <- sal;
        CTD$oxygen_conc   <- sal;
        CTD$oxygen_sat    <- sal;
        CTD$depth_p_r     <- CTD$depth.r;
        CTD$pressure      <- sal;
        CTD$sound_vel     <- sal;  
        CTD$depth_msw_r   <- depth_r;
      } else {  		  # Else, process CTD data and calculate oxygen saturation
        # Calculate depth from pressure in SEAWATER (factors-in gravity and latitude)
        CTD$depth.r <- calc_depth(CTD$lat.s,CTD$press)
        # calculate the seawater depth (m) from ROV depth and altitude 
        CTD$depth.msw	<- CTD$depth.r - CTD$alt   
        # calculate oxygen saturation during transect
        CTD$oxy.sat <- calc_sat(CTD$sal,CTD$temp,CTD$oxy.conc) 
        # select CTD data to output to the database
        CTD.output <- CTD[ ,c("cid","oid","dive.name","ctd.name","date.time",
                              "lat.r","lon.r","depth.r","temp","cond","press","sal","sound.vel","oxy.conc","oxy.sat")]
        names(CTD.output) <- c("ctd_id","object_id","dive_name","ctd_name","date_time","lat_dd_c","lon_dd_c",
                               "depth_c","temperature_c","conductivity_c","pressure_c","salinity_c","sound_vel_c","oxygen_conc_c","oxygen_sat_c")
        # write DAT data frame to CSV file for import into the database
        CTD.output$date_time <- format(CTD.output$date_time,format="%m/%d/%Y %H:%M:%S")
        # save CTD results to file					
        write.csv(CTD.output,file = file.path(i,paste(CTD.name,".txt",sep="")),quote=FALSE, row.names=FALSE,na = "-999")
        # add CTD results to CTD.temp
        CTD.temp <- rbind(CTD.temp,CTD.output)
        # increment CTD ID by one for next cast
        ctd.seed <- max(CTD.output$ctd_id)+1		
        
        # plot data from each CTD cast
        CTD.gg <- CTD[ ,c("oid","temp","sal","oxy.conc","sound.vel","depth.r")]
        names(CTD.gg) <- c("oid","Temperature (C)","Salinity (PSU)","Oxygen Concentration (uMol)","Sound Velocity (m/s)","Depth")
        CTD.gg <- melt(CTD.gg,id=c("oid","Depth"))
        ctd <- ggplot(CTD.gg,aes(x = value,y = Depth,colour = variable)) +
          geom_path() + facet_wrap(~variable, nrow=1, scales = "free_x") + 
          scale_y_continuous("Depth (m)\n",lim=c(min(CTD.gg$Depth),0),expand=c(0,0)) + xlab("\nSensor Value") + theme_bw() +
          theme(legend.position = "none",
                strip.text.x = element_text(size=12, face="bold"),
                strip.background = element_rect(fill="white"),
                panel.margin = unit(1, "lines")) + 
          ggtitle(paste("CTD profile for",CTD.name,"\n"))
        ggsave(ctd,filename = file.path(i,paste(CTD.name,".png",sep="")),height=9.8,width=13.3,units="in")
      }
    }
  }
  
  # Calculate and report new indices for next transect ####
  
  # Calculate next nav_id
  nav.seed <- max(DAT$nid)+1	
  # Calculate next event_id
  if (log.info$size > 0){
    # if log file is not empty, increment the event seed by 1
    event.seed <- max(LOG$eid)+1
  } else {
    # else keep the same
    event.seed <- event.seed
  }
  # Calculate next photo_id
  if (length(grep(dive.name,p))>0){
    # if the dive name is in the list of photo directories, increment the photo seed by 1
    photo.seed <- max(PHOTO$photo_id)+1
  } else {
    # else keep the same
    photo.seed <-photo.seed
  }
  # Calculate next ctd_id
  if (length(CTD.dir)>0){
    # if CTD casts were performed, increment the CTD seed by 1
    ctd.seed <- max(CTD$cid)+1
  } else {
    # else keep the same
    ctd.seed <- ctd.seed
  }
  # update the progress bar
  info <- sprintf("%d%% done", round((j/length(dir.list))*100))
  setWinProgressBar(pb, round((j/length(dir.list))*100), label=info)
  # increment loop counter
  j <- j + 1
  # insert a new line in the console
  cat("\n")
}
close(pb)

# Write NAV data for all dives to text file
write.csv(DAT.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"NavData.txt", sep = "_")),row.names = FALSE,quote=FALSE,na = "-999")
# Write PHOTO data for all dives to text file
write.csv(PHOTO.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"PhotoInfo.txt", sep = "_")),row.names = FALSE,quote=FALSE,na = "-999")
# Write LOG data for all dives to text file
if(dim(LOG.temp)[1] > 0){
  write.csv(LOG.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"DiveEvents.txt", sep = "_")),row.names = FALSE,quote=FALSE,na="-999")
}
# Write CTD data for all dives to text file
if(dim(CTD.temp)[1] > 0){
  write.csv(CTD.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"CTDCasts.txt", sep = "_")),row.names = FALSE,quote=FALSE,na="-999")
}

# Copy plots to Results directory
file.copy(list.files(pattern="*CameraData.png","C:/NAVDATA",recursive = TRUE,full.names = TRUE),file.path(dat.root,"_Results","CameraData"))
file.copy(list.files(pattern="*PhysData.png","C:/NAVDATA",recursive = TRUE,full.names = TRUE),file.path(dat.root,"_Results","PhysData"))
file.copy(list.files(pattern="*NavData.png","C:/NAVDATA",recursive = TRUE,full.names = TRUE),file.path(dat.root,"_Results","NavData"))
file.copy(list.files(pattern="*CTD.png","C:/NAVDATA",recursive = TRUE,full.names = TRUE),file.path(dat.root,"_Results","CTD_Casts"))
file.copy(list.files(pattern="*LatLonData.png","C:/NAVDATA",recursive = TRUE,full.names = TRUE),file.path(dat.root,"_Results","LatLonData"))

# Report new indices for next dive ####
cat(paste("Finished processing dives",start.dir,"to",end.dir,"\n", sep=" "),
    paste("Next nav_id is: ",nav.seed,"\n"),
    paste("Next event_id is: ",event.seed,"\n"),
    paste("Next photo_id is: ",photo.seed,"\n"),
    paste("Next ctd_id is: ",ctd.seed,"\n"),sep="")

# End of script ####