# Code for processing ROV nav, event, CTD, and photo data generated during ROV transects
# Applies to WinFrog v2 .DAT files, .LOG files, CTD files, and photo exif data

# Kevin L. Stierhoff

# Script setup #####
# clear system memory
rm(list = ls())
# Set time zone to GMT
Sys.setenv(TZ = "GMT")

# List packages required to run the script
pkgs <- c("tidyverse","RODBC","reshape2","plyr","forecast",
          "scales","grid","gridExtra","cowplot","exifr")

# Install and load all CRAN packages provided from a character vector
load_pkgs = function(pkgs) {
  new_pkgs = pkgs[!(pkgs %in% installed.packages()[ ,'Package'])]
  if (length(new_pkgs) > 0) install.packages(new_pkgs)
  invisible(lapply(pkgs,function(x)
    suppressPackageStartupMessages(library(x,character.only = T))))
}
# Load packages
load_pkgs(pkgs)

# Install and load surveyR package from github
if ("surveyR" %in% installed.packages()[ ,'Package'] == F) {
  install_github("kstierhoff/surveyR")
}

# Load surveyR
library(surveyR)

# You are about to process DAT, LOG, CTD and PHOTO data for a series of ROV transects
# You will need to supply some information below

# User-defined variables ####
# Enter the directory of the ROV database
# db.dir <- "C:/DATA/ROV_AtSea_20160906.accdb"  # on ROV laptop
db.dir <- "D:/DATA/rov_data/ROV_Master.accdb"  # on KLS desktop

# Enter the start and end dive names (if only processing one dive, make these the same)
start.dir 	<- "16-341A"
end.dir 	  <- "16-342D"
# Is the CTD present (this will almost always be TRUE)
ctd.on <- T
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
nav.seed    <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_NAV.nav_id) AS nav_seed
                                   FROM dbo_tbl_NAV;")[1] + 1)
event.seed  <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_EVENTS.event_id) AS event_seed
                                   FROM dbo_tbl_EVENTS;")[1] + 1)
photo.seed  <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_PHOTO_INFO.photo_id) AS photo_seed
                                   FROM dbo_tbl_PHOTO_INFO;")[1] + 1)
ctd.seed    <- as.numeric(sqlQuery(channel, "SELECT max(dbo_tbl_CTD_CASTS.ctd_id) AS ctd_seed
                                   FROM dbo_tbl_CTD_CASTS;")[1] + 1)
close(channel)

# Create temporary data frames for storing processing results ####
DAT.temp 	<- data.frame()
PHOTO.temp	<- data.frame()
LOG.temp 	<- data.frame()
CTD.temp 	<- data.frame()

# Process NAV date from DAT files ####
if (.Platform$OS.type == "unix") { 
  # if working on a Mac
  dat.root 	<- "/Users/kevinstierhoff/NAVDATA" 	# This is the WinFrog project directory
  photo.root 	<- "/Users/kevinstierhoff/PHOTOS" # This is the photo directory
} else { 
  # if working on a Windows PC
  dat.root 	<- "C:/NAVDATA" # This is the WinFrog project directory
  photo.root 	<- "C:/PHOTOS"#  This is the photo directory
}
# get a list of WinFrog project directories that contain NAV data
d <- sort(dir(dat.root,recursive = F, pattern = "\\d{2}-\\d{3}\\w", full.names = T))
# dir.list <- d[which(d == start.dir):which(d == end.dir)]
dir.list <- d[grep(start.dir,d):grep(end.dir,d)]

# create status bar
pb <- winProgressBar(title = "ROV File Processing Progress", label = "0% done", min = 0, max = 100, initial = 0)
# set initial variable for counter
j <- 1
for (i in dir.list) {
  # Copy this script and the functions source code to the DAT directory
  invisible(file.copy(proc.file, i, overwrite = T))
  # extract dive name from directory path
  dive.name <- 	str_extract(i,"\\d{2}-\\d{3}\\w")
  # list DAT files in directory
  dat.files <- list.files(i, pattern = "\\d{2}-\\d{3}\\w{1}(\\(\\d{3}-\\d{6}\\))?.DAT", full.names = T)
  
  DAT <- data.frame()
  for (ii in dat.files) {
    # read DAT file and append to previous
    dat <- read_csv(ii,col_names = F)
    DAT <- bind_rows(DAT,dat)
  }
  # subset columns
  DAT <- select(DAT ,c(1:47,65))
  # add variable names to data frame
  names(DAT) <- c("oid","blank","date_time","lat_r","long_r","depth","northing_r","easting_r",
                  "blank","blank","blank","heading_r","cmg_r","speed_r","blank","blank","blank","blank",
                  "blank","blank","blank","blank","pitch","roll","temperature","conductivity",
                  "pressure","salinity","sound_vel","oxygen_conc","oxygen_sat","altitude",
                  "blank","blank","lat_s","long_s","blank","northing_s","easting_s","blank",
                  "blank","blank","heading_s","cmg_s","speed_s","blank","blank","blank")
  # remove variables with no data
  DAT <- DAT[ ,names(DAT) != "blank"]
  
  # Process DAT file ####
  # create nav_id and object_id
  options(digits = 9)
  # Create new variables and format data
  DAT <- DAT %>% 
    mutate(nav_id    = seq(nav.seed,nav.seed + dim(DAT)[1] - 1,1),
           object_id = oid + 1,
           dive_name = as.factor(dive.name),
           date_time = as.POSIXct(date_time,format = "%m-%d-%y %H:%M:%OS"),
           lat_r     = winfrog2dd(lat_r),
           long_r    = winfrog2dd(long_r),
           lat_s     = winfrog2dd(lat_s),
           long_s    = winfrog2dd(long_s),
           time.int  = c(as.numeric(date_time[2:nrow(DAT)] - date_time[1:nrow(DAT) - 1]),0),
           disp_s    = speed_s * 0.514444444 * time.int,
           disp_r    = speed_r * 0.514444444 * time.int,
           good      = 1)
  
  # Add DVL pitch/roll to position sensor to get the actual pitch/roll of the camera ####
  # get directory of WinFrog RAW files to read
  raw.files <- list.files(i, pattern = "*.RAW", full.names = T)
  pr.data <- data.frame()
  for (k in raw.files) {
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
    pr.temp$date_time <- t.seed + as.numeric(as.character(pr.temp$time))
    # rbind all dataframes together
    pr.data <- rbind(pr.data,pr.temp)
  }
  # convert pitch and roll to numeric
  pr.data <- pr.data %>% 
    mutate(pitch = as.numeric(as.character(pitch)),
           roll = as.numeric(as.character(roll)))
  
  # match pitch and roll data to nav 
  pid <- numeric()
  pid.lag <- numeric()
  # (if RAW data are logged 'With Events')
  if (nrow(pr.data) != nrow(DAT)) { 
    # match P/R and nav datetimes
    for (jj in 1:nrow(DAT)) {
      # calculate the time difference between jth video obs and each nav record
      time.diff	  <- abs(difftime(DAT$date_time[jj], pr.data$date_time,units = "secs"))
      # get min index for merging later
      pid <- c(pid, which.min(time.diff))
      # get time lag
      pid.lag <- c(pid.lag, as.numeric(time.diff[which.min(time.diff)]))
    }
    # add DVL pitch to the pitch measured by the tilt tray position sensor
    DAT$pitch <- DAT$pitch + round(pr.data$pitch[pid])
  } else {# (if RAW data are logged 'With Events')
    # add DVL pitch to the pitch measured by the tilt tray position sensor
    DAT$pitch <- DAT$pitch + round(pr.data$pitch)
  }
  
  # Write DVL pitch/roll data to CSV
  write.csv(pr.data,file = file.path(i, paste(dive.name,"PitchRollData.txt", sep = "_")),
            row.names = F,quote = F,na = "-999")
  
  # Process CTD data ####
  if (ctd.on == FALSE) {  # If CTD not present, make data -999
    DAT <- DAT %>% 
      mutate(
        sal           = -999,
        conductivity  = -999,
        oxygen_conc   = -999,
        oxygen_sat    = -999,
        depth_p_r     = depth,
        pressure      = -999,
        sound_vel     = -999,  
        depth_msw_r   = depth)
  } else {# Else, process CTD data and calculate oxygen saturation
    # Calculate depth from pressure in SEAWATER (factors-in gravity and latitude)
    DAT <- DAT %>% 
      mutate(
        depth         = calc_depth(lat_s, pressure),
        depth_msw	    = depth - altitude,
        oxygen_sat    = calc_sat(salinity, temperature, oxygen_conc))
  }
  
  # Smooth ROV roll, pitch and altitude for width/area calculations ####
  # replace NAs with raw (unsmoothed) data
  # set negative (bad) altitude data to NA
  DAT$altitude[which(DAT$altitude < 0)] <- NA
  # if altitude data is present (not all values > 0)
  if (length(which(is.na(DAT$altitude) == F)) > 0) {
    # use linear interpolation to replace NAs
    DAT$altitude <- as.numeric(na.interp(DAT$altitude))
    # smooth altitude data
    DAT$altitude_sm <- as.numeric(ma(DAT$altitude,order = nav.smoother))
    # replace NA data with non-smoothed data
    isna <- which(is.na(DAT$altitude_sm) == TRUE)
    DAT$altitude_sm[isna] <- DAT$altitude[isna] 
  } else {
    DAT$altitude_sm <- as.numeric(NA) 
  }
  # smooth pitch data
  # set negative (bad) altitude data to NA
  DAT$pitch[which(DAT$pitch >= 0)] <- NA
  # use linear interpolation to replace NAs
  DAT$pitch <- as.numeric(na.interp(DAT$pitch))
  DAT$pitch_sm <- as.numeric(ma(DAT$pitch,order = nav.smoother))
  # replace NAs with non-smoothed data
  isna <- which(is.na(DAT$pitch_sm) == T)
  DAT$pitch_sm[isna] <- DAT$pitch[isna]
  DAT$pitch[which(DAT$pitch > 0)] <- NA
  # smooth roll data
  DAT$roll_sm <- as.numeric(ma(DAT$roll,order = nav.smoother))
  # replace NAs with non-smoothed data
  isna <- which(is.na(DAT$roll_sm) == T)
  DAT$roll_sm[isna] <- DAT$roll[isna]
  
  # Calculate width and area ####
  # uses the method described in Stierhoff et al. (in review) and calc_width function in surveyR package
  width.df <- calc_width(DAT$pitch_sm, DAT$altitude_sm, ROV = ROV)
  
  DAT <- DAT %>% 
    mutate(
      camera_altitude = width.df$camera_alt,
      slant_range     = width.df$slant_range,
      center_width    = width.df$center_width)
  
  # Select nav data for database export ####
  DAT.output <- DAT %>% 
    select(nav_id, object_id, dive_name, date_time, 
           lat_s, long_s, northing_s,easting_s,
           heading_s, cmg_s, speed_s, disp_s,
           lat_r, long_r, depth, northing_r, easting_r,
           heading_r,cmg_r,speed_r, pitch, roll,
           temperature, conductivity,pressure, salinity, 
           sound_vel, oxygen_conc, oxygen_sat,
           altitude, disp_r, good)
  
  # write DAT data frame to CSV file for import into the database
  DAT.write <- DAT.output
  DAT.write$date_time <- format(DAT.write$date_time, format = "%m/%d/%Y %H:%M:%S")
  write.csv(DAT.write, file = file.path(i,paste(dive.name,"NavData.txt",sep = "_")), 
            quote = F,row.names = F, na = "-999")
  # add results to DAT.temp
  DAT.temp <- rbind(DAT.temp,DAT.write)
  
  # Plot CTD data and save ####
  ctd.gg <- DAT %>% 
    select(date_time, depth, depth_msw, salinity, conductivity, temperature, pressure,
           sound_vel, oxygen_conc, oxygen_sat) %>% 
    gather(variable, value, -date_time) 
  
  ctd.p 	<- ggplot(ctd.gg,aes(x = date_time, y = value, group = variable)) + 
    geom_line(colour = "blue") + scale_x_datetime() + 
    facet_wrap(~variable, scales = "free_y") + 
    labs(title = paste("CTD Data from transect ",dive.name,"\n",sep = "")) + 
    xlab("\nTime of Day (GMT)") + ylab("Sensor Value\n") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"),
          strip.background = element_rect(fill = "white"),
          panel.spacing = unit(1, "lines"))
  ggsave(ctd.p, filename = file.path(i, paste(dive.name,"PhysData.png",sep = "_")), 
         height = 9.8, width = 13.3, units = "in")
  
  # Plot results from center width and area calculations ####
  # plot pitch data
  p.gg <- DAT %>% 
    select(date_time, pitch, pitch_sm) %>% 
    gather(variable, value, -date_time)
  
  p.p	<- ggplot(p.gg,aes(x = date_time, y = value, colour = variable)) + 
    geom_line() + theme_bw() + labs(title = paste("ROV data from ",dive.name,"\n", sep = "")) +
    xlab("") + ylab("Pitch (degrees)\n") + scale_x_datetime() + scale_colour_manual("Pitch", values = c("black","green"))
  
  # plot roll data
  r.gg <- DAT %>% 
    select(date_time, roll, roll_sm) %>% 
    gather(variable, value, -date_time)
  
  r.p	<- ggplot(r.gg, aes(x = date_time, y = value, colour = variable)) + 
    geom_line() + theme_bw() + 
    xlab("") + ylab("Roll (degrees)\n") + scale_x_datetime() + scale_colour_manual("Roll",values = c("black","blue"))
  
  # plot altitude data
  a.gg <- DAT %>% 
    select(date_time, altitude, altitude_sm) %>% 
    gather(variable, value, -date_time)
  
  if (length(which(is.na(a.gg$value) == F)) > 0) {
    a.p	<- ggplot(a.gg, aes(x = date_time, y = value, colour = variable)) + 
      geom_line() + theme_bw() + 
      xlab("") + ylab("Altitude (meters)\n") + scale_x_datetime() + 
      scale_colour_manual("Altitude", values = c("black","red","blue"))
  }
  
  # plot of ROV depth and altitude data
  z.gg <- DAT %>% 
    select(date_time, depth, depth_msw) %>% 
    gather(variable, value, -date_time)
  
  z.p		<- ggplot(z.gg, aes(x = date_time, y = value, colour = variable)) + 
    geom_line() + theme_bw() + 
    scale_colour_manual("Depth", values = c("green","black")) + 
    xlab("\nTime of Day") + ylab("Depth (m)\n") +	scale_x_datetime()
  
  # save the plot grid
  if (length(which(is.na(a.gg$value) == F)) > 0) {
    nav.grid <- plot_grid(p.p, r.p, a.p, z.p, nrow = 4, align = "v")
    save_plot(file.path(i,paste(dive.name,"NavData.png",sep = "_")), nav.grid, ncol = 1,nrow = 4, base_aspect_ratio = 4)
  } else {
    nav.grid <- plot_grid(p.p,r.p,z.p,nrow = 3, align = "v")
    save_plot(file.path(i,paste(dive.name,"NavData.png",sep = "_")),nav.grid, ncol = 1,nrow = 3, base_aspect_ratio = 4)
  }
  
  # Plot camera data ####
  # if camera altitude data is not all NA
  if (length(which(is.na(width.df$camera_altitude) == F)) > 0) {
    width.df$date_time <- DAT$date_time
    
    w.gg <-  gather(width.df,variable, value, -date_time)
    
    w.p <- ggplot(w.gg, aes(x = date_time, y = value, colour = variable)) + 
      geom_line() + theme_bw() + scale_colour_manual("Variable", values = c("green","black","blue")) + 
      xlab("\nDate/time") + ylab("Variable (m)\n") +	scale_x_datetime() +	
      labs(title = paste("Camera data from ",dive.name,"\n",sep = ""))
    
    ggsave(w.p,filename = file.path(i,paste(dive.name,"CameraData.png", sep = "_")),
           height = 5, width = 10, units = "in")
  }
  
  # Process WinFrog events from LOG file ####
  log.info <- file.info(file.path(i,'logs.LOG'))
  
  if (log.info$size == 0) {
    # If the LOG file is empty, do nothing
    cat(paste("Log file for",dive.name,"is empty.\n"))
  } else {
    # Else, process the LOG file
    LOG <- read.csv(file.path(i, "logs.LOG"), header = F)
    LOG <- LOG[ ,c(1,3:11,46:53)]
    names(LOG) <- c("comment","date_time","lat_r","long_r","depth","heading_r","cmg_r","speed_r","northing_r","easting_r",
                    "lat_s","long_s","depth_s","heading_s","cmg_s","speed_s","northing_s","easting_s")
    
    # create nav_id and object_id
    LOG <- LOG %>% 
      mutate(event_id  = seq(event.seed,event.seed + nrow(LOG) - 1, 1),
             object_id = seq(1, nrow(LOG),1),
             dive_name = dive.name,
             date_time = as.POSIXct(date_time,format = "%m-%d-%y %H:%M:%OS"), # 04-17-12 21:45:57.2
             lat_r     = winfrog2dd(lat_r),
             long_r    = winfrog2dd(long_r),
             lat_s     = winfrog2dd(lat_s),
             long_s    = winfrog2dd(long_s))
    
    # sync log and nav datetimes
    for (kk in 1:nrow(LOG)) {
      # calculate the time difference between jth video obs and each nav record
      time.diff	      <- abs(difftime(LOG$date_time[kk],DAT$date_time,units = "secs"))
      LOG$nav_id[kk]	<- DAT$nav_id[which.min(time.diff)]
      LOG$lag_s[kk] 	<- min(time.diff)
      
      # select nav data to output to the database
      LOG.output <- LOG %>% 
        select(event_id, nav_id, lag_s, dive_name, date_time, comment, lat_s, long_s, 
               northing_s, easting_s, heading_s, cmg_s, speed_s,
               lat_r, long_r, depth, northing_r, easting_r, heading_r, cmg_r, speed_r)
      
      LOG.write <- LOG.output 
      LOG.write$date_time <- format(LOG.output$date_time,format = "%m/%d/%Y %H:%M:%S")
      # write dive events to text file
      write.csv(LOG.write,file = file.path(i, paste(dive.name,"DiveEvents.txt", sep = "_")), 
                row.names = F, quote = F,na = "-999")
      # add results to LOG.temp
      LOG.temp <- rbind(LOG.temp,LOG.write)
    }
  }
  
  # Process photos to get metadata and sync with nav data ####
  photo.list <- dir(photo.root, recursive = F, pattern = "\\d{2}-\\d{3}\\w", full.names = T)
  p <- photo.list[grep(dive.name,photo.list)]
  
  if (length(grep(dive.name,p)) > 0) { # if photos were taken, then the photo directory exists in photo.root
    if (.Platform$OS.type == "unix") {
      # apply time fix to each photo using ExifTool
      # add or substract time from EXIF tags. e.g., -=Y:M:D h:m:s. Sign dictates plus or minus
      system(paste("exiftool -AllDates", timefix, " ", p, sep = ""), intern = T) 
    } else {
      # apply time fix to each photo using ExifTool
      # add or substract time from EXIF tags. e.g., -=Y:M:D h:m:s. Sign dictates plus or minus
      system(paste("C:/exiftool.exe -AllDates", timefix, " ", p, sep = ""), intern = T)
    }
    
    PHOTO <- read_exif(p, tags = c("filename","createdate"), recursive = T) %>% 
      dplyr::rename(filename = FileName) %>% 
      mutate(
        date_time = lubridate::ymd_hms(CreateDate),
        photo_id  = seq(photo.seed,photo.seed + length(date_time) - 1, 1),
        dive_name = dive.name,
        filepath  = paste("\\\\swc-storage1\\ROV\\PHOTOS", dive_name, filename, sep = "\\"),
        nav_id    = NA,
        lag_s     = NA,
        comment   = NA)
    
    # sync photo and nav datetimes
    for (jj in 1:nrow(PHOTO)) {
      # calculate the time difference between jth video obs and each nav record
      time.diff	<- abs(difftime(PHOTO$date_time[jj],DAT$date_time,units = "secs"))
      PHOTO$nav_id[jj]	<- DAT$nav_id[which.min(time.diff)]
      PHOTO$lag_s[jj] 	<- min(time.diff)
    }
    
    # write PHOTO_INFO to a text file
    PHOTO.output <- select(PHOTO, photo_id, nav_id, lag_s, dive_name, date_time, filename, filepath, comment)
    # create a duplicate df for writing to the database
    PHOTO.write <- PHOTO.output %>% 
      mutate(date_time = format(date_time, format = "%m/%d/%Y %H:%M:%S"))
    # format date/time to a database compatible format
    PHOTO.write$date_time <- format(PHOTO.output$date_time,format = "%m/%d/%Y %H:%M:%S")
    
    write.csv(PHOTO.write, file = file.path(i, paste(dive.name, "_PhotoInfo.txt", sep = "")),
              quote = F, row.names = F, na = "-999")
    # add results to PHOTO.temp
    PHOTO.temp <- rbind(PHOTO.temp,PHOTO.write)
  } else {
    cat(paste("No photos to process for", dive.name, ".\n"))
  }
  
  # Plot lat/lon data of the ROV, ship, and photos ####
  # subset nav data for ship
  ship <- select(DAT, lat_s, long_s) %>% 
    mutate(vessel = "ship") %>% 
    dplyr::rename(lat = lat_s,
                  long = long_s)
  
  # subset nav data for ROV
  rov <- select(DAT, lat_r, long_r) %>% 
    mutate(vessel = "ROV") %>% 
    dplyr::rename(lat = lat_r,
                  long = long_r)
  
  ll.gg	<- rbind(ship,rov)
  ll.p	<- ggplot(ll.gg, aes(x = long, y = lat, colour = vessel)) +
    geom_path() + coord_map() + theme_bw() + labs(title = paste("Lat/Long data from ",dive.name,"\n", sep = "")) +
    xlab("\nLongitude") + ylab("Latitude\n") + scale_colour_manual("Vessel", values = c("green","black"))
  
  ggsave(ll.p,filename = file.path(i, paste(dive.name,"LatLongData.png", sep = "_")))
  
  
  # Process CTD cast data ####
  # list CTD files in directory
  CTD.dir <- list.files(i, pattern = "\\d{2}-\\d{3}\\w{1}_(CTD.)(\\(\\d{3}-\\d{6}\\))?.DAT", full.names = TRUE)
  
  if (length(CTD.dir) == 0) {
    # If no CTD files are present, do nothing
    cat(paste("No CTD casts to process for ", dive.name, ".\n", sep = ""))
  } else {
    # else process each CTD file
    for (ii in CTD.dir) {
      # Get CTD name
      CTD.name <- str_extract(ii,'\\d{2}-\\d{3}\\w{1}_(CTD.)')
      CTD <- read.csv(ii, header = FALSE)
      CTD <- CTD[ ,c(1:47,65)]
      # add variable names to data frame
      names(CTD) <- c("object_id","blank","date_time","lat_c","long_c","depth","northing_c","easting_c","blank","blank","blank",
                      "heading_r","cmg_r","speed_r","blank","blank","blank","blank","blank","blank","blank","blank",
                      "pitch","roll","temperature","conductivity","pressure","salinity","sound_vel","oxygen_conc",
                      "oxygen_sat","altitude","blank","blank","lat_s","long_s","blank","northing_s","easting_s",
                      "blank","blank","blank","heading_s","cmg_s","speed_s","blank","blank","blank")
      
      # remove variables with no data
      CTD <- CTD[ ,names(CTD) != "blank"] %>% 
        mutate(
          ctd_id    = seq(ctd.seed,ctd.seed + length(date_time) - 1, 1), 
          object_id = seq(1, length(date_time), 1),
          ctd_name  = str_extract(ii, "\\d{2}-\\d{3}\\w{1}_(CTD.)"),
          dive_name = dive.name,
          date_time = as.POSIXct(date_time, format = "%m-%d-%y %H:%M:%OS"), # 04-17-12 21:45:57.2
          lat_c     = winfrog2dd(lat_c),
          long_c    = winfrog2dd(long_c),
          lat_s     = winfrog2dd(lat_s),
          long_s    = winfrog2dd(long_s),
          time.int  = c(date_time[2:length(date_time)] - date_time[1:length(date_time) - 1], 0))
      
      # process CTD data
      if (ctd.on == FALSE) {  # If CTD not present, make data -999
        CTD <- CTD %>% 
          mutate(
            sal           = -999,
            conductivity  = -999,
            oxygen_conc   = -999,
            oxygen_sat    = -999,
            depth_p_c     = depth,
            pressure      = -999,
            sound_vel     = -999,  
            depth_msw_c   = depth)
      } else {# Else, process CTD data and calculate oxygen saturation
        # Calculate depth from pressure in SEAWATER (factors-in gravity and latitude)
        CTD <- CTD %>% 
          mutate(
            depth         = calc_depth(lat_s, pressure),
            depth_msw	    = depth - altitude,
            oxygen_sat    = calc_sat(salinity, temperature, oxygen_conc))
      }
      
      # select CTD data to output to the database
      CTD.output <- CTD %>% 
        select(ctd_id, object_id, dive_name, ctd_name, date_time, lat_c, long_c, 
               depth, temperature, conductivity, pressure, salinity, sound_vel,
               oxygen_conc, oxygen_sat) %>% 
        mutate(date_time = format(date_time, format = "%m/%d/%Y %H:%M:%S"))
      
      # save CTD results to file					
      write.csv(CTD.output, file = file.path(i, paste(CTD.name,".txt",sep = "")),
                quote = F, row.names = F, na = "-999")
      
      # add CTD results to CTD.temp
      CTD.temp <- rbind(CTD.temp, CTD.output)
      
      # increment CTD ID by one for next cast
      ctd.seed <- max(CTD.output$ctd_id) + 1		
      
      # plot data from each CTD cast
      CTD.gg <- select(CTD, object_id, temperature, salinity, oxygen_conc, sound_vel, depth) %>% 
        dplyr::rename(
          "Temperature (C)" = temperature,
          "Salinity (PSU)" = salinity,
          "Oxygen Concentration (uMol)" = oxygen_conc,
          "Sound Velocity (m/s)" = sound_vel,
          Depth = depth) %>% 
        gather(variable, value, -object_id, -Depth)
      
      ctd.p <- ggplot(CTD.gg,aes(x = value,y = Depth,colour = variable)) +
        geom_path() + facet_wrap(~variable, nrow = 1, scales = "free_x") + 
        scale_y_continuous("Depth (m)\n", lim = c(min(CTD.gg$Depth),0),expand = c(0,0)) + 
        xlab("\nSensor Value") + 
        theme_bw() +
        theme(legend.position = "none",
              strip.text.x = element_text(size = 12, face = "bold"),
              strip.background = element_rect(fill = "white"),
              panel.spacing = unit(1, "lines")) + 
        ggtitle(paste("CTD profile for",CTD.name,"\n"))
      
      ggsave(ctd.p,filename = file.path(i,paste(CTD.name,".png", sep = "")),
             height = 9.8, width = 13.3, units = "in")
    }
  }
  
  # Calculate and report new indices for next transect ####
  # Calculate next nav_id
  nav.seed <- max(DAT$nav_id) + 1	
  # Calculate next event_id
  if (log.info$size > 0) {
    # if log file is not empty, increment the event seed by 1
    event.seed <- max(LOG$event_id) + 1
  } else {
    # else keep the same
    event.seed <- event.seed
  }
  # Calculate next photo_id
  if (length(grep(dive.name, p)) > 0) {
    # if the dive name is in the list of photo directories, increment the photo seed by 1
    photo.seed <- max(PHOTO$photo_id) + 1
  } else {
    # else keep the same
    photo.seed <- photo.seed
  }
  # Calculate next ctd_id
  if (length(CTD.dir) > 0) {
    # if CTD casts were performed, increment the CTD seed by 1
    ctd.seed <- max(CTD$ctd_id) + 1
  } else {
    # else keep the same
    ctd.seed <- ctd.seed
  }
  # update the progress bar
  info <- sprintf("%d%% done", round((j/length(dir.list))*100))
  setWinProgressBar(pb, round((j/length(dir.list))*100), label = info)
  # increment loop counter
  j <- j + 1
  # insert a new line in the console
  cat("\n")
}
close(pb)

# Write NAV data for all dives to text file
write.csv(DAT.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"NavData.txt", sep = "_")),
          row.names = F, quote = FALSE, na = "-999")
# Write PHOTO data for all dives to text file
write.csv(PHOTO.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"PhotoInfo.txt", sep = "_")),
          row.names = F,quote = F, na = "-999")
# Write LOG data for all dives to text file
if (dim(LOG.temp)[1] > 0) {
  write.csv(LOG.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"DiveEvents.txt", sep = "_")),
            row.names = F, quote  = F, na = "-999")
}
# Write CTD data for all dives to text file
if (dim(CTD.temp)[1] > 0) {
  write.csv(CTD.temp,file = file.path(dat.root,"_Results",paste(start.dir,end.dir,"CTDCasts.txt", sep = "_")),
            row.names = F,quote = F,na = "-999")
}

# Copy plots to Results directory
file.copy(list.files(pattern = "*CameraData.png","C:/NAVDATA", recursive = T, full.names = TRUE), 
          file.path(dat.root, "_Results", "CameraData"))
file.copy(list.files(pattern = "*PhysData.png","C:/NAVDATA", recursive = TRUE, full.names = TRUE), 
          file.path(dat.root, "_Results", "PhysData"))
file.copy(list.files(pattern = "*NavData.png","C:/NAVDATA", recursive = TRUE, full.names = TRUE), 
          file.path(dat.root, "_Results", "NavData"))
file.copy(list.files(pattern = "*CTD.png","C:/NAVDATA", recursive = TRUE, full.names = TRUE), 
          file.path(dat.root, "_Results", "CTD_Casts"))
file.copy(list.files(pattern = "*LatLonData.png","C:/NAVDATA", recursive = TRUE, full.names = TRUE), 
          file.path(dat.root, "_Results", "LatLonData"))

# Report new indices for next dive ####
cat(paste("Finished processing dives", start.dir, "to", end.dir, "\n", sep = " "),
    paste("Next nav_id is: ", nav.seed, "\n"),
    paste("Next event_id is: ", event.seed, "\n"),
    paste("Next photo_id is: ", photo.seed, "\n"),
    paste("Next ctd_id is: ", ctd.seed,"\n"), sep = "")

# End of script ####