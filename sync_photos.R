# Code for syncing ROV photos and nav data based on GMT time

# Kevin L. Stierhoff

# Script setup #####
# clear system memory
rm(list=ls())
# Set time zone to GMT
Sys.setenv(TZ="GMT")
# load libraries
library(RODBC);library(stringr)

# You are about to sync photos to nav data based on time
# Photo time may be adjusted using exifTool
# You will need to supply some information below

# User-defined variables ####
db.dir <- "D:/DATA/rov_data/ROV_Master.accdb"  # on KLS desktop

# Query starting IDs for all database tables ####
channel <- odbcConnectAccess2007(db.dir)
NAV   <- sqlQuery(channel,"SELECT dbo_tbl_NAV_DATA.nav_id, dbo_tbl_NAV_DATA.object_id, dbo_tbl_NAV_DATA.dive_name, dbo_tbl_NAV_DATA.date_time
                  FROM dbo_tbl_NAV_DATA;")

PHOTO <- sqlQuery(channel, "SELECT dbo_tbl_PHOTO_INFO.photo_id, dbo_tbl_PHOTO_INFO.dive_name, dbo_tbl_PHOTO_INFO.date_time
                  FROM dbo_tbl_PHOTO_INFO;")
close(channel)

# Enter the start and end dive names (if only processing one dive, make these the same)
start.dir 	<- "05-104B"
end.dir 	  <- "05-104B"

# Which ROV was used (e.g., HDHV or Phantom)
ROV <- "HDHV"
# If the camera time was not set correctly, apply the following time fix
# If the time was set correctly, all numerics in the string below should be zeros
timefix <- '-="0:0:0 0:0:0"' #e.g., -=Y:M:D h:m:s (- or + to subtract or add date/time)

# Create temporary data frames for storing processing results ####
NAV.temp 	<- data.frame()
PHOTO.temp	<- data.frame()

# Process NAV date from DAT files ####
if(.Platform$OS.type == "unix") { 
  # if working on a Mac
  photo.root 	<- "/Users/kevinstierhoff/PHOTOS" # This is the photo directory
} else { 
  # if working on a Windows PC
  photo.root 	<- "D:/PHOTOS/ROV_PHOTOS"#  This is the photo directory
}
# get a list of PHOTO directories
d <- sort(dir(photo.root,recursive=FALSE,pattern='\\d{2}-\\d{3}\\w',full.names=TRUE))
# select directories that match the start and end directories
dir.list <- d[grep(start.dir,d):grep(end.dir,d)]

for (i in dir.list){
  # i = dir.list[4]
  # extract dive name from directory path
  dive.name <- 	str_extract(i,'\\d{2}-\\d{3}\\w')
  # subset NAV and PHOTO_INFO for each dive
  NAV.sub <- droplevels(subset(NAV,NAV$dive_name %in% dive.name))
  PHOTO.sub <- droplevels(subset(PHOTO,PHOTO$dive_name %in% dive.name))

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
    PHOTO.data <- data.frame(PHOTO.sub$photo_id,PHOTO.sub$dive_name,exif.filename,exif.datetime)
    names(PHOTO.data) <- c("photo_id","dive_name","filename","date_time")
    # create other variables
    PHOTO.data$nav_id 	 <- rep(NA,dim(PHOTO.data)[1])
    PHOTO.data$lag_s	 	 <- rep(NA,dim(PHOTO.data)[1])
    
    # sync photo and nav datetimes
    for (jj in 1:dim(PHOTO.data)[1]){
      # calculate the time difference between jth video obs and each nav record
      time.diff	<- abs(difftime(PHOTO.data$date_time[jj],NAV.sub$date_time,units = "secs"))
      PHOTO.data$nav_id[jj]	<- NAV.sub$nav_id[which.min(time.diff)]
      PHOTO.data$lag_s[jj] 	<- min(time.diff)
    }
    # add results to PHOTO.temp
    PHOTO.temp <- rbind(PHOTO.temp,PHOTO.data)
  } else {
    cat(paste("No photos to process for",dive.name,".\n"))
  }
}

# create a duplicate df for writing to the database
PHOTO.write <- PHOTO.temp
# format date/time to a database compatible format
PHOTO.write$date_time <- format(PHOTO.write$date_time,format="%m/%d/%Y %H:%M:%S")
# Write PHOTO data for all dives to text file
write.csv(PHOTO.write,file = file.path("C:/PHOTOS",paste(start.dir,end.dir,"PhotoSync.txt", sep = "_")),row.names = FALSE,quote=FALSE,na = "-999")
