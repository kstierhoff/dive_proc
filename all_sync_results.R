# A script to combine results from sync_photos.R

# Directory containing results files
res.dir <- "C:/PHOTOS"
files <- dir(res.dir,pattern = "*PhotoSync.txt",full.names = TRUE)

res.temp <- data.frame()

for(i in files){
  # i=files[1]
  temp <- read.csv(i)
  res.temp <- rbind(res.temp,temp)
}

dives <- levels(res.temp$dive_name)

write.csv(PHOTO.write,file = file.path("C:/PHOTOS",paste(dives[1],dives[length(dives)],"PhotoSyncAll.txt", sep = "_")),row.names = FALSE,quote=FALSE,na = "-999")

