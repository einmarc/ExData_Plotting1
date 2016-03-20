plot2 <- function(sourceFile) {
  
  # sourceFile contains the directory to the input file
  dataTable <- read.table(file = sourceFile,header = TRUE, sep =";")
  
  # convert column date in the standard format
  dataTable$Date <- as.Date(dataTable$Date, "%d/%m/%Y")
  
  # extract the useful dataset
  dataTable <- dataTable[dataTable$Date > as.Date("2007-01-31"),]
  dataTable <- dataTable[dataTable$Date < as.Date("2007-02-03"),]
  
  # convert time into expected format
  dataTable$Time <- strptime(dataTable$Time,"%H:%M:%S")
  dataTable$Time <- format(dataTable$Time, "%H:%M:%S")
  
  # convert Global_active_power into numeric
  dataTable$Global_active_power <- as.numeric(as.character(dataTable$Global_active_power))
  
  dataTable$DateTime<-strptime(paste(dataTable$Date,dataTable$Time,sep = " "), 
                               format="%Y-%m-%d %H:%M:%S")
  
  # create the plot
  plot(x = dataTable$DateTime, y = dataTable$Global_active_power, 
       type = "l", 
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
  
  
  # save the histogram in png format
  dev.copy(png,'plot2.png',480,480)
  dev.off()
  
  
}