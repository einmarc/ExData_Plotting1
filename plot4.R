plot4 <- function(sourceFile) {
  
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
  
  # convert into numeric
  dataTable$Global_active_power <- as.numeric(as.character(dataTable$Global_active_power))
  dataTable$Sub_metering_1 <- as.numeric(as.character(dataTable$Sub_metering_1))
  dataTable$Sub_metering_2 <- as.numeric(as.character(dataTable$Sub_metering_2))
  dataTable$Sub_metering_3 <- as.numeric(as.character(dataTable$Sub_metering_3))
  dataTable$Voltage <- as.numeric(as.character(dataTable$Voltage))
  dataTable$Global_reactive_power <- as.numeric(as.character(dataTable$Global_reactive_power))
    
  dataTable$DateTime<-strptime(paste(dataTable$Date,dataTable$Time,sep = " "), 
                               format="%Y-%m-%d %H:%M:%S")
  
  # create the plot
  
  par(mfcol = c(2,2), mar = c(4,4,2,2))
  
  #1
  plot(x = dataTable$DateTime, y = dataTable$Global_active_power, 
       type = "l", 
       xlab = "",
       ylab = "Global Active Power")
  
  #2
  plot(x = dataTable$DateTime, y = dataTable$Sub_metering_1, 
       type = "l", 
       xlab = "",
       ylab = "Energy sub metering")
  
  lines(x = dataTable$DateTime, y = dataTable$Sub_metering_2, 
        col = "red"
  )
  lines(x = dataTable$DateTime, y = dataTable$Sub_metering_3, 
        col = "blue"
  )
  
  legend("topright", c("Sub_metering_1",
                       "Sub_metering_2",
                       "Sub_metering_3"),
         col = c("black","red","blue"),
         lwd = 1,
         bty = "o")
  
  #3
  plot(dataTable$DateTime, dataTable$Voltage,
       type = "l",
       xlab = "datetime",
       ylab = "Voltage")
  
  
  #4
  plot(dataTable$DateTime, dataTable$Global_reactive_power,
       type = "l",
       xlab = "datetime",
       ylab = "Global_reactive_power")
  
  # save the histogram in png format
  dev.copy(png,'plot4.png',480,480)
  dev.off()
  
  
}