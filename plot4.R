readData <- function()
{
  data <- read.csv(file="household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?")
  data1 <- data[data$Date == "1/2/2007",]
  data2 <- data[data$Date == "2/2/2007",]
  data <- rbind(data1, data2)
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  data$datetime <- chron(as.character(data$Date), as.character(data$Time), format=c("Y-m-d", "h:m:s"))
#  data$day <- as.character()
  data
}

plot4 <- function()
{
  data4 <- readData()
  png("plot4.png", width=480, height=480)
  
  par(mfrow=c(2,2))

  plot(Global_active_power ~ DateTime, data=data2, type='l',
       xlab="",
       ylab="Global Active Power (kilowatts)",
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03"))
  )
  axis.Date(1, at=seq(as.Date("2007-02-01"), as.Date("2007-02-03"), by="1 day"), format="%a")
  
  plot(Voltage ~ datetime, data=data4, type='l',
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03")),
       ylim=c(234, 246),
       col='black'
  )
  
  axis.Date(1, at=seq(as.Date("2007-02-01"), as.Date("2007-02-03"), by="1 day"), format="%a")
  
  plot(Sub_metering_1 ~ DateTime, data=data3, type='l',
       xlab="",
       ylab="Energy sub metering",
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03")),
       ylim=c(0, 40),
       col='black'
  )
  
  par(new=T)
  
  plot(Sub_metering_2 ~ DateTime, data=data3, type='l',
       xlab="",
       ylab="Energy sub metering",
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03")),
       ylim=c(0, 40),
       col='red'
  )
  
  par(new=T)
  
  plot(Sub_metering_3 ~ DateTime, data=data3, type='l',
       xlab="",
       ylab="Energy sub metering",
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03")),
       ylim=c(0, 40),
       col='blue'
  )
  
  axis.Date(1, at=seq(as.Date("2007-02-01"), as.Date("2007-02-03"), by="1 day"), format="%a")
  
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col = c('black', 'red', 'blue'), lwd = 1)
  
  plot(Global_reactive_power ~ datetime, data=data4, type='l',
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03")),
       ylim=c(0, 0.5),
       col='black'
  )
  
  axis.Date(1, at=seq(as.Date("2007-02-01"), as.Date("2007-02-03"), by="1 day"), format="%a")
  
  dev.off()
  
}