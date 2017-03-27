readData <- function()
{
  library(chron)
  data <- read.csv(file="household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?")
  data1 <- data[data$Date == "1/2/2007",]
  data2 <- data[data$Date == "2/2/2007",]
  data <- rbind(data1, data2)
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  data$DateTime <- chron(as.character(data$Date), as.character(data$Time), format=c("Y-m-d", "h:m:s"))
#  data$day <- as.character()
  data
}

plot2 <- function()
{
  data3 <- readData()
  png("plot2.png", width=480, height=480)
  plot(Global_active_power ~ DateTime, data=data3, type='l',
       xlab="",
       ylab="Global Active Power (kilowatts)",
       xaxt='n',
       xlim=c(as.Date("2007-02-01"), as.Date("2007-02-03"))
  )
  axis.Date(1, at=seq(as.Date("2007-02-01"), as.Date("2007-02-03"), by="1 day"), format="%a")
  dev.off()
}