readData <- function()
{
  data <- read.csv(file="household_power_consumption.txt", header = TRUE, sep=";", na.strings = "?")
  data1 <- data[data$Date == "1/2/2007",]
  data2 <- data[data$Date == "2/2/2007",]
  data <- rbind(data1, data2)
  data$Date <- as.Date(data$Date, format="%d/%m/%Y")
  data$DateTime <- chron(as.character(data$Date), as.character(data$Time), format=c("Y-m-d", "h:m:s"))
  data
}

plot1 <- function()
{
  data <- readData()
  png("plot1.png", width=480, height=480)
  hist(data$Global_active_power, col="red", 
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)",
       ylab="Frequency",
       xlim=range(0, 7.5),
       xaxt='n',
       ylim=range(0,1200),
       )
  axis(side=1, at=seq(0, 6, 2), labels=seq(0, 6, 2))
  dev.off()
}