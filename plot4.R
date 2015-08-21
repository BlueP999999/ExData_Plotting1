Plot4<-function(){
    FILE<-"./household_power_consumption.txt"
    ### Reading the dates
    DATES<-read.table(FILE,header=TRUE,sep=";")[,1]
    
    ### Figuring out the location of the two dates
    INDICATOR<-which(DATES=="2/2/2007"|DATES=="1/2/2007")
    MIN<-min(INDICATOR)
    MAX<-max(INDICATOR)
    
    ### Reading the correct lines
    SUBSET<-read.table(FILE,header=FALSE,sep=";",skip=MIN,nrow=MAX-MIN+1)
    
    ### Assign column names
    SAMPLE<-read.table(FILE,header=TRUE,sep=";",nrow=1)
    names(SUBSET)<-names(SAMPLE)
    
    ### Combine date and time
    SUBSET$date.and.time<-strptime(
        paste(SUBSET$Date,SUBSET$Time),format="%d/%m/%Y %H:%M:%S")
    
    par(mfrow = c(2, 2))
    LOWER<-min(SUBSET$Global_active_power)
    UPPER<-max(SUBSET$Global_active_power)
    plot(SUBSET$date.and.time,SUBSET$Global_active_power,type="l",ylim=c(LOWER,UPPER),xlab="",ylab="Global Active Power")

    LOWER<-min(SUBSET$Voltage)
    UPPER<-max(SUBSET$Voltage)
    plot(SUBSET$date.and.time,SUBSET$Voltage,type="l",ylim=c(LOWER,UPPER),xlab="datetime",ylab="Volage")
    
    LOWER<-min(SUBSET$Sub_metering_1,SUBSET$Sub_metering_2,SUBSET$Sub_metering_3)
    UPPER<-max(SUBSET$Sub_metering_1,SUBSET$Sub_metering_2,SUBSET$Sub_metering_3)
    plot(SUBSET$date.and.time,SUBSET$Sub_metering_1,type="l",ylim=c(LOWER,UPPER),xlab="",ylab="Energy sub metering")
    lines(SUBSET$date.and.time,SUBSET$Sub_metering_2,type="l",col="red")
    lines(SUBSET$date.and.time,SUBSET$Sub_metering_3,type="l",col="blue")
    legend("topright",col = c("black","red", "blue"), legend = names(SUBSET)[7:9],lty=c(1,1,1),bty="n",cex=0.5)
    
    LOWER<-min(SUBSET$Global_reactive_power)
    UPPER<-max(SUBSET$Global_reactive_power)
    plot(SUBSET$date.and.time,SUBSET$Global_reactive_power,type="l",ylim=c(LOWER,UPPER),xlab="datetime",ylab="Global_reactive_power")

    
    dev.copy(png,file="plot4.png")
    dev.off()
}