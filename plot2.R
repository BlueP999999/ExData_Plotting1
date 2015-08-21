Plot2<-function(){
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
    
    ### 
    SUBSET$date.and.time<-strptime(
        paste(SUBSET$Date,SUBSET$Time),format="%d/%m/%Y %H:%M:%S")
    
    plot(SUBSET$date.and.time,SUBSET$Global_active_power,type="l",ylim=c(0,8),xlab="",ylab="Global Active Power (kilowatts)")

    dev.copy(png,file="plot2.png")
    dev.off()
}