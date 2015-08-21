Plot1<-function(){
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
    
    ### Set margin, plot, label axis and title
    par(mar=c(5,5,2,2))
    hist(SUBSET$Global_active_power,xlab="Global Active Power (kilowatts)",ylab="Frequency",main="Global Active Power",col="red")

    dev.copy(png,file="plot1.png")
    dev.off()
}