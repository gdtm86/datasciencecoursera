######### Example to run the above function #########
## complete("specdata",c(1,4))
## complete("specdata",1)
## complete("specdata",1:10)

complete <- function(directory,monitors = 1:332){
        completeData <- data.frame(id = integer(0),nobs = numeric(0))
        for (ID in monitors){ 
                fileName <- paste(directory,"/",sprintf("%03d",ID),".csv", sep="")
                data <- read.csv(fileName)
                singleFileData <- data[complete.cases(data),]
                completeData <- rbind(completeData,data.frame(id=ID,nobs =nrow(singleFileData)))
        }
        completeData
}
