######### Example to run the above function #########
## pollutantmean("specdata","sulfate",c(1,4))
## pollutantmean("specdata","nitrate",1)
## pollutantmean("specdata","sulfate",1:10)

pollutantmean <- function(directory,pollutant,monitors = 1:332){
        
        fullData <- data.frame(Date= character(0), sulfate= numeric(0), nitrate
                               = numeric(0), ID = integer(0))
        for (id in monitors){ 
                fileName <- paste(directory,"/",sprintf("%03d",id),".csv", sep="")
                data <- read.csv(fileName)
                fullData <- rbind(fullData, data)
        }
        pollutantData <- fullData[pollutant]
        mean(as.matrix(pollutantData[pollutant])[,1], na.rm=TRUE)
}

