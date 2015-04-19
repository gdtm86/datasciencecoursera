######### Example to run the above function #########
## corr("specdata",threshold=0
## corr("specdata",3)
corr <- function(directory,threshold=0){
        corr_vector <- numeric(0)
        for(file in list.files(directory)){
                temp_dataset <- read.csv(paste(directory,"/",file,sep=""))
                completed_temp_dataset <- temp_dataset[complete.cases
                                                       (temp_dataset),]
                noCompleteObs <- nrow(completed_temp_dataset)
                if(noCompleteObs > threshold){
                        sulfate_data <- as.matrix(completed_temp_dataset
                                                  ['sulfate'])[,1]
                        nitrate_data <- as.matrix(completed_temp_dataset
                                                  ['nitrate'])[,1]
                        corr_value <- cor(sulfate_data,nitrate_data)
                        corr_vector <- c(corr_vector,corr_value)
                }
        }  
        corr_vector
}

## Pseduo code
# * get the files in the directory and create dataframes
# * get the obeservations with no NA values from the file in a completed_temp_dataset
# * pick only the dataframes of monitors where number of observations with
#   complete cases is greater than threshold
# * 
# * create two vectors - one for nitrate, one for sulfate
# * convert the data frames into vectors
# * run the cor() function on the sulfate and nitrate
