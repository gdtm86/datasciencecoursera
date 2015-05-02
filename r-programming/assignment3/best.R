

best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        state <- state
        outcome <- outcome
        
        ## Check that state and outcome are valid
        state_data <- subset(data, data$State == state)
        
        if (nrow(state_data) == 0){
                stop("invalid state")
        }
        
        if(outcome == 'heart attack'){
                state_data <- subset(state_data, select = c(7,2,11))
        } else if (outcome == 'heart failure'){
                state_data <- subset(state_data, select = c(7,2,17))
        } else if (outcome == 'pneumonia'){
                state_data <- subset(state_data, select = c(7,2,23))
        } else {
                stop("invalid outcome")
        }
        
        state_data[,3] <- as.numeric(state_data[,3])
        state_data <- na.omit(state_data)
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        state_data <- state_data[ order(state_data[,3],state_data[,2]),]
        low_data <- subset(state_data, state_data[,3] == min(state_data[3]))
        low_data_hospitals <- low_data$Hospital.Name[1]
        low_data_hospitals
}
