best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
        dataframe <- cbind.data.frame (data[, 2], ##hospital
                      data[, 7], ##state
                      data[, 11], ##deaths from heartattack
                      data[, 17], ##deaths from heartfailure
                      data[, 23], ##deaths from pneumonia
                      na.strings="Not Available",
                      stringAsfactors=FALSE)
        colnames(dataframe) <- (c("hospital", "state", "heart attack", "heart failure", "pneumonia"))
                   
        ## Check that state and outcome are valid
        if(!state %in% dataframe[, "state"]) { 
                        stop('invalid state') 
                }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                        stop('invalid outcome')   
                }
        ## Return hospital name in that state with lowest 30-day death rate
      
                            
        
} 
