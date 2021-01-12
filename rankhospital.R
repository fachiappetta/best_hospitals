rankhospital <- function(state, outcome, num = "best") {
        ## ---- Read outcome data -----
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available") 
        dataframe <- cbind.data.frame (data[, 2], ##hospital
                                       data[, 7], ##state
                                       data[, 11], ##deaths from heartattack
                                       data[, 17], ##deaths from heartfailure
                                       data[, 23], ##deaths from pneumoia
                                       stringAsFactors = FALSE)
        colnames(dataframe) <- (c("hospital", "state", "heart attack", "heart failure", "pneumonia"))
        state1 <- state
        
        # ---- check if the arguments are valid ---
        if(outcome == "heart attack") {
                outcome <- "heart attack"
        }
        else if(outcome == "heart failure") {
                outcome <- "heart failure"
        }
        else if(outcome == "pneumonia") {
                outcome <- "pneumonia"
        }
        else {
                stop("invalid outcome")
        }
        if(!state %in% dataframe$state) {
                stop("invalid state") 
        }
        
        # ---- subset the data by first 1) the rows with the desired state 2) the column we want (hospital, state, outcome) 3) removes NAs ----
        
        subdataframe <- subset(dataframe, dataframe$state == state1, c(1,
                                                                       
                                                                       if(outcome == "heart attack") {
                                                                               3 }
                                                                       else if (outcome =="heart failure") {
                                                                               4 }
                                                                       else {outcome =="pneumonia"
                                                                               5}  
                                                                        )
                                  )
        
        cleandataframe <- na.omit(subdataframe)
        
        ## ---- Return hospital name in that state with the given rank ---- 
        colnames(cleandataframe) <- (c("hospital", "outcome1"))
        ordered <- cleandataframe[order(cleandataframe$outcome1, cleandataframe$hospital),]
        
        ## ---- 30-day death rate ---- 
        if(num == "best") {
                ordered[1,1]
        }
        else if(num == "worst") {
                ordered[nrow(ordered),1]
        }
        else{
                ordered[num,1]
        }
}
