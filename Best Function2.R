best <- function(state, outcome) {
        ## ---- Read outcome data ----
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
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
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
        
        # ----- ranks by the outcome rate -----
        colnames(cleandataframe) <- (c("hospital", "outcome1"))
        best <- min(cleandataframe$outcome1)
        outcome2 <-subset(cleandataframe, cleandataframe$outcome1 == best)
        
        # ----- alphabetize hospital name ----
        sortoutcome <- outcome2[order(outcome2$hospital),]
        
        # ----- return outcome -----
        sortoutcome[,1]
       
}



        
       
        

