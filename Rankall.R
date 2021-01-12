rankall <- function(outcome, num = "best") {
        ## ---- Read outcome data ----
        data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available") 
        dataframe <- cbind.data.frame (data[, 2], ##hospital
                                       data[, 7], ##state
                                       data[, 11], ##deaths from heartattack
                                       data[, 17], ##deaths from heartfailure
                                       data[, 23], ##deaths from pneumoia
                                       stringAsFactors = FALSE)
        colnames(dataframe) <- (c("hospital", "state", "heart attack", "heart failure", "pneumonia"))
        
        
        # ---- check if the arguments are valid ---
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        # ---- subset the data by first 1) the rows with the desired state 2) the column we want (hospital, state, outcome) 3) removes NAs ----
        subdf <- subset(dataframe, dataframe == dataframe, c(1,2,
                                                                       if(outcome == "heart attack") {
                                                                               3 }
                                                                       else if (outcome =="heart failure") {
                                                                               4 }
                                                                       else {outcome =="pneumonia"
                                                                               5}  
        ))
        
        
        cleandf <- na.omit(subdf)
        colnames(cleandf) <- (c("hospital", "state", "mortality_rate"))
        ## ---- subset the data ____
        
        orderdf <- cleandf[order(cleandf$state, cleandf$mortality_rate, cleandf$hospital),]
        states <- split(orderdf, orderdf$state)
        states
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        if(num == "best") {
                states[1,1:2]
        }
        else if(num == "worst") {
                states[nrow(states),1:2]
        }
        else{
                states[num,1:2]
        }
}

        ## ---- subset the data ____
        
        #orderdf <- cleandf[order(cleandf$state, cleandataframe$hospital),]
        #states <- split(dataframe, dataframe$state)
        #lapply(states, )
                                                                                 
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name




## notes
## HAdf <- na.omit(select(dataframe, "hospital", "state", "heart attack")) --> gave me dataframe w/ three columns and omit NA
## orderdf <- HAdf[order(HAdf$state, HAdf$`heart attack`),] --> ordered by state then by outcome rank
## HAstates <- split(orderdf, orderdf$state)


