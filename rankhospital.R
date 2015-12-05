rankhospital <- function(state, outcome, num = "best") {
    ## Input
    ##      state - name of a state
    ##      outcome - one of "heart attack", "heart failure", or "pneumonia"
    ##      num- can take "best", "worst", or an integer indicating the ranking
    ##      (smaller numbers are better)
    ##
    ## Return
    ##      hospital name in that state with best, worst or ranked 30-day 
    ##      death rate
    ##
    ## This function reads outcome data, check that state, outcome and num are 
    ## valid and returns hospital name in that state with best, worst or ranked
    ## (smaller numbers are better) 30-day death rate
    
    rawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check the state is valid. Then filter out data for that state only
    if(!is.element(state, rawData$State)){
        stop("invalid state")
    }
    
    # Check the outcome is valid. Then find the corresponding min mortality rate
    if(outcome == "heart attack"){
        colMortality <- 11
    }
    else if(outcome == "heart failure"){
        colMortality <- 17
    }
    else if(outcome == "pneumonia"){
        colMortality <- 23
    }
    else{
        stop("invalid outcome")
    }
    
    # Get only data for the input state
    stateData <- split(rawData, rawData$State)
    stateData <- stateData[[state]]
    
    # Suppress warning as some mortality rates are not available
    suppressWarnings(
        stateData[[colMortality]] <- as.numeric(stateData[[colMortality]])
    )
    
    # Order the hospital with increasing mortality rate
    orderMortality <- order(stateData[[colMortality]], 
                            stateData$Hospital.Name, 
                            na.last = NA)
    
    hospital <- stateData$Hospital.Name[orderMortality]
    
    if(num == "best"){
        hospital[1]
    }
    else if(num == "worst"){
        hospital[length(hospital)]
    }
    else{
        if(num > length(hospital)) NA else hospital[num]
    }
}
