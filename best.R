best <- function(state, outcome) {
    ## Input
    ##      state - name of a state
    ##      outcome - one of "heart attack", "heart failure", or "pneumonia"
    ##
    ## Return
    ##      hospital name in that state with lowest 30-day death rate
    ##
    ## This function reads outcome data, check that state and outcome are valid
    ## and returns hospital name in that state with lowest 30-day death rate
    
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
    
    # Find the hospital with minimum mortality rate
    minMortality <- min(stateData[[colMortality]], na.rm = TRUE)
    hospital <- stateData$Hospital.Name[stateData[colMortality] == minMortality]

    # Sort and return the first data    
    hospital <- sort.list(hospital, na.last = NA)
    hospital[1]
}
