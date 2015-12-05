rankall <- function(outcome, num = "best") {
    ## Input
    ##      outcome - one of "heart attack", "heart failure", or "pneumonia"
    ##      num- can take "best", "worst", or an integer indicating the ranking
    ##      (smaller numbers are better)
    ##
    ## Return
    ##      data frame containing hospital name for each state with best, worst
    ##      or ranked 30-day death rate
    ##
    ## This function reads outcome data, check that state, outcome and num are
    ## valid and returns hospital name in that state with best, worst or ranked
    ## (smaller numbers are better) 30-day death rate
    
    rawData <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Check the outcome is valid. Then find the corresponding min mortality rate
    if (outcome == "heart attack") {
        colMortality <- 11
    }
    else if (outcome == "heart failure") {
        colMortality <- 17
    }
    else if (outcome == "pneumonia") {
        colMortality <- 23
    }
    else{
        stop("invalid outcome")
    }
    
    # Suppress warning as some mortality rates are not available
    suppressWarnings(
        rawData[colMortality] <- lapply(rawData[colMortality], as.numeric)
    )
    
    # Order the whole data acccording to increasing mortality rate
    orderMortality <- order(rawData[[colMortality]],
                            rawData$Hospital.Name,
                            na.last = NA)
    
    rawData <- rawData[orderMortality,]
    
    # Split the data for each state
    stateData <- split(rawData, rawData$State)
    
    if (num == "best") {
        hospital <- sapply(stateData, function(x) {
            x$Hospital.Name[1]
        })
    }
    else if (num == "worst") {
        hospital <- sapply(stateData, function(x) {
            len <- length(x$Hospital.Name)
            x$Hospital.Name[len]
        })
    }
    else{
        hospital <- sapply(stateData, function(x) {
            if (num > length(x$Hospital.Name)) {
                NA
            }else{
                x$Hospital.Name[num]
            }
        })
    }
    
    data.frame(hospital = hospital, state = names(stateData))
}
