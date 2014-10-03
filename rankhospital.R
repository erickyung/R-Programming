rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state, outcome and num are valid
    if (!is.element(state, outcomeData[["State"]])){
        stop("invalid state")
    }
    
    possibleOutcome <- c("heart attack", "heart failure", "pneumonia")
    possibleOutcomeColNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                                 "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    names(possibleOutcomeColNames) <- possibleOutcome
    if (!is.element(outcome, possibleOutcome)){
        stop("invalid outcome")
    }
    
    outcomeColName <- possibleOutcomeColNames[[outcome]]
    
    possibleRank <- c("best", "worst")
    if (!is.element(num, possibleRank) && !is.numeric(num)){
        stop("invalid num")
    }
    
    rank <- NULL
    if (is.numeric(num)){
        rank <- as.numeric(num)
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    stateOutcomeData <- subset(outcomeData,
                               State == state,
                               select = c("Hospital.Name", outcomeColName))
    stateOutcomeData[, outcomeColName] <- as.numeric(stateOutcomeData[, outcomeColName])
    stateOutcomeData <- stateOutcomeData[complete.cases(stateOutcomeData[[outcomeColName]]), ]
    hospitalsByRank <- stateOutcomeData[order(stateOutcomeData[[outcomeColName]], stateOutcomeData[["Hospital.Name"]]), ][["Hospital.Name"]]
    
    if (!is.null(rank) && rank <= length(hospitalsByRank)){
        hospitalsByRank[rank]
    }
    else{
        if (num == "best"){
            hospitalsByRank[1]
        }
        else if (num == "worst"){
            hospitalsByRank[length(hospitalsByRank)]
        }
        else{
            NA
        }
    }
}