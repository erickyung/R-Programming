best <- function(state, outcome) {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
    ## Check that state and outcome are valid
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    stateOutcomeData <- subset(outcomeData,
                               State == state,
                               select = c("Hospital.Name", outcomeColName))
    stateOutcomeData[, outcomeColName] <- as.numeric(stateOutcomeData[, outcomeColName])
    stateOutcomeData[order(stateOutcomeData[[outcomeColName]], stateOutcomeData[["Hospital.Name"]]), ][["Hospital.Name"]][1]
}