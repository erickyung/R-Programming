rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome and num are valid
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
    
    ## For each state, find the hospital of the given rank
    statesOutcomeData <- outcomeData
    statesOutcomeData[, outcomeColName] <- as.numeric(statesOutcomeData[, outcomeColName])
    validStatesOutcomeData <- statesOutcomeData[complete.cases(statesOutcomeData[[outcomeColName]]),
                                                c("Hospital.Name", "State", outcomeColName)]
    rankResult <- lapply(split(validStatesOutcomeData, validStatesOutcomeData[["State"]]),
                         function(stateOutcomeData){
                             hospitalsByRank <- stateOutcomeData[order(stateOutcomeData[[outcomeColName]],
                                                                       stateOutcomeData[["Hospital.Name"]]), ]
                             
                             data <- data.frame()
                             if (!is.null(rank) && rank <= nrow(hospitalsByRank)){
                                 data <- data.frame(hospitalsByRank[rank, c("Hospital.Name", "State")])
                             }
                             else{
                                 if (num == "best"){
                                     data <- data.frame(hospitalsByRank[1, c("Hospital.Name", "State")])
                                 }
                                 else if (num == "worst"){
                                     data <- data.frame(hospitalsByRank[nrow(hospitalsByRank), c("Hospital.Name", "State")])
                                 }
                                 else{r
                                      data <- data.frame(NA, hospitalsByRank[1, c("State")])
                                 }
                             }
                             
                             colnames(data) <- c("hospital", "state")
                             data
                         })
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    result <- data.frame(Reduce(rbind, rankResult))
    rownames(result) <- result[["state"]]
    result
}