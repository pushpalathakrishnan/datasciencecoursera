rankall <- function(outcome, num = "best") {
  ## Read outcome data
  rkall <- data.frame(hospital = character(), state = character())
  care<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    rkall <- "invalid outcome"
  }
  else {
    k <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outkey <- k[outcome]
    ## For each state, find the hospital of the given rank
    pstate <- split(care, care$State)
    for (stat in names(pstate)) {
      ostate <- pstate[[stat]]
      out <- suppressWarnings(as.numeric(ostate[, outkey]))
      good <- complete.cases(out)
      out <- out[good]
      ostate <- ostate[good,]
      ostate <- ostate[ order(out, ostate["Hospital.Name"]), ]
      
      if (num == "best") {
        numState <- c(1)
      } else {
        if (num == "worst") {
          numState <- length(out)
        } else {
          numState <- num
        }
      }
      
      dataPart <- data.frame(hospital = ostate[numState, "Hospital.Name"], state = stat, row.names = stat)
      rkall <- rbind(rkall, dataPart)
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  rkall
  
 
}