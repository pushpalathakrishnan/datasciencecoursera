best <- function(state, outcome) {
  ## Read outcome data
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% d$State)){
    stop('invalid state')
  }
  else if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
    stop('invalid outcome')
  }
  else{
    k<-c('heart attack'=11,'heart failure'=17,'pneumonia'=23)
    outkey<-k[outcome]
  } 
  ## Return hospital name in that state with lowest 30-day death
  ds<-split(d,d$State)
  resstate<-ds[[state]]
  resstate<-resstate[order(resstate["Hospital.Name"]), ]
  ##resstate$Hospital.Name
  ##resoutcome <- suppressWarnings(as.numeric(resstate[, outkey]))
  good <- complete.cases(resoutcome)
  resoutcome <- resoutcome[good]
  resstate <- resstate[good,]
  minimum <- min(resoutcome)
  index <- match(minimum, resoutcome)
  result <- resstate[index, 2]
  ## rate
  result
}
