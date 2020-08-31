rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  cv=read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% cv$State)){
    stop('invalid state')
  }
  else if(!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))){
    stop('invalid outcome')
  }
  else{
    k<-c('heart attack'=11,'heart failure'=17,'pneumonia'=23)
    outkey<-k[outcome]
    print('outkey<-k[outcome]')
    print(outkey)
  } 
  ## Return hospital name in that state with the given rank
## split records based on all available states 
  pstate<- split(cv, cv$State)
## take records that only belongs to the state that given in input argument
  ostate <- pstate[[state]]
  ##print('ostate <- pstate[[state]]')
  ##print(ostate)
## find out the column values of outcome field taken from input argument
  out <- suppressWarnings(as.numeric(ostate[, outkey]))
  print('out <- suppressWarnings(as.numeric(ostate[, outkey]))')
  print(out)
## test and the boolean value false corresponding to NA's and true otherwise
  good <- complete.cases(out)
## take the original values from the data set that has no NA's
  out <- out[good]
  ##ostate_temp<-ostate[good]
## filter all updated records (having non NA's)
  ostate <- ostate[good,]
## Arrange in an alphabetical order of records based on hospital name
  ostate <- ostate[order(out, ostate["Hospital.Name"]),]
  if (grepl("^[0-9]+$", num)) {
    if (as.numeric(num) > length(out)) {
      result <- NA
    }
    else {
      result <- ostate[as.numeric(num), "Hospital.Name"]
    }
  }    
  else if (num == "best") {
    result <- ostate[1, "Hospital.Name"]
  }
  else if (num == "worst") {
    result <- ostate[length(out), "Hospital.Name"]
  }
  else result <- NA
## 30-day death rate
result
}
