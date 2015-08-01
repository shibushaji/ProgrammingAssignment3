best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #### Reading outcome data  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #### Filter outcome data for the desired state
  dset <- data[data$State == state,]

  #### getting number of rows after filtering
  rows <- nrow(dset)
  
  if(rows > 0) {
    ####  State is valid
    
    #### make a list with valid values to compare against
    valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    
    #### checking for outcome...
    if (! outcome %in% valid_outcomes) {
      stop('invalid outcome')
    }
  }
  else {
    #### No rows were there after filtering. So return error.
    stop('invalid state')
  }

  #### Process for heart attack
  if (outcome == 'heart attack') {
    #### first get the lowest rate for the state
    lowest_rate <- min( suppressWarnings(as.numeric( dset[ ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])), na.rm = TRUE)

    #### Now get the dataset filtered even more to pull the records with lowest mortality rate
    filtered <- dset[suppressWarnings(as.numeric(dset$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")) == lowest_rate, ]
  }
  
  #### Process for heart failure
  if (outcome == 'heart failure') {
    #### first get the lowest rate for the state
    lowest_rate <- min( suppressWarnings(as.numeric(dset[ ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])),  na.rm = TRUE)

    #### Now get the dataset filtered even more to pull the records with lowest mortality rate
    filtered <- dset[suppressWarnings(as.numeric(dset$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")) == lowest_rate, ]
  }
  
  #### Process for pneumonia
  if (outcome == 'pneumonia') {
    #### first get the lowest rate for the state
    lowest_rate <- min( suppressWarnings(as.numeric(dset[ ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])), na.rm = TRUE)
    
    #### Now get the dataset filtered even more to pull the records with lowest mortality rate
    filtered <- dset[suppressWarnings(as.numeric(dset$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")) == lowest_rate, ]
  }
  
  #### Just get the hospital name column from the filtered list
  final <- filtered[,"Hospital.Name"]
  
  #### to handle ties, first sort by name and then select the top row (hospital name)
  final <- head( sort(final), 1)
  print(final)
}

