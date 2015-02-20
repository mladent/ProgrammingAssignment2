


rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
            
      ## Real path: concatenate current position & relative path in 'directory'
      directory <- paste( getwd(), "/repos/ProgrammingAssignment3/", sep = "" )
      
      fileName <- paste( directory, "outcome-of-care-measures.csv", sep = "" )
      
      
      ## Load the file
      outcomeFile <- read.csv(fileName, colClasses = "character")
      
      
      # Convert heart attack
      suppressWarnings(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                       <- as.numeric(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      
      # Convert Heart Failure
      suppressWarnings(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                       <- as.numeric(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      
      # Convert Pneumonia
      suppressWarnings(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                       <- as.numeric(outcomeFile$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      
      
      ## Split by states
      
      splitTable <- split (outcomeFile, 
                           outcomeFile$State,
                           drop = TRUE
                          )
      
      searchFor <- "Hospital.30.Day.Death..Mortality..Rates.from."
      
      if (outcome == "heart attack"){
            searchFor <- paste( searchFor, "Heart.Attack", sep = "" )
      }
      else if (outcome == "heart failure"){
            searchFor <- paste( searchFor, "Heart.Failure", sep = "" )
      }
      else if (outcome == "pneumonia"){
            searchFor <- paste( searchFor, "Pneumonia", sep = "" )
      }
      else{
            stop('invalid outcome')
      }
      
      
      if (num == "best")
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                x[which.min( x[,c(searchFor)] ), 2]     
                                
                          }
                          
            )
      else if (num == "worst")
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                x[which.max( x[,c(searchFor)] ), 2]     
                                
                          }
                          
            )
      else {
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                y <- cbind(x[ order( x[,c(searchFor)],x[,c("Hospital.Name")] ) , c("Hospital.Name")],
                                           x[ order( x[,c(searchFor)],x[,c("Hospital.Name")] ) , c(searchFor)])
                                y <- y[complete.cases(y),]
                                y[as.numeric(num)]
                          }
                          
            )
      }
      
      if(is.null(sol[[state]]))
            stop('invalid state')
      else
            sol[[state]]
      
      
      
}