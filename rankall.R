
rankall <- function(outcome, num = "best") {
      
      ## Read outcome data
      
      ## Check that state and outcome are valid
      
      ## For each state, find the hospital of the given rank
      
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      
      
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
      
      
      
      ## Setup the column name to be searched
      
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
      
      
      
      
      ## Split by states
      
      splitTable <- split (outcomeFile, 
                           outcomeFile$State,
                           drop = TRUE
      )
      
      
      
      if (num == "best")
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                minResult <- which.min( x[,c(searchFor)] )
                                
                                cbind(x[minResult , c("Hospital.Name")],
                                           x[minResult , c("State")])    

                          }
                          
            )
      else if (num == "worst")
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                maxResult <- which.max( x[,c(searchFor)] )
                                
                                cbind( hospital = x[ maxResult , c("Hospital.Name")],
                                           state = x[ maxResult , c("State")])

                          }
                          
            )
      else {
            
            ## Create a list by state
            sol <- lapply(splitTable,
                          
                          function(x) {
                                
                                orderedList <- order( x[,c(searchFor)],x[,c("Hospital.Name")] )
                                
                                selectedFromList <- orderedList [as.numeric(num)]
                                
                                cbind(x[ selectedFromList , c("Hospital.Name") ],
                                           x[ 1 , c("State")])
                                

                          }
                          
            )
      }
      

      
      solDFrame <- data.frame( 
                        matrix( unlist(sol), nrow=length(sol), byrow=T ),
                        row.names = names(sol)
                   )
      
      colnames(solDFrame) <- c("hospital", "state")
      
      solDFrame
      

      
      
      
}
