best<-function(state,outcome){
        
        ## Read outcome data
        outcomefulldf<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        myvars<-c(2,7,11,17,23)
        data<-outcomefulldf[,myvars]
        rm(outcomefulldf,myvars)
        
        data$State<-as.factor(data$State)
        
        cols.num <- c(3:5)
        data[,cols.num]<-suppressWarnings(sapply(data[,cols.num],as.numeric))
        
        outcomes<-list("heart attack"=3,"heart failure"=4, "pneumonia"=5)
        
        ## Check that state and outcome are valid
        outcomeVar<-outcomes[[outcome]]
        if(is.null(outcomeVar)){
                return("Invalid outcome")
        }
        
        validState<-sum(data$State==state)!=0
        if(validState!=TRUE){
                return("Invalid state")
        }
        statedata <- data[ which(data$State==state), ]
        
        ## Return hospital name in that state with lowest 30-day death rate
        minoutcome<-min(statedata[,outcomeVar],na.rm=TRUE)
        statedata$Hospital.Name[which(statedata[outcomeVar]==minoutcome)]
        
}


### test
#head(data[,cols.num])
# best("TX","heart attack")
# best("TX","heart failure")
# best("MD","heart attack")
# best("MD","pneumonia")
# best("BB","heart attack")
# best("NY","hert attack")

#sort alphabetically in case of a tie
#return error messages

