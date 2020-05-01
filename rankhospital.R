rankhospital<-function(state,outcome,num="best"){
        
        ## Read outcome data
        #-------------------
        outcomeOrig_df<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        myvars<-c(2,7,11,17,23) # numbers of the columns of the variables that we need
        data<-outcomeOrig_df[,myvars] # subsets de dataframe, keeping only the variables that we need
        rm(outcomeOrig_df,myvars) 
        
        data$State<-as.factor(data$State)
        
        cols.num <- c(3:5) # idicates the numerical variables
        data[,cols.num]<-suppressWarnings(sapply(data[,cols.num],as.numeric)) # converts our numeric variables to numeric, suppressing the warning for NAs
        
        outcomes<-list("heart attack"=3,"heart failure"=4, "pneumonia"=5) #creates a list of outcomes and corresponding column numbers
        outcomeVar<-outcomes[[outcome]] #assigns the column number to 'outcomeVar'
        
        ## Check that state and outcome are valid
        #----------------------------------------
        if(is.null(outcomeVar)){
                stop("Invalid outcome") #returns an error message
        }
        validState<-sum(data$State==state)!=0
        if(validState!=TRUE){
                stop("Invalid state") #returns an error message
        }
        statedata <- data[which(data$State==state),] #subsets the state data
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        #--------------------------------------------------------
        statedata<-statedata[order(statedata[,outcomeVar], statedata$Hospital.Name),]
        statedata$Rank<-rank(statedata[,outcomeVar],ties.method="first") #
        selectRank<-statedata$Hospital.Name[statedata$Rank==num] #
        selectRank
        
}