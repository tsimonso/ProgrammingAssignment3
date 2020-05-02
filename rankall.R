rankall<-function(outcome,num="best"){
        ## Read outcome data
        ##-------------------
        outcomeOrig_df<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        myvars<-c(2,7,11,17,23) # numbers of the columns of the variables that we need
        data<-outcomeOrig_df[,myvars] # subsets de dataframe, keeping only the variables that we need
        rm(outcomeOrig_df,myvars) 
        
        data$State<-as.factor(data$State)
        
        cols.num <- c(3:5) # idicates the numerical variables
        data[,cols.num]<-suppressWarnings(sapply(data[,cols.num],as.numeric)) # converts our numeric variables to numeric, suppressing the warning for NAs
        
        outcomes<-list("heart attack"=3,"heart failure"=4, "pneumonia"=5) #creates a list of outcomes and corresponding column numbers
        outcomeVar<-outcomes[[outcome]] #assigns the column number to 'outcomeVar'
        
        ## Check that outcome is valid
        ##-----------------------------
        if(is.null(outcomeVar)){
                stop("Invalid outcome") #returns an error message
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ##-----------------------------------------------------
        
        atRank<-num #copies the value of the rank argument, before the
        if(num=="best") atRank<-1 # interprets 'Best' as rank=1
        result<-NA # creates the object result that will receive the names of the hospitals

        states<-levels(data$State)
        for(i in seq_along(states)){ # loops over the states
                state<-states[i] # abbreviated name of the state
                statedata<-data[which(data$State==state),] #subsets the state data
                
                nrObs<-sum(!is.na(statedata[,outcomeVar])) #number of non missing observation for that outcome
                if(num=="worst") {
                        atRank<-nrObs # 'Worst' = last rank in the state
                }
                
                statedata<-statedata[order(statedata$Hospital.Name),] # sorts the observations by the outcome variable and by the name of the hospital
                statedata$Rank<-rank(statedata[,outcomeVar],ties.method="first")#,na.last=TRUE) # ranks the hospitals. If outcome=NA -> rank=NA
                ##In case of a tie, the ranks are given in the order of appearance in the df (already sorted alphabetically).
                HospAtRank<-statedata$Hospital.Name[statedata$Rank==atRank] # Name of the hospital at the selected rank
                if(identical(HospAtRank, character(0))) HospAtRank<-NA # If there is no hospital at that rank, NA is returned
                result[i]<-HospAtRank # adds the name of the hospital to the vector
        }
        resultDf<-data.frame(hospital=result,state=states,row.names=states) # creates the output dataframe
        resultDf
}