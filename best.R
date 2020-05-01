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
        outcomesAbbr<-list("HA"=3,"HF"=4,"PN"=5)
        
        for(i in outcomesAbbr){
                Rank<-rank(data[,i],ties.method="min")
                varnameRank<-paste("National.Rank",names(outcomesAbbr)[outcomesAbbr==i],sep=".")
                assign(varnameRank,Rank)
                data<-cbind(data,get(varnameRank))
                names(data)[ncol(data)]<-varnameRank
        }
        
        ## Check that state and outcome are valid
        outcomeVar<-outcomes[[outcome]]
        if(is.null(outcomeVar)){
                stop("Invalid outcome") #returns an error message
        }
        
        validState<-sum(data$State==state)!=0
        if(validState!=TRUE){
                stop("Invalid state") #returns an error message
        }
        statedata <- data[ which(data$State==state), ]
        
        ## Return hospital name in that state with lowest 30-day death rate
        minoutcome<-min(statedata[,outcomeVar],na.rm=TRUE)
        statedata$Hospital.Name[which(statedata[,outcomeVar]==minoutcome)]
        
}


#sort alphabetically in case of a tie

