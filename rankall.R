rankall <- function(outcome, num="best"){
	outcomeCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	stateName <- outcomeCM[,7]
	ha_dr <- suppressWarnings(as.numeric(outcomeCM[,11]))
	hf_dr <- suppressWarnings(as.numeric(outcomeCM[,17]))
	pn_dr <- suppressWarnings(as.numeric(outcomeCM[,23]))
	stateNameUni<- unique(stateName[!is.na(stateName)])
	state.Name.Order <- stateNameUni[order(stateNameUni)]
	const <- list(outcome=c("heart attack","heart failure","pneumonia"),state=state.Name.Order,dr.w.na=list(heart.attack=ha_dr,heart.failure=hf_dr,pneumonia=pn_dr))
	if (!is.element(outcome,const$outcome)){
		stop("invalid outcome")
	}
	hospitalNumber <- function(state,outcome){
		if(outcome=="heart attack"){
			dr=ha_dr
			orderHospitalAll<-outcomeCM[order(dr,outcomeCM[,2]),]
			orderHospitalValid<-orderHospitalAll[orderHospitalAll[,11]!="Not Available",]
		}
		if(outcome=="heart failure"){
			dr=hf_dr
			orderHospitalAll<-outcomeCM[order(dr,outcomeCM[,2]),]
			orderHospitalValid<-orderHospitalAll[orderHospitalAll[,17]!="Not Available",]
		}
		if(outcome=="pneumonia"){
			dr=pn_dr
			orderHospitalAll<-outcomeCM[order(dr,outcomeCM[,2]),]
			orderHospitalValid<-orderHospitalAll[orderHospitalAll[,23]!="Not Available",]
		}
		length(orderHospitalValid[orderHospitalValid$State==state,][,2])
	}
	
	rankhospitalLite <- function(state, outcome, num="best"){
		if(outcome=="heart attack"){
			orderHA<-outcomeCM[order(ha_dr,outcomeCM[,2]),]
			if(num=="best"){num=1}
			if(num=="worst"){num<-hospitalNumber(state,outcome)}
			if(num>hospitalNumber(state,outcome)){num=""}
			orderHA[orderHA$State==state,][,2][num]
		}
		else if(outcome=="heart failure"){
			orderHA<-outcomeCM[order(hf_dr,outcomeCM[,2]),]
			if(num=="best"){num=1}
			if(num=="worst"){num<-hospitalNumber(state,outcome)}
			if(num>hospitalNumber(state,outcome)){num=""}
			orderHA[orderHA$State==state,][,2][num]
		}
		else if(outcome=="pneumonia"){
			orderHA<-outcomeCM[order(pn_dr,outcomeCM[,2]),]
			if(num=="best"){num=1}
			if(num=="worst"){num<-hospitalNumber(state,outcome)}
			if(num>hospitalNumber(state,outcome)){num=""}
			orderHA[orderHA$State==state,][,2][num]
		}
	}
	resu <- data.frame(hospital="",state=const$state)
	resu$hospital <- sapply(const$state,rankhospitalLite,outcome,num)
	return(resu)
}