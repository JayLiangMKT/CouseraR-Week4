rankhospital <- function(state,outcome, num ="best"){
	outcomeCM<- read.csv("outcome-of-care-measures.csv",colClasses="character")
	stateName<- outcomeCM[,7]
	stateNameUni <- unique(stateName[!is.na(stateName)])
	state.Name.Order <- stateNameUni[order(stateNameUni)]
	const <- list(outcome.Index=c("heart attack","heart failure","pneumonia"),
		state=state.Name.Order,
		dr.index=list(heart.attack=11,heart.failure=17,pneumonia=23),
		num.index=c("best","worst")
		)
	if(!is.element(state,const$state)){
		stop("invalid state")
	}
	if(!is.element(outcome,const$outcome)){
		stop("invalid outcome")
	}
	const.Index <- which(const$outcome.Index==outcome)
	dr <- suppressWarnings(as.numeric(outcomeCM[,const$dr.index[[const.Index]]]))
	dr <- ifelse(rep(num==const$num.index[2],length(dr)),-dr,dr)
	num <- ifelse(is.numeric(num),num,1)
	orderHospitalAll <- outcomeCM[order(dr,outcomeCM[,2]),]
	orderHospitalAll[orderHospitalAll$State==state,][,2][num]
}