
# This script defines the functions used to calculate predictions for each strategy.
# Each strategy takes as input the cues data (and possibly the shift data in addition). By default, 
# it outputs the mean squared error across all shifts and all 20 versions of each extended shift. 
# When raw=TRUE is set, each function outputs the raw data, i.e. a list of 20 versions of each shift,
# each version comprising the sid, actual end time, predicted end time, and difference between the two.
# It can be executed using "source("hh_stop_2strategies.R")" before the analysis. 


library(data.table)
library(lubridate)

######################################################################################################
# FUNCTIONS FOR STRATEGIES
######################################################################################################

max <- as.difftime(1440, units="mins") 	# 24*60min = 1440min

revenuetarget <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]])
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[d$crev>=par, .(pred=tfin[1]), by=sid]
		res <- merge(obs,pre,by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction"))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}


durationtarget <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]])
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[d$cdur>=par, .(pred=tfin[1]), by=sid]
		res <- merge(obs,pre,by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction"))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}


# durationtarget <- function(l,par,raw=FALSE){
# 	res <- lapply(1:length(l), function(i){
# 		d <- as.data.table(l[[i]]); par <- as.difftime(par, units="mins")
# 		sids <- d[imp==FALSE,.SD[.N],by=sid][,sid]
# 		real <- d[imp==FALSE,.SD[.N],by=sid][,tfin]
# 		sbeg <- d[imp==FALSE,.SD[.N],by=sid][,tfin-cdur]
# 		pred <- d[d$cdur>=par,.SD[1], by=sid][,tfin]
# 		flag <- difftime(pred,sbeg, units="mins")>=max
# 		pred[flag] <- sbeg[flag]+max		
# 		diff <- difftime(pred,real, units="mins")
# 		if(raw==TRUE){ret <- data.table(sid=sids,real=real,pred=pred,diff=diff)}
# 		if(raw==FALSE){ret <- sqrt(mean(as.numeric(diff)^2))}
# 		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
# 		ret
# 	})
# 	if(raw==TRUE){return(res)}
# 	if(raw==FALSE){return(mean(sapply(res,mean)))}
# }


tplus <- function(h){
	if(h>=0 & h<=19){out <- c(h,h+1,h+2,h+3,h+4)}
	if(h==20){out <- c(20,21,22,23,0)}
	if(h==21){out <- c(21,22,23,0,1)}
	if(h==22){out <- c(22,23,0,1,2)}
	if(h==23){out <- c(23,0,1,2,3)}
	return(out)
}


clocktarget <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]])
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[hour(d$tfin)%in%tplus(floor(par)), .(pred=tfin[1]), by=sid]
		res <- merge(obs,pre,by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction"))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}


# clocktarget <- function(l,par,raw=FALSE){
# 	res <- lapply(1:length(l), function(i){
# 		d <- as.data.table(l[[i]])
# 		sids <- d[imp==FALSE,.SD[.N],by=sid][,sid]
# 		real <- d[imp==FALSE,.SD[.N],by=sid][,tfin]
# 		sbeg <- d[imp==FALSE,.SD[.N],by=sid][,tfin-cdur]
# 		pred <- d[hour(d$tfin)%in%tplus(floor(par)),.SD[1], by=sid][,tfin]
# 		flag <- difftime(pred,sbeg, units="mins")>=max
# 		pred[flag] <- sbeg[flag]+max
# 		diff <- difftime(pred,real, units="mins")
# 		if(raw==TRUE){ret <- data.table(sid=sids,real=real,pred=pred,diff=diff)}
# 		if(raw==FALSE){ret <- sqrt(mean(as.numeric(diff)^2))}
# 		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
# 		ret
# 	})
# 	if(raw==TRUE){return(res)}
# 	if(raw==FALSE){return(mean(sapply(res,mean)))}
# }



hiatustarget <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]]); par <- as.difftime(par, units="mins")
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[,.SD[if(any(cgap>=par)) which.max(cgap>=par) else .N], by=sid][,.(sid=sid,pred=tfin)]
		res <- merge(obs,pre,by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction"))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}



# hiatustarget <- function(l,par,raw=FALSE){
# 	res <- lapply(1:length(l), function(i){
# 		d <- as.data.table(l[[i]]); par <- as.difftime(par, units="mins")
# 		# predictions and real values
# 		sids <- d[imp==FALSE,.SD[.N],by=sid][,sid]
# 		real <- d[imp==FALSE,.SD[.N],by=sid][,tfin]		
# 		sbeg <- d[imp==FALSE,.SD[.N],by=sid][,tfin-cdur]
# 		pred <- d[,.SD[if(any(cgap>=par)) which.max(cgap>=par) else .N], by=sid][,tfin]
# 		# apply ceiling to predictions
# 		flag <- difftime(pred,sbeg, units="mins")>=max
# 		pred[flag] <- sbeg[flag]+max
# 		diff <- difftime(pred,real, units="mins")
# 		# find what to return
# 		if(raw==TRUE){ret <- data.table(sid=sids,real=real,pred=pred,diff=diff)}
# 		if(raw==FALSE){ret <- sqrt(mean(as.numeric(diff)^2))}
# 		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
# 		ret
# 	})
# 	if(raw==TRUE){return(res)}
# 	if(raw==FALSE){return(mean(sapply(res,mean)))}
# }


maxu <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]]); par1 <- par[1]; par2 <- par[2]
		# calculate utilities based on current and expected values
		d$us <- d$crev - par1/(1+par2) * as.numeric(d$cdur, units="hours")^(1+par2)
		d$uc <- d$erev - par1/(1+par2) * as.numeric(d$edur, units="hours")^(1+par2)
		d[,us:= ifelse(.I == last(.I),Inf,us),by=.(sid)]
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[us>uc, .(pred=tfin[1]), by=sid]
		res <- merge(obs,pre, by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction"))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}

maxref <- function(l,par,raw=FALSE){
	res <- lapply(1:length(l), function(e){
		d <- as.data.table(l[[e]])
		theta <- par[1]  # disutility of work
		rho <- par[2]    # elasticity of marginal rate of substitution
		eta <- par[3]    # weight of gain-loss utility (0-1)
		lambda <- par[4] # coefficient of loss-aversion (typically around 2)
		rrev <- par[5]   # reference income
		rdur <- par[6]   # reference duration
		# calculate utilities based on current and expected values
		d$us <- (1-eta)*(d$crev - (theta/(1+rho)*as.numeric(d$cdur, units="hours")^(1+rho))) + eta*((d$crev-rrev<=0)*lambda*(d$crev-rrev)+(d$crev-rrev>0)*(d$crev-rrev)) - eta*((as.numeric(d$cdur, units="hours")-rdur>=0)*lambda*((theta/(1+rho)*as.numeric(d$cdur, units="hours")^(1+rho)) - (theta/(1+rho)*rdur^(1+rho)))) - eta*((as.numeric(d$cdur, units="hours")-rdur<0)*((theta/(1+rho)*as.numeric(d$cdur, units="hours")^(1+rho)) - (theta/(1+rho)*rdur^(1+rho))))
		d$uc <- (1-eta)*(d$erev - (theta/(1+rho)*as.numeric(d$edur, units="hours")^(1+rho))) + eta*((d$erev-rrev<=0)*lambda*(d$erev-rrev)+(d$erev-rrev>0)*(d$erev-rrev)) - eta*((as.numeric(d$edur, units="hours")-rdur>=0)*lambda*((theta/(1+rho)*as.numeric(d$edur, units="hours")^(1+rho)) - (theta/(1+rho)*rdur^(1+rho)))) - eta*((as.numeric(d$edur, units="hours")-rdur<0)*((theta/(1+rho)*as.numeric(d$edur, units="hours")^(1+rho)) - (theta/(1+rho)*rdur^(1+rho))))
		d[,us:= ifelse(.I == last(.I),Inf,us),by=.(sid)]
		# predictions and real values
		obs <- d[imp==FALSE, .SD[.N],by=sid][,]
		obs <- d[imp==FALSE, .(real=tfin[.N], sbeg=tfin[.N]-cdur[.N]), by=sid]
		pre <- d[us>uc, .(pred=tfin[1]), by=sid]
		res <- merge(obs,pre, by="sid")
		if(nrow(res)<0.8*nrow(obs)){print(paste0("warning: ",100*(1-round(nrow(res)/nrow(obs),2))," percent NA in prediction, theta=",theta,", rho=",rho,", eta=",eta,", lambda=",lambda,", rrev=",rrev,", rdur=",rdur))}
		# cut predictions at ceiling
		flag <- difftime(res$pred,res$sbeg, units="mins")>=max
		res$pred[flag] <- res$sbeg[flag]+max
		res$diff <- difftime(res$pred,res$real, units="mins")
		# find what to return
		if(raw==TRUE){ret <- res}
		if(raw==FALSE){ret <- sqrt(mean(as.numeric(res$diff)^2))}
		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
		ret
	})
	if(raw==TRUE){return(res)}
	if(raw==FALSE){return(mean(sapply(res,mean)))}
}

# ratmem <- function(l,par,raw=FALSE){
# 	res <- lapply(1:length(l), function(i){
# 		d <- as.data.table(l[[i]]); par1 <- par[1]; par2 <- par[2]
# 		# calculate utilities based on current and expected values
# 		d$us <- d$crev - par1/(1+par2) * as.numeric(d$cdur, units="hours")^(1+par2)
# 		d$uc <- d$erev - par1/(1+par2) * as.numeric(d$edur, units="hours")^(1+par2)
# 		d[,us:= ifelse(.I == last(.I),1e9,us),by=.(sid)]
# 		# predictions and real values
# 		sids <- d[imp==FALSE,.SD[.N],by=sid][,sid]
# 		real <- d[imp==FALSE,.SD[.N],by=sid][,tfin]
# 		sbeg <- d[imp==FALSE,.SD[.N],by=sid][,tfin-cdur]
# 		pred <- d[us>uc,.SD[1], by=sid][,tfin]
# 		#linexp <- d[us>uc,.SD[1], by=sid][,linexp]
# 		# cut predictions at ceiling
# 		flag <- difftime(pred,sbeg, units="mins")>=max
# 		pred[flag] <- sbeg[flag]+max
# 		diff <- difftime(pred,real, units="mins")
# 		# find what to return
# 		if(raw==TRUE){ret <- data.table(sid=sids,real=real,pred=pred,diff=diff)}
# 		if(raw==FALSE){ret <- sqrt(mean(as.numeric(diff)^2))}
# 		if(!raw%in%c(FALSE,TRUE)){stop("value for raw needs to be TRUE or FALSE")}
# 		ret
# 	})
# 	if(raw==TRUE){return(res)}
# 	if(raw==FALSE){return(mean(sapply(res,mean)))}
# }


# ratreg <- function(l,par1,par2){
# 	modd <- lm(sdur ~ 1 + friday + saturday + sunday + holiday + harbor + soccer + strike + precipitation + arrivals)	# model of duration
# 	modr <- lm()	# model of revenue
# 	res <- lapply(1:length(l), function(i){
# 		d <- as.data.table(l[[i]])
# 		real <- d[imp==FALSE,.SD[.N],by=sid][,tfin]
# 		sbeg <- d[imp==FALSE,.SD[.N],by=sid][,tfin-cdur]
# 		expd <- predict()	# calculate expected duration
# 		expr <- predict()	# calculate expected revenue
# 		d$eu <- d$cdur*... 	# calculate expected utility
# 		pred <- d[d$eu>=0,.SD[1], by=sid][,tfin]
# 		flag <- difftime(pred,sbeg, units="mins")>=max
# 		pred[flag] <- sbeg[flag]+max
# 		diff <- difftime(pred,real, units="mins")
# 		sqrt(mean(as.numeric(diff)^2))
# 	})
# 	mean(sapply(res,mean))
# }



