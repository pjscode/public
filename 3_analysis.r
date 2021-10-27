
# This script takes as input the hh_selected_data.RData file and the hh_cues_driverXX.RData file 
# to assess the predictive accuracy of all five strategies for driver XX. It requires the 
# hh_stop_1strategies.R file to be in the same folder and outputs the 10-fold cross-validation results
# in hh_predacc_driverXX.RData, each containing for all 20 versions, the predicted and actual end times
# of each shift for each strategy. 
# The script requires the MPI grid, where the job can be called by:
# day shifts (adjust parameters below and save)
# echo 'module load R/3.6 ; Rscript hh_stop_3analysis.r $PBS_ARRAYID' | qsub -N testday -d. -t 1-2197 -l walltime=31:0:0 -m af -j oe -o $HOME/hh/testlogs/
# night shifts (adjust parameters below and save)
# echo 'module load R/3.6 ; Rscript hh_stop_3analysis.r $PBS_ARRAYID' | qsub -N testnight -d. -t 1-2136 -l walltime=31:0:0 -m af -j oe -o $HOME/hh/testlogs/
# where 1-X regulates the number of drivers processed. 


rm(list=ls())
###setwd("/Users/Perke/Volumes/tardis/hh/")

# load packages and functions for strategies
source("hh_stop_2strategies.r")

# relevant libraries
library(dfoptim)	# derivative-free optimization algorithms


######################################################################################################
# ANALYSIS
######################################################################################################

# set driver from command line argument
i <- as.integer(commandArgs(TRUE)[1])
load("./dataout/hh_selected_data.RData")

####################################################
# modify here according to shift type
####################################################

# select shift type
shift <- "night"

# restrict cues to selected shift type
#dno <- unlist(d[olddayshifts>25 | newdayshifts>25,"did"][i])
dno <- unlist(d[oldnightshifts>25 | newnightshifts>25,"did"][i])

####################################################
 
# load data
load(paste0("./testin/hh_cues_driver",dno,".RData"))
rm(d); rm(t)

# restrict to day or night shifts only
cues <- lapply(cues, function(x) x[stype==shift])

# set up analysis
# sids before and after increase / minimum wage
oldsids <- unique(cues[[1]][date(tbeg)<date(as.POSIXct("2014-10-01 00:00:00"))]$sid)
newsids <- unique(cues[[1]][date(tbeg)>=date(as.POSIXct("2015-01-01 00:00:00"))]$sid)
oldsids <- oldsids[!oldsids%in%intersect(oldsids,newsids)]; newsids <- newsids[!newsids%in%intersect(oldsids,newsids)]

# determine whether minimum number of shifts before/after fare increase / minimum wage 
min <- 25
flagold <- length(oldsids)>=min; if(flagold){print("enough old shifts")}; if(!flagold){print("too few old shifts")}
flagnew <- length(newsids)>=min; if(flagnew){print("enough new shifts")}; if(!flagnew){print("too few new shifts")}

# create and shuffle fold assignments
if(flagold){
	set.seed(41)
	oldfolds <- cut(seq(1,length(oldsids)), breaks=10, labels=FALSE)
	set.seed(42)
	oldfolds <- oldfolds[sample(1:length(oldsids))]
}
if(flagnew){
	set.seed(41)
	newfolds <- cut(seq(1,length(newsids)), breaks=10, labels=FALSE)
	set.seed(42)
	newfolds <- newfolds[sample(1:length(newsids))]
}



# run 10-fold CV for shifts before increase / minimum wage
if(flagold){
	print("start old shifts")
	rev.resb <- dur.resb <- clock.resb <- hia.resb <- maxu.resb <- maxref.resb <- list()
	for(k in 1:10){
		print(paste0("start oldfold ",k))
		fold <- oldsids[oldfolds==k]
		test <- lapply(cues, subset, sid%in%fold)
		fit <- lapply(cues, subset, !sid%in%fold)

		# fit strategies
		print(paste0("oldfold ",k,": fit revenue"))
		rev.fit <- optim(par=100, revenuetarget, l=fit, method="Brent", lower=1, upper=300)
		rev.par <- rev.fit$par

		print(paste0("oldfold ",k,": fit duration"))
		dur.fit <- optim(par=60*8, durationtarget, l=fit, method="Brent", lower=15, upper=60*24)
		dur.par <- dur.fit$par

		print(paste0("oldfold ",k,": fit clock"))
		clock.fit <- optim(par=20, clocktarget, l=fit, method="Brent", lower=0, upper=23)
		clock.par <- clock.fit$par

		print(paste0("oldfold ",k,": fit hiatus"))
		hia.fit <- optim(par=60, hiatustarget, l=fit, method="Brent", lower=5, upper=240)
		hia.par <- hia.fit$par

		print(paste0("oldfold ",k,": fit maxu"))
		pars <- c(0.5,0.5)
		low <- c(0,0); up <- c(Inf,Inf)
		maxu.fit <- hjkb(par=pars, maxu, lower=low, upper=up, l=fit)
		maxu.par <- maxu.fit$par

		print(paste0("oldfold ",k,": fit maxref"))
		pars <- c(0,0,0.5,2,s[did==dno,mean(srev)],as.numeric(s[did==dno,mean(sdur)], units="hours"))
		low <- c(0,0,0,0,0,0); up <- c(Inf,Inf,1,Inf,Inf,Inf)
		maxref.fit <- hjkb(par=pars, maxref, lower=low, upper=up, l=fit)
		maxref.par <- maxref.fit$par

		# test strategies
		print(paste0("oldfold ",k,": test revenue"))
		rev.test 	<- revenuetarget(test, rev.par, raw=TRUE)
		print(paste0("oldfold ",k,": test duration"))
		dur.test 	<- durationtarget(test, dur.par, raw=TRUE)
		print(paste0("oldfold ",k,": test clock"))
		clock.test 	<- clocktarget(test, clock.par, raw=TRUE)
		print(paste0("oldfold ",k,": test hiatus"))
		hia.test 	<- hiatustarget(test, hia.par, raw=TRUE)
		print(paste0("oldfold ",k,": test maxu"))
		maxu.test <- maxu(test, maxu.par, raw=TRUE)
		print(paste0("oldfold ",k,": test maxref"))
		maxref.test <- maxref(test, maxref.par, raw=TRUE)

		# collect additional data
		sdat <- s[sid%in%fold,c("did","sid","ctype","stype","srev","sdur","sfin")]
		sdat$fold <- k
		sdat$new <- FALSE
		sdat$rev.par <- rev.par
		sdat$dur.par <- dur.par
		sdat$clock.par <- clock.par
		sdat$hia.par <- hia.par
		sdat$maxu.theta <- maxu.par[1]
		sdat$maxu.rho <- maxu.par[2]
		sdat$maxref.theta <- maxref.par[1]
		sdat$maxref.rho <- maxref.par[2]
		sdat$maxref.eta <- maxref.par[3]
		sdat$maxref.lambda <- maxref.par[4]
		sdat$maxref.rrev <- maxref.par[5]
		sdat$maxref.rdur <- maxref.par[6]

		# export
		print(paste0("oldfold ",k,":  export"))
		if(k==1) {	rev.resb <- rev.test
					dur.resb <- dur.test
					clock.resb <- clock.test
					hia.resb <- hia.test
					maxu.resb <- maxu.test
					maxref.resb <- maxref.test
					shift.resb <- sdat
				}
		if(k>1){	rev.resb <- Map(rbind, rev.resb, rev.test)
					dur.resb <- Map(rbind, dur.resb, dur.test)
					clock.resb <- Map(rbind, clock.resb, clock.test)
					hia.resb <- Map(rbind, hia.resb, hia.test)
					maxu.resb <- Map(rbind, maxu.resb, maxu.test)
					maxref.resb <- Map(rbind, maxref.resb, maxref.test)
					shift.resb <- rbind(shift.resb, sdat)
				}
		print(paste0("finsihed oldfold ",k))
	}
}

if(flagnew){
	print("start new shifts")
	rev.resp <- dur.resp <- clock.resp <- hia.resp <- maxu.resp <- maxref.resp <- list()
	for(k in 1:10){
		print(paste0("start newfold ",k))
		fold <- newsids[newfolds==k]
		test <- lapply(cues, subset, sid%in%fold)
		fit <- lapply(cues, subset, !sid%in%fold)

		# fit strategies
		print(paste0("newfold ",k,": fit revenue"))
		rev.fit <- optim(par=100, revenuetarget, l=fit, method="Brent", lower=1, upper=300)
		rev.par <- rev.fit$par

		print(paste0("newfold ",k,": fit duration"))
		dur.fit <- optim(par=60*8, durationtarget, l=fit, method="Brent", lower=15, upper=60*24)
		dur.par <- dur.fit$par

		print(paste0("newfold ",k,": fit clock"))
		clock.fit <- optim(par=20, clocktarget, l=fit, method="Brent", lower=0, upper=23)
		clock.par <- clock.fit$par

		print(paste0("newfold ",k,": fit hiatus"))
		hia.fit <- optim(par=60, hiatustarget, l=fit, method="Brent", lower=5, upper=240)
		hia.par <- hia.fit$par

		print(paste0("newfold ",k,": fit maxu"))
		pars <- c(0.5,0.5)
		low <- c(0,0); up <- c(Inf,Inf)
		maxu.fit <- hjkb(par=pars, maxu, lower=low, upper=up, l=fit)
		maxu.par <- maxu.fit$par

		print(paste0("newfold ",k,": fit maxref"))
		pars <- c(0,0,0.5,2,s[did==dno,mean(srev)],as.numeric(s[did==dno,mean(sdur)], units="hours"))
		low <- c(0,0,0,0,0,0); up <- c(Inf,Inf,1,Inf,Inf,Inf)
		maxref.fit <- hjkb(par=pars, maxref, lower=low, upper=up, l=fit)
		maxref.par <- maxref.fit$par

		# test strategies
		print(paste0("newfold ",k,": test revenue"))
		rev.test 	<- revenuetarget(test, rev.par, raw=TRUE)
		print(paste0("newfold ",k,": test duration"))
		dur.test 	<- durationtarget(test, dur.par, raw=TRUE)
		print(paste0("newfold ",k,": test clock"))
		clock.test 	<- clocktarget(test, clock.par, raw=TRUE)
		print(paste0("newfold ",k,": test hiatus"))
		hia.test 	<- hiatustarget(test, hia.par, raw=TRUE)
		print(paste0("newfold ",k,": test maxu"))
		maxu.test <- maxu(test, maxu.par, raw=TRUE)
		print(paste0("newfold ",k,": test maxref"))
		maxref.test <- maxref(test, maxref.par, raw=TRUE)

		# collect additional data
		sdat <- s[sid%in%fold,c("did","sid","ctype","stype","srev","sdur","sfin")]
		sdat$fold <- k
		sdat$new <- TRUE
		sdat$rev.par <- rev.par
		sdat$dur.par <- dur.par
		sdat$clock.par <- clock.par
		sdat$hia.par <- hia.par
		sdat$maxu.theta <- maxu.par[1]
		sdat$maxu.rho <- maxu.par[2]
		sdat$maxref.theta <- maxref.par[1]
		sdat$maxref.rho <- maxref.par[2]
		sdat$maxref.eta <- maxref.par[3]
		sdat$maxref.lambda <- maxref.par[4]
		sdat$maxref.rrev <- maxref.par[5]
		sdat$maxref.rdur <- maxref.par[6]

		# export
		print(paste0("newfold ",k,":  export"))
		if(k==1) {	rev.resp <- rev.test
					dur.resp <- dur.test
					clock.resp <- clock.test
					hia.resp <- hia.test
					maxu.resp <- maxu.test
					maxref.resp <- maxref.test
					shift.resp <- sdat
				}
		if(k>1){	rev.resp <- Map(rbind, rev.resp, rev.test)
					dur.resp <- Map(rbind, dur.resp, dur.test)
					clock.resp <- Map(rbind, clock.resp, clock.test)
					hia.resp <- Map(rbind, hia.resp, hia.test)
					maxu.resp <- Map(rbind, maxu.resp, maxu.test)
					maxref.resp <- Map(rbind, maxref.resp, maxref.test)
					shift.resp <- rbind(shift.resp, sdat)
				}
		print(paste0("finsihed newfold ",k))
	}
}


# create joint data table 
if(flagold & flagnew){	rev.res <- Map(rbind, rev.resb, rev.resp)
						dur.res <- Map(rbind, dur.resb, dur.resp)
						clock.res <- Map(rbind, clock.resb, clock.resp)
						hia.res <- Map(rbind, hia.resb, hia.resp)
						maxu.res <- Map(rbind, maxu.resb, maxu.resp)
						maxref.res <- Map(rbind, maxref.resb, maxref.resp)
						shift.res <- rbind(shift.resb, shift.resp)
}
if(flagold & !flagnew){	rev.res <- rev.resb
						dur.res <- dur.resb
						clock.res <- clock.resb
						hia.res <- hia.resb
						maxu.res <- maxu.resb
						maxref.res <- maxref.resb
						shift.res <- shift.resb
}
if(!flagold & flagnew){	rev.res <- rev.resp
						dur.res <- dur.resp
						clock.res <- clock.resp
						hia.res <- hia.resp
						maxu.res <- maxu.resp
						maxref.res <- maxref.resp
						shift.res <- shift.resp
}


# save results
save(rev.res, dur.res, clock.res, hia.res, maxu.res, maxref.res, shift.res, file=paste0("./testout/hh_test",shift,"_driver",dno,".RData"))
print(paste0("Job completed. Result saved as hh_test",shift,"_driver",dno,".RData"))









