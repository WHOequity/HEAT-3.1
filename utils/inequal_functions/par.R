# © Copyright World Health Organization (WHO) 2016.
# This file is part of the Health Equity Assessment Toolkit (HEAT).
# HEAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License Version 2 as published by
# the Free Software Foundation.
# 
# HEAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with HEAT. If not, see http://www.gnu.org/licenses/.

######### Population Attributable Risk (PAR)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.par <- function(dat, forBS = NULL){
  
  if(!is.null(forBS)) dat$estimate <- forBS
  
  funcname <- as.character(match.call()[[1]])
  make_pieces(dat, funcname)
  ord <- is_ordered(dat)
  positive <- is_positive(dat)
  est_se <- get_est_se_min_max(dat)
  est_max <- est_se$estimate[est_se$type == "max"]
  est_min <- est_se$estimate[est_se$type == "min"]
  most_adv_indx <- which_most_advantaged(dat)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  if(ord & !hasrefgroup) ref_est <- dat$estimate[most_adv_indx]
  if(!ord & !hasrefgroup) ref_est <- ifelse(positive, est_max, est_min)
  if(hasrefgroup) ref_est <- dat$estimate[dat$reference_subgroup==1]
  
  inequal.par <- ref_est - est_natl
  
  if(positive & inequal.par < 0) inequal.par  <- 0
  if(!positive & inequal.par > 0) inequal.par <- 0
  
  return(inequal.par)
}

#dat<-onestrata      
PAR <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){


  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, par_paf_fail)
  
  if(any(badData)) return(na_return)
  
  
  
  
  inequal.par <- wrap.par(dat)
  

  
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    
    if(bs == TRUE){
      n <- bsiter
      
      inequal.boot.vals <-  sapply(1:n, function(x){
        estrand <- rnorm(length(est), est, se)
        wrap.par(dat, forBS = estrand)
      })
      
      se.boot <- sd(inequal.boot.vals)
    }
    
    
    mu<-weighted.mean(est, pop) #af
    
    co6<-pop[which.min(est)] - (min(est)/100)*pop[which.min(est)]
    cp6<-(min(est)/100)*pop[which.min(est)]
    cq6 <- (mu/100)*sum(pop)-cp6
    cr6 <- sum(pop)-cq6-cp6-co6
    cv6 <- mu-min(est)
    cx6 <- cv6/mu
    cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(pop)*cp6)))
    
    ct6<-(log(1-cx6))-qnorm(0.975)*cs6
    cu6<-(log(1-cx6))+qnorm(0.975)*cs6
    cy6<-abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
    
    se.formula <- abs(mu*((cx6+qnorm(0.975)*cy6)-(cx6-qnorm(0.975)*cy6))) / (2*qnorm(0.975))
    ci <- conf.int.norm(inequal.par, se.formula)
  } # end SE calcs
  
  
  
  return(list(inequal.par=inequal.par, se.par.boot=se.boot,  se.par.formula=se.formula, se.lowerci.par = ci$l, se.upperci.par = ci$u))  # return a list of the inequality measure and the standard error 
  
}
