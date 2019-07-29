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

######### Population Attributable Fraction (PAF)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########


paf <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  
  # relative complex, non ordered, weighted, two groups
  # wealth, educ, area, sex, region
  # TEST WITH dat <- disagdata.split[[4]][[1]]
  # dat$reference_subgroup <- 0
  # dat$reference_subgroup[1] <- 1
  # TEST WITH dat <- disagdata.split[[1]][[1]]
  # dat$reference_subgroup <- 0
  # dat$reference_subgroup[5] <- 1
  # dat <- disagdata.split[[33138]]
  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, par_paf_fail)
  
  if(any(badData)) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  inequal.par <- wrap.par(dat)
  inequal.paf <- (inequal.par/est_natl) * 100
  
  if(positive & inequal.paf < 0) inequal.paf  <- 0
  if(!positive & inequal.paf > 0) inequal.paf <- 0
  
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    if(bs == TRUE){
      n <- bsiter
      
      inequal.boot.vals <-  sapply(1:n, function(x){
        estrand <- rnorm(length(est), est, se)
        tmp <- wrap.par(dat, forBS = estrand)
        (tmp/est_natl) * 100
      })
      
      se.boot <- sd(inequal.boot.vals)
    }
    
    
    
    # SE Formula
    
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
    se.formula <- 100*abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
    ci <- conf.int.norm(inequal.paf, se.formula)
  }
  
  
 
  
  
  # Return the results as a list
  return(list(inequal.paf=inequal.paf, se.paf.boot=se.boot,  se.paf.formula=se.formula, se.lowerci.paf = ci$l, se.upperci.paf = ci$u))  # return a list of the inequality measure and the standard error 
  
}


