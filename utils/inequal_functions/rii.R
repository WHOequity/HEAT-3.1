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

######### Relative Index of Inequality (RII)



rii <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  
  # relative, complex, ordered, weighted, greater than two
  # wealth, educ
  # TEST WITH: dat <- disagdata.split[[205]]
  
  #dat <- arrange(dat, subgroup_order)
  dat$subgroup.new <- -1*((dat$indicator_scale==1)-0.5)*(dat$favourable_indicator-0.5) * dat$subgroup_order
  dat <- dat[order(dat$country, dat$year, dat$indicator_abbr, dat$dimension, dat$subgroup.new),]
  
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                          two_subgroups,
                          not_ordered,
                          missing_estimates,
                          missing_population,
                          #missing_natl_avg,
                         estimates_all_zero,
                         #natl_all_zero,
                         se_all_missing,
                         has_zero_se,
                         less_than2_subgroups)
  
  # git845 manual computation, moved here from below
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est) 
  
  if(any(badData) || is.na(est_natl) || !SEuseful) return(na_return)
  
  est <- round((est/scaleval) * pop)
  pop <- round(pop)
  
  # Compute the ranks
  rank <- midPointProp(pop)
  
  # Set up new data with the y/pop as y
  newdat <- data.frame(rank=rank, est=est, pop = pop)

  
  res <- try(rma.glmm(measure="PLO", mods = ~rank, xi=est, ni=pop, method="FE",data=newdat), TRUE)

  warn <- any(class(res) == "try-error")
  
  # renamed to rii but leaving as kmi here
  inequal.kmi <- NA
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  
  if(!warn){
    
    model2 <- res
    summary.info <- coef(summary(model2))
    coefs <- summary.info[,"estimate"]
    
    alpha <- summary.info["intrcpt","estimate"]
    se.alpha <- summary.info["intrcpt","se"]
    
    beta <- summary.info["rank","estimate"]
    se.beta <- summary.info["rank","se"]
    
    vcov <- vcov.rma(model2)
    q.val <- qnorm(0.975)
    
    # predicted values at bottom and top of rank
    p1 <- exp(alpha + beta) / (1 + exp(alpha + beta))
    p0 <- exp(alpha) / (1 + exp(alpha))
    
  if(positive){
    
    KMI <- p1/p0
    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
  }
  
  if(!positive){
    
    KMI <- p0/p1
    KMIse <-  deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) / (exp(x1) / (1+exp(x1)))), 
                          coefs, vcov)
    
  }
  
    inequal.kmi <- KMI
    se.formula <- KMIse
    if(SEuseful){
      ci <- conf.int.norm(inequal.kmi, se.formula)
    }
    
    
  }
  
  
  
  


  return(list(inequal.rii=inequal.kmi, se.rii.boot=se.boot,  se.rii.formula=se.formula, se.lowerci.rii = ci$l, se.upperci.rii = ci$u))  # return a list of the inequality measure and the standard error 
}





