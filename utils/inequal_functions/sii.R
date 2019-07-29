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

######### Slope Index of Inequality (SII)


sii <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  
  
  # relative, complex, ordered, weighted, greater than two
  # wealth, educ
  # TEST WITH: dat <- disagdata.split[[1]]
  
  dat$subgroup.new <- (dat$favourable_indicator-0.5) * dat$subgroup_order
  dat <- dat[order(dat$country, dat$year, dat$indicator_abbr, dat$dimension, dat$subgroup.new),]
  
  #dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]])
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         not_ordered,
                         missing_estimates,
                         missing_population,
                         estimates_all_zero,
                         se_all_missing,
                         has_zero_se,
                         less_than2_subgroups)
  
  if(any(badData) || !SEuseful) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  est <- round((est/scaleval) * pop)
  pop <- round(pop)
  
  # Compute the ranks
  rank <- midPointProp(pop)
  
  # Set up new data with the y/pop as y
  newdat <- data.frame(rank=rank, est=est, pop = pop)
  
  # there must be a better way than this. Looks like it computes the
  # model twice, but we want to know if there's a warning
  
  res <- try(rma.glmm(measure="PLO", mods = ~rank, xi=est, ni=pop, method="FE",data=newdat), TRUE)
  
  warn <- any(class(res) == "try-error")
  
  
  inequal.sii <- NA
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
      
      SII <- scaleval * (p1 - p0)  
      
      SIIse <- scaleval * deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                                      coefs, vcov)
    }
    
    if(!positive){
      SII <- scaleval * (p0 - p1) 
      SIIse <- scaleval * deltamethod(~ (exp(x1+x2) / (1+exp(x1+x2)) - (exp(x1) / (1+exp(x1)))), 
                                      coefs, vcov)
    }
    
    
    inequal.sii <- SII
    se.formula <- SIIse
    if(SEuseful){
      ci <- conf.int.norm(inequal.sii, se.formula)
    }
  }
  
  
  
  
  
  
  
  
  # Return the results as a list
  return(list(inequal.sii=inequal.sii, se.sii.boot=se.boot,  se.sii.formula=se.formula, se.lowerci.sii = ci$l, se.upperci.sii = ci$u))  # return a list of the inequality measure and the standard error 
}
