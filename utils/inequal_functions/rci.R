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

######### Relative Concentration Index (RCI)
# Is a measures of the covariance between social rank and health and measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference:
#########


rci <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){ 
  # relative, complex, ordered, weighted, greater than two
  # wealth, educ
  # TEST WITH: dat <- disagdata.split[[1]][[1]]
  
  dat <- arrange(dat, subgroup_order)
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
                         less_than2_subgroups)
  

  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  if(any(badData) || is.na(est_natl)) return(na_return)
  
  inequal.aci <- aci(dat, bs=F)$inequal.aci  # ACI
  inequal.rci <- inequal.aci / est_natl
  
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    # Each groups proportion of the population
    mid.point <- midPointProp(popsh)  # cumulative mid point proportions ... Rank in the parlance of Ahmad Hosseinpoor
    sumprodsqrd <- sum(popsh * est)^2
    
    s2_s6 <- (popsh^2)*((2*mid.point-1)-inequal.rci)^2*se^2
    se.formula <- sqrt( sum(s2_s6) / sumprodsqrd )
    
    
    if(bs){
      n <- bsiter
      inequal.boot.vals <- sapply(1:n, function(x){ 
        estrand <- rnorm(length(est), est, se) 
        inequal.aci <- aci(dat, bs=F, forBS = estrand)$inequal.aci  # ACI
        inequal.aci / est_natl
      })
      se.boot <- sd(inequal.boot.vals)
    }
    
    ci <- conf.int.norm(inequal.rci, se.formula)
  }
  

  
  return(list(inequal.rci=inequal.rci, se.rci.boot=se.boot, se.rci.formula=se.formula, se.lowerci.rci = ci$l, se.upperci.rci = ci$u))
}


