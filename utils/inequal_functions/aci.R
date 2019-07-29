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

######### Absolute Concentration Index (ACI)
# Is a measures of the covariance between social rank and health. It measures the extent to which health/illness 
# is concentrated among groups on the absolute scale.  It may only be used with groups that have a natural ordering (p.9);
# e.g., income quintiles, levels of education, etc.
#
# Reference: Handbook on Health Inequality Monitoring, WHO (2013)
#########

# Absolute Concentration Index (ACI)

aci <- function(dat, bs=FALSE, forBS = NULL, bsiter = 200, simulated_results = NULL){ 
  # absolute, complex, ordered, weighted, greater than two, not logscale
  # wealth, educ
  # TEST WITH: dat <- disagdata.split[[5]][[1]]
  # dat <- strata
  # TODO, need to figure out order and reference groups
  # dat <- disagdata.split[[234]]
  dat <- arrange(dat, subgroup_order)
  
  if(!is.null(forBS)){
    dat$estimate <- forBS
  }
  
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                          two_subgroups,
                          not_ordered,
                          missing_estimates,
                          missing_population,
                          estimates_all_zero,
                         less_than2_subgroups)
  
  if(any(badData)) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  # I'm replacing wrap.aci
  mid.point <- midPointProp(pop) 
  inequal.aci <- sum(popsh * (2*mid.point - 1) * est)
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  

  if(SEuseful){
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    se.formula <- sqrt(sum((popsh^2)*(2*midPointProp(popsh)-1)^2*se^2))
    ci <- conf.int.norm(inequal.aci, se.formula)
  }
  
  
  
  
  
  return(list(inequal.aci=inequal.aci, se.aci.boot=se.boot, se.aci.formula=se.formula, se.lowerci.aci = ci$l, se.upperci.aci = ci$u))
}
