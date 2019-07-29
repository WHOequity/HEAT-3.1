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

######### Theil Index
# http://en.wikipedia.org/wiki/Theil_index
#########

ti <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){ 
  
  
  # relative, complex, non ordered, weighted, greater than two
  # region
  # TEST WITH: dat <- disagdata.split[[4]][[1]]
  
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         is_ordered,
                         missing_estimates,
                         missing_population,
                         #missing_natl_avg,
                         estimates_all_zero,
                         #se_all_missing,
                         #has_zero_se,
                         #natl_all_zero,
                         less_than2_subgroups)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  if(any(badData) || is.na(est_natl)) return(na_return)
  
  rj <- est / est_natl
  inequal.ti <- sum(popsh * rj * log(rj))
  
  se.formula <- NA
  se.boot <- NA
  
  if(SEuseful){
    # Formula-based SE: provided by Ahmad Hosseinpoor (WHO, Geneva)
    mu <- sum(popsh * est)
    rj <- est / mu
    s_u <- popsh * rj * (1 + log(rj))
    ti.se.indiv <- ((1 + log(rj) - sum(s_u))^2)*((popsh^2)*(se^2)/(mu^2))
    se.formula <- sqrt(sum(ti.se.indiv))
  }
  
  ci <- conf.int.norm(inequal.ti, se.formula)
  
  return(list(inequal.ti=inequal.ti, se.ti.boot=se.boot, se.ti.formula=se.formula, se.lowerci.ti = ci$l, se.upperci.ti = ci$u))
}
