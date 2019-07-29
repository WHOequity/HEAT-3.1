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

######### Rate Ratio (RR)
# The rate Ratio is used to measure the extent to which one group is relatively better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########


r <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  # Absolute, simple, non ordered, unweighted, two groups, not logscale
  # wealth, educ, area, sex, region
  # TEST WITH: dat <- disagdata.split[[2]][[1]]
  
  
  
  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  
  badData <- apply_tests(dat, d_r_fail)
  
  if(badData) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  

  if(!rankable) ascending <- TRUE
  if(rankable) ascending <- positive
  
 
  ranked_vals <- get_est_se_min_max(dat, userank = rankable, ascending = ascending,
                                    userefgroup = hasrefgroup)
  
  rmin <- ranked_vals$estimate[ranked_vals$type == "min"]
  rmax <- ranked_vals$estimate[ranked_vals$type == "max"]
  semin <- ranked_vals$se[ranked_vals$type=="min"]
  semax <- ranked_vals$se[ranked_vals$type=="max"]
  
  inequal.r <-  rmax/rmin

  
  se.formula <- NA
  se.boot <- NA
  
  # note there is no usefulSE test see git 873
  se.formula <- sqrt((1/(rmin^2))*((semax^2)+(inequal.r^2)*((semin^2))))
  
  ci <- conf.int.norm(inequal.r, se.formula)
  
  if(is.infinite(inequal.r)){
    inequal.r <- NA
    se.formula <- NA
    se.boot <- NA
    ci$l <- NA
    ci$u <- NA
  }
  
  #  Return the results as a list
  return(list(inequal.r=inequal.r, se.r.boot=se.boot,  se.r.formula=se.formula, se.lowerci.r = ci$l, se.upperci.r = ci$u))  # return a list of the inequality measure and the standard error 
  
}
