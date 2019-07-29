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

######### Rate Difference (RD)
# The rate difference is used to measure the extent to which one group is better (or worse) off than another group.  It is 
# implemented here in a way that ignores an a priori ordering and selects the two groups (from 2 plus groups) that have
# the best and worst outcomes.
#
# Reference: Ontario Agency for Health Protection and Promotion (Public Health Ontario). Summary measures of
#            socioeconomic inequalities in health. Toronto, ON: Queen’s Printer for Ontario; 2013. (p.17)
#########



d <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  # Absolute, simple, non ordered, unweighted, two groups, not logscale
  # wealth, educ, area, sex, region
  # TEST WITH: dat <- disagdata.split[[3]][[1]] # for education
  # TEST WITH: dat <- disagdata.split[[3722]][[1]] # for sex
  
  
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
  
  
  inequal.d <-  rmax - rmin
  
  semin <- ranked_vals$se[ranked_vals$type == "min"]
  semax <- ranked_vals$se[ranked_vals$type == "max"]
  
  
  
  se.formula <- NA
  se.boot    <- NA
  
  # note there is no usefulSE test see git 873
  se.formula <- sqrt( semax^2 + semin^2 ) 
  
  ci <- conf.int.norm(inequal.d, se.formula)
  
  # Return the results as a list
  return(list(inequal.d=inequal.d, se.d.boot=se.boot, se.d.formula=se.formula, se.lowerci.d = ci$l, se.upperci.d = ci$u))
}



