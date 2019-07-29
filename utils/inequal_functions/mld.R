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

######### Mean Log Deviation (MLD)
# Mean Log Deviation is a measures of general disproportionality, developed 
# by the economist Henri Theil (10)
# 
#########

mld <- function(dat, bs=FALSE, bsiter = 200, simulated_results = NULL){
  #relative, complex, non ordered, weighted, greater than two
  # region
  # TEST WITH dat <- disagdata.split[[4]][[1]]
  
  # TODO, need to figure out order and reference groups
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
                         #natl_all_zero,
                         less_than2_subgroups)
  
  
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  if(any(badData) || is.na(est_natl)) return(na_return)
  
  inequal.mld <- sum(popsh * -log(est/est_natl))
  
  se.formula <- NA
  se.boot <- NA
  ci <- list(l = NA, u = NA)
  
  if(SEuseful){
    
    # The SE of MLD
    
    # The xls formula for the se component of each group's estimate:
    # =(((H2^2)*(Y2^2))/(SUMPRODUCT(G2:G6,Y2:Y6)^2))*((1-(1/CF2))^2)
    wgt.mean <- weighted.mean(est, popsh) # Calculate the weighted mean, because it is re-used
    
    # Element 1: ((H2^2)*(Y2^2))
    #pop.prop <- w/sum(w) # Y is the proportion weight of each group
    el1 <- se^2 * popsh^2 # H is the vector of se's
    
    # Element 2: (SUMPRODUCT(G2:G6,Y2:Y6)^2))  # The sumproduct is the weighted mean
    el2 <- (wgt.mean)^2  # square of the weighted mean
    # Element 3: ((1-(1/CF2))^2)  # CF2 is the ratio of each x/weighted.mean(x)
    rj <- est/wgt.mean
    el3 <- (1-(1/rj))^2
    # Element 4: combine elements 1..3 according to the xls formula
    el4 <- (el1/el2)*el3
    # Return the combined se elements
    se.formula <- sqrt(sum(el4))   
    
    
    if(is.nan(se.formula)){
      se.formula <- NA
    }
    ci <- conf.int.norm(inequal.mld, se.formula)
  }
  
  
  
  # Return the results as a list
  return(list(inequal.mld=inequal.mld, se.mld.boot=se.boot,  se.mld.formula=se.formula, se.lowerci.mld = ci$l, se.upperci.mld = ci$u))  # return a list of the inequality measure and the standard error 
  
}
