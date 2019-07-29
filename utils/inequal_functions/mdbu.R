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

######### Mean Difference from the Best performing Subgroup (mdbu)


mdbu <- function(dat, bs=TRUE, bsiter = 200, simulated_results = NULL){
  # absolute, complex, non ordered, weighted, greater than two
  # region
  # TEST WITH dat <- disagdata.split[[4]][[1]]

  # TODO, need to figure out order and reference groups
  dat <- arrange(dat, subgroup_order)
  funcname <- as.character(match.call()[[1]]) # current function
  make_pieces(dat, funcname) # est, pop, se, ord, na_return
  estrand <- simulated_results$estrand
  
  
  badData <- apply_tests(dat, 
                         two_subgroups,
                         is_ordered,
                         missing_estimates,
                         estimates_all_zero,
                         less_than2_subgroups)
  
  if(any(badData)) return(na_return)
  
  # git845 manual computation
  popsh <- pop / sum(pop)
  est_natl <- sum(popsh * est)
  
  
  rankable <- is_ordered(dat)
  positive <- is_positive(dat)
  
  
  if(!hasrefgroup) ref_est <- ifelse(positive, max(est), min(est))
  if(hasrefgroup) ref_est <- dat$estimate[dat$reference_subgroup == 1]
  
  est1 <- est / scaleval
  se1 <- se / scaleval
  ref_est1 <- ref_est / scaleval
  
  inequal.mdbu <- sum(abs(ref_est1-est1))
  #inequal.mdbu <- sum(popsh * abs(ref_est1-est1))
  
  se.formula <- NA
  se.boot <- NA
  boot.lcl2 <- NA
  boot.ucl2 <- NA
  
 if(SEuseful){
   
    if(bs){
      inequal.boot.vals <- purrr::map_dbl(estrand, function(x){
        if ((scaleval == 100 & all(x >= 0) & all(x <= 1)) | (scaleval != 100 & all(x >= 0))) {
          # The weighted mean calculated for each simulation was used instead of the national average.
          return(scaleval * sum(abs(ref_est1 - x)))
          
        }else{
          return(NA)
        }
        

      })
      
      boot.lcl2 <- quantile(inequal.boot.vals, probs = c(0.025), na.rm = T)
      boot.ucl2 <- quantile(inequal.boot.vals, probs = c(0.975), na.rm = T)
    }

 }
  
  inequal.mdbu <- inequal.mdbu * scaleval
  # Return the results as a list
  return(list(inequal.mdbu=inequal.mdbu, se.mdbu.boot=se.boot,  se.mdbu.formula=se.formula, se.lowerci.mdbu = boot.lcl2, se.upperci.mdbu = boot.ucl2))  # return a list of the inequality measure and the standard error 
  
}
