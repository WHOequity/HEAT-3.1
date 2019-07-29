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


# The purpose of this file is to create the functions used in tests
# in inequality functions. These include tests for whether the
# dimension is "sex" or whether the data is ordered. For the most
# part these functions take, as input, the full dataset for a strata
# rather than just the piece or two needed.

# library(readxl)
# test <- read_excel("X:/projects/who_heat/data/original_data/20160630_upload_examples/Database example - 2016-06-02.xlsx")
# test <- test[1:26,]
# splitter <- paste(test$country, test$year, test$source, test$indicator_abbr, test$indic_category,
#                   test$dimension)
# multistrata <- split(test, splitter)
# onestrata <- multistrata[[1]]
# strata <- onestrata

#******************************************************************************
# this function will allow us to apply multiple tests
# to a single data.frame so that we can  do, say,
# multi.sapply(strata, two_subgroups, is_ordered)
# from http://rsnippets.blogspot.com/2011/11/applying-multiple-functions-to-data.html
#******************************************************************************

apply_tests <- function(...) {
  #browser()
      arglist <- match.call(expand.dots = FALSE)$...
      var.names <- sapply(arglist, deparse)
      has.name <- (names(arglist) != "")
      var.names[has.name] <- names(arglist)[has.name]
      arglist <- lapply(arglist, eval.parent, n = 2)
      x <- arglist[[1]]
      arglist[[1]] <- NULL
      # I made a change to this line
      result <- sapply(arglist, function(FUN, x) 
        do.call(FUN, list(strata=x)), x)
      names(result) <- var.names[-1]
      return(result)
}


#******************************************************************************
# If one of the test in an inequality function fails then we return
# all NA values. This function creates the named list of NA vals
#******************************************************************************

inequal_na_return <- function(funcname){
  str <- sprintf("list(inequal.%1$s=NA, se.%1$s.boot=NA,  se.%1$s.formula=NA, se.lowerci.%1$s=NA, se.upperci.%1$s=NA)", funcname)
  eval(parse(text=str))
}


#******************************************************************************
# Determine if the SE can be used to compute formula/boot SE
#******************************************************************************


usefulSE <- function(strata){

  # Not useful if any are NA
  anyNA <- any(is.na(strata$se))

  "se"%in%names(strata) && !anyNA
}



#******************************************************************************
# Estimates all zero
#******************************************************************************

estimates_all_zero <- function(strata){
  all(strata$estimate[!is.na(strata$estimate)] == 0)
}





#******************************************************************************
# Test if dimension has 2 subgroups
#******************************************************************************
two_subgroups <- function(strata) nrow(strata) == 2


#******************************************************************************
# Test if dimension has 2 subgroups
#******************************************************************************
less_than2_subgroups <- function(strata) nrow(strata) < 2


#******************************************************************************
# Test if dimension has more than 2 subgroups
#******************************************************************************
more_than2_subgroups <- function(strata) nrow(strata) > 2

#******************************************************************************
# Test if data is ordered
#******************************************************************************
is_ordered <- function(strata) all(strata$ordered_dimension == 1)

#******************************************************************************
# test if data is not ordered
#******************************************************************************
not_ordered <- function(strata) all(strata$ordered_dimension == 0)

#******************************************************************************
# tests if any estimates are missing
#******************************************************************************
missing_estimates <- function(strata){
  # strata <- dat
  est <- strata$estimate
  sum(!is.na(est)) != nrow(strata)
}

#******************************************************************************
# Tests if it's two subgroups and one is missing
#******************************************************************************
two_subgroups_and_missing <- function(strata){
  is2 <- two_subgroups(strata)
  miss <- missing_estimates(strata)
  is2 & miss
}

#******************************************************************************
# Test if a popshare value is missing
#******************************************************************************
missing_population <- function(strata){
  pop <- strata$population
  sum(!is.na(pop)) != nrow(strata) || sum(pop, na.rm=T) == 0
}

#******************************************************************************
# Test if the national average is missing
#******************************************************************************
missing_natl_avg <- function(strata){
  unique_natl <- unique(strata$national)
  length(unique_natl)!=1 | is.na(unique_natl)
}


#******************************************************************************
# National all zero
#******************************************************************************

natl_all_zero <- function(strata){
  all(strata$national == 0 | is.na(strata$national))
}



#******************************************************************************
# Has zero se
#******************************************************************************

has_zero_se <- function(strata){
  
  if(se_all_missing(strata)) return(FALSE)
  any(strata$se[!is.na(strata$se)] == 0)
}




#******************************************************************************
# SE all missing
#******************************************************************************

se_all_missing <- function(strata){
  all(is.na(strata$se))
}



#******************************************************************************
# Not sure if this is the best approach frankly but each of the inequality
# functions requires several variables like the estimates, pop share etc
# we could do dat$est, dat$popsh over and over but this gets hard to read
# so this function assigns -- to the environment of the function that calls it -- 
# a series of variables to use
#******************************************************************************

make_pieces <- function(dat, funcname){
  assign("est", dat$estimate, envir = parent.frame())
  assign("pop", dat$population, envir = parent.frame())
  assign("scaleval", unique(dat$indicator_scale), envir = parent.frame())
  #assign("popsh", dat$population/sum(dat$population), envir = parent.frame())
  # I return an se value with NA so I can use it in d and then return NA
  assign("se", if(!is.null(dat[['se']])) c(dat$se) else rep(NA, nrow(dat)), envir = parent.frame())
  assign("ord", dat$subgroup_order,  envir = parent.frame())
  #assign("est_natl", unique(dat$national),  envir = parent.frame())
  assign("na_return", inequal_na_return(funcname),  envir = parent.frame())
  assign("SEuseful", usefulSE(dat), envir=parent.frame())
  assign("positive", is_positive(dat), envir=parent.frame())
  assign("rankable", is_ordered(dat), envir=parent.frame())
  assign("sex", is_sex(dat), envir=parent.frame())
  assign("hasrefgroup", ref_grp_exists(dat), envir=parent.frame())
}

#******************************************************************************
# Many of the calculation rely on knowing the cumnulative proportion of the population in the mid-point of each 
# ordered group.  That is the "Mid-Point Proportion".  This function returns that value.
#******************************************************************************

midPointProp <- function(w){
  # This function returns the cumulative mid point proportion of each group:
  # Usage
  # w -- a vector of numbers of the population in each group
  # returns a vector representing the cumulative mid-point proportion
  #
  if(!is.numeric(w)){
    stop('This function operates on vector of numbers')
  }
  if(all(w==0)){
    stop('The population is of size 0 in all cells')
  }
  p <- w/sum(w)  # Calculate the pop. proportion in each group 
  p.mid <- p/2   # Calculate the mid-point proportion in each group
  p.cumsum <- cumsum(p) # Calculate the cumulative proprtion
  p.mid.cumsum <- p.mid + c(0, p.cumsum)[1:length(w)]  # Calculate the cumulative mid point proportion of each group
  return(p.mid.cumsum)  # Return the answer
}



#******************************************************************************
# Does a reference group exist
#******************************************************************************
ref_grp_exists <- function(strata){
  ref_subgrp <- strata$reference_subgroup
  sum(ref_subgrp==1 & !is.na(ref_subgrp)) == 1
}

#******************************************************************************
# What's the index number of the reference group
#******************************************************************************
which_is_refgroup <- function(strata){
  if(!ref_grp_exists(strata)) stop("A reference group is required for this function")
  which(strata$reference_subgroup==1)
}


#******************************************************************************
# Get the estimate for the low and high ordered level. Note that the "min" and
# "max" can be based on the estimates themselves (userank=FALSE) or they can
# be based on the subgroup order.
#******************************************************************************
get_est_se_min_max <- function(strata, userank = FALSE, ascending = TRUE, userefgroup = FALSE){
  #strata <- dat
  if(!userank & !userefgroup){
    
    if(ascending) strata <- arrange(strata, estimate)
    if(!ascending) strata <- arrange(strata, desc(estimate))
    min.indx <- 1
    max.indx <- nrow(strata)
  }
 
  if(userank & !userefgroup){
    
    if(ascending) strata <- arrange(strata, subgroup_order)
    if(!ascending) strata <- arrange(strata, desc(subgroup_order))
    min.indx <- 1
    max.indx <- nrow(strata)
  }
  
    if(userefgroup){
      positive <- is_positive(strata)
      
      if(positive){
      max.indx <- which(strata$reference_subgroup == 1)
      # a hack because we can't choose reference for both
      vals <- strata$estimate
      vals[max.indx] <- Inf
      min.indx <- which.min(vals)
      }
      
      if(!positive){
        min.indx <- which(strata$reference_subgroup == 1)
        # a hack because we can't choose reference for both
        vals <- strata$estimate
        vals[min.indx] <- -Inf
        max.indx <- which.max(vals)
        
      }
    
  }

  if("se"%in%names(strata)){
    vals <- strata[c(min.indx, max.indx), c("estimate", "se")]
  } else {
    vals <- strata[c(min.indx, max.indx), c("estimate"), drop=FALSE]
    vals$se <- NA
  }
  vals$type <- c("min", "max")
  vals
  
}



#******************************************************************************
# Is this a positive indicator?
#******************************************************************************
is_positive <- function(strata) unique(strata$favourable_indicator) == 1


#******************************************************************************
# What's the index number of the most advantaged group
#******************************************************************************
which_most_advantaged <- function(strata){
  which.max(strata$subgroup_order)
}


#******************************************************************************
# Is the dimension sex -- perhaps I should test for dimension rather than
# subgroup but should be fine either way.
#******************************************************************************

is_sex <- function(strata){
  all(strata$subgroup%in%c("Female", "Male"))
}


#******************************************************************************
# d and r share a set of rules for pass/fail so rather than use
# a series of rules we'll use one combination rule. TRUE means fail
#******************************************************************************

# For dimensions with 2 subgroups: Return NA if any subgroup estimate is missing. 
# For ordered dimensions with >2 subgroups: Return NA if the estimate for the most-disadvantaged subgroup (min subgroup_order) or most-advantaged subgroup (max subgroup_order) are missing.
# For non-ordered dimensions with >2 subgroups: Return NA if any subgroup estimate is missing. 


d_r_fail <- function(strata){

  est <- strata$estimate
  subgroup_order <- strata$subgroup_order
  
  
  less2 <- less_than2_subgroups(strata)
  ord <- is_ordered(strata)
  two_groups <- two_subgroups(strata)
  more_groups <- more_than2_subgroups(strata)
    estimates.all.zero <- estimates_all_zero(strata)
  #natl.all.zero <- natl_all_zero(strata)
  if(estimates.all.zero) return(TRUE)
  if(less2) return(TRUE)
  #if(estimates.all.zero | natl.all.zero) return(TRUE)
  # if we have two subgroups and either is missing
  # an estimate return TRUE (fail)
  if(two_groups){
    if(any(is.na(est))) return(TRUE)
  }
  
  # if we have an ordered dimension and more than
  # two dimensions
  if(ord  & more_groups){
    eitherNA <- any(is.na(get_est_se_min_max(strata, userank = TRUE)$estimate))
    if(eitherNA) return(TRUE)
  }
  
  if(!ord & more_groups){
    anyNA <- any(is.na(est))
    if(anyNA) return(TRUE)
  }
  
  return(FALSE)
  
}


#******************************************************************************
# par, paf share a set of rules for pass/fail so rather than use
# a series of rules we'll use one combination rule. TRUE means fail
#******************************************************************************


par_paf_fail <- function(strata){
#strata <- dat
  less2 <- less_than2_subgroups(strata)
  ord <- is_ordered(strata)
  est <- strata$estimate
  ref_group_exists <- ref_grp_exists(strata)
  if(ref_group_exists) ref_group_indx <- which_is_refgroup(strata)
  est_se <- get_est_se_min_max(strata)
  est_max <- est_se$estimate[est_se$type=="max"]
  estimates.all.zero <- estimates_all_zero(strata)
  #natl.all.zero <- natl_all_zero(strata)
  
  # git845 manual computation
  popsh <- strata$population / sum(strata$population)
  est_natl <- sum(popsh * strata$estimate)
  
  if(estimates.all.zero | is.na(est_natl) | less2) return(TRUE)
  
  # If reference_subgroup is specified: 
  # Return NA if the estimate for the reference_subgroup is missing. 

  if(ref_group_exists && is.na(est[ref_group_indx])) return(TRUE)
  
  # If reference_subgroup is not specified: For ordered dimensions: 
  # Return NA if the estimate for the most-advantaged subgroup 
  # (max subgroup_order) is missing. 
  
  if(!ref_group_exists & ord){
    if(is.na(est_max)) return(TRUE)
  }
  
  
  # If reference_subgroup is not specified: 
  # For non-ordered dimensions: 
  # Return NA if any subgroup estimate is missing. 

  if(!ref_group_exists & !ord){
    if(any(is.na(est_se$estimate))) return(TRUE)
  }
  
  return(FALSE)
  
}
 









