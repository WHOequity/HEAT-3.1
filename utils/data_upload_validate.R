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





uploaded_data_adjustments <- function(dat){
  
  # strip any rows that are entirely NA
  dat <- strip_all_NA_rows(dat)
  
  # see if any of these fields have any non-digits
  
  # Convert anything that is supposed to be a number to a number
  # TODO: right now this is hard-coded, perhaps add as global?
  
  integerVars <- c("year", "favourable_indicator", "ordered_dimension",
                   "subgroup_order", "reference_subgroup")  
  dat[,integerVars] <- suppressWarnings(lapply(dat[,integerVars], as.integer))
  
  numericVars <- c("estimate", "population", "se", "ci_lb", "ci_ub","national", "indicator_scale")
  dat[,numericVars] <- suppressWarnings(lapply(dat[,numericVars], as.numeric))
  
  charVars <- c("country", "source", "indicator_abbr", "indicator_name", "dimension",
                "subgroup", "flag", "iso3")
  dat[,charVars] <- suppressWarnings(lapply(dat[,charVars], trimws))
  
  dat
  


  
}

stop_invalid_data <- function(key) {
  stop(
    structure(
      class = c("invalid_data", "error", "condition"),
      list(key = key, message = "uploaded data is invalid", call = NULL)
    )
  )
}

get_strata <- function(dat) {
  split(dat, paste(dat$country, dat$year, dat$indicator_abbr, dat$dimension))
}

# 1 Missing variables ----
missing_required_variables <- function(dat) {
  reqname <- c(
    "country", "year", "source", "indicator_abbr", "indicator_name",
    "indicator_scale", "dimension", "subgroup", "estimate", "se", "ci_lb",
    "ci_ub", "population", "flag", "national", "iso3", "favourable_indicator",
    "ordered_dimension", "subgroup_order", "reference_subgroup"
  )
  
  missing_req <- reqname[!(reqname %in% names(dat))]
  missing_req <- gsub("country", "setting", missing_req)
  missing_req <- gsub("national", "setting_average", missing_req)

  if (length(missing_req) > 0) {
    stop_invalid_data("warnmissingvars")
  }
}

# 2 Missing values for mandatory variables (except "estimate") ----
missing_one_val <- function(dat){
  vars <- c(
    "country", "year", "source", "indicator_abbr", "indicator_name", 
    "dimension", "subgroup", "favourable_indicator", "indicator_scale", 
    "ordered_dimension", "subgroup_order", "reference_subgroup"
  )
  
  has_missing_values <- vapply(
    dat[, vars], 
    function(col) any(is.na(col) | col == ""),
    logical(1)
  )
  
  if (any(has_missing_values)) {
    stop_invalid_data("warnmissingvals")
  }
}

# 3 Missing values for mandatory variable "estimate" ----
empty_key_variables <- function(dat) {
  if (all(is.na(as.numeric(dat$estimate)))) {
    stop_invalid_data("warnmissingestimates")
  }
}

# 4 String variables are numeric ----
vars_not_char <- function(dat) {
  vars <- c(
    "country", "source", "indicator_abbr", "indicator_name", "dimension",
    "subgroup", "flag", "iso3"
  )
  
  # here we test if all characters are digits or if we have digit period digit
  are_numerics <- vapply(
    dat[, vars], 
    function(x) any(grepl("^\\d+(\\.\\d+)?$", x)),
    logical(1)
  )

  if (any(are_numerics)) {
    stop_invalid_data("warncharsvars")
  }
}

# 5 iso3 not 3 characters long ----
iso_not_3_chars <- function(dat) {
  
  not_3_chars <- vapply(
    get_strata(dat),
    function(s) any(ifelse(is.na(s$iso3), FALSE, nchar(s$iso3) != 3)),
    logical(1)
  )

  if (any(not_3_chars)) {
    stop_invalid_data("warnisovars")
  }
}

# 6 Numeric variables are text  ----
vars_not_nums <- function(dat) {
  vars <- c(
    "year", "estimate", "population", "se", "ci_lb", "ci_ub", "national", 
    "favourable_indicator", "indicator_scale", "ordered_dimension",
    "subgroup_order", "reference_subgroup"
  )
  
  not_nums <- vapply(
    dat[, vars],
    function(col) {
      if (all(is.na(col))) {
        return(FALSE)
      }
      
      any(!is.na(col) & is.na(suppressWarnings(as.double(col))))
      # below did cover exponential notation
      # !grepl("^(\\d+\\.?|\\.?\\d+|\\d+\\.\\d+)$", col)))
    },
    logical(1)
  )
  
  if (any(not_nums)) {
    stop_invalid_data("warnnumsvars")
  }
}

# 7 Year is not 4 digits long ----
year_test <- function(dat) {
  col <- dat[["year"]]
  
  invalid_years <- is.na(col) | !grepl("^\\d{4}$", col)
  
  if (any(invalid_years)) {
    stop_invalid_data("warnyearvars")
  }
}

# 8 Numeric variables are not integers ----
not_discrete <- function(dat) {
  vars <- c(
    "year", "favourable_indicator", "indicator_scale", "ordered_dimension", 
    "subgroup_order", "reference_subgroup"
  )
  
  not_integers <- vapply(
    dat[, vars], 
    function(x) {
      any(as.numeric(x) %% 1 != 0)
    },
    logical(1)
  )
  
  if (any(not_integers)) {
    stop_invalid_data("warnintegervars")
  }
}

# 9 Numeric variables are negative ----
has_negative <- function(dat) {
  vars <- c(
    "year", "estimate", "se", "ci_lb", "ci_ub", "population", "national",
    "favourable_indicator", "indicator_scale", "ordered_dimension", 
    "subgroup_order","reference_subgroup"
  )
  
  are_negative <- vapply(
    dat[, vars],
    function(col) any(col < 0, na.rm = TRUE),
    logical(1)
  )
  
  if (any(are_negative)) {
    stop_invalid_data("warnnegativevars")
  }
}

# 10 Population is 0 ----
population_not_zero <- function(dat) {
  invalid_populations <- !is.na(dat$population) & dat$population < 0

  if (any(invalid_populations)) {
    stop_invalid_data("warnpopulationvars")
  }
}

# 11 Favourable indicator is not 0 or 1----
fav_indicator_is_binary <- function(dat) {
  col <- dat[["favourable_indicator"]]
  
  not_0_or_1 <- !grepl("^[01]$", col)
  
  if (any(not_0_or_1)) {
    stop_invalid_data("warnfavbinary")
  }
}

# 12 Ordered dimension is not 0 or 1 ----
ordered_dim_is_binary <- function(dat) {
  col <- dat[["ordered_dimension"]]
  
  not_0_or_1 <- !grepl("^[01]$", col)
  
  if (any(not_0_or_1)) {
    stop_invalid_data("warnorddimbinary")
  }
}

# 13 Subgroup order is not 0 when ordered_dimension = 0 OR subgroup_order is 0 when ordered_dimension = 1 -----
subgroup_matches_ordered <- function(dat) {
  
  # if "it" is NOT an ordered dimension then the subgroup_order needs to be 0.
  # Otherwise the subgroup_order needs to be NOT 0
  
  incorrect_order <- vapply(
    get_strata(dat),
    function(s) {
      if (all(s$ordered_dimension == 0)) {
        any(s$subgroup_order != 0)
      } else {
        any(s$subgroup_order == 0)
      }
    },
    logical(1)
  )
  
  if (any(incorrect_order)) {
    stop_invalid_data("warnsubgrpvars")
  }
}


# 14 Subgroup order is an increasing sequence when ordered_dimension = 1 ----
subgroup_is_sequence <- function(dat) {
  # if an ordered dimension then the sequence must be 1:nrow(strata)
  
  incorrect_sequence <- vapply(
    get_strata(dat),
    function(s) {
      if (all(s$ordered_dimension == 0)) {
        return(FALSE)
      }
      
      !identical(seq_len(NROW(s)), as.integer(sort(s$subgroup_order)))
    },
    logical(1)
  )
  
  if (any(incorrect_sequence)) {
    stop_invalid_data("warngrpsequence")
  }
}

# 15 Reference subgroup is not 0 or 1 ----
reference_is_binary <- function(dat) {
  col <- dat[["reference_subgroup"]]
  
  is_not_binary <- !grepl("^[10]$", col)
  
  if (any(is_not_binary)) {
    stop_invalid_data("warnrefbinary")
  }
}


# 16 Reference subgroup is not 0 when ordered dimension is 1 ----
reference_matches_ordered <- function(dat) {
  # if they give a reference group AND an ordered dimension
  
  incorrect_subgroup <- vapply(
    get_strata(dat),
    function(s) {
      all(s$ordered_dimension == 1) & 
        any(s$reference_subgroup == 1, na.rm = TRUE)
    },
    logical(1)
  )
  
  if (any(incorrect_subgroup)) {
    stop_invalid_data("warnrefgrpvars")
  }
}

# 17 Reference subgroup must have only one subgroup with 1 when ordered dimension is 0 ----
reference_has_single_1 <- function(dat) {
  has_multiple_1s <- vapply(
    get_strata(dat),
    function(s) sum(s$reference_subgroup, na.rm = TRUE) > 1,
    logical(1)
  )
  
  if (any(has_multiple_1s)) {
    stop_invalid_data("warnrefmultones")
  }
}

# 18 Each country and iso3 appears exactly once for a respective iso3 or country ----
country_iso3_nomatch <- function(dat) {
  vars <- c("country", "iso3")
  
  # bijection here
  duplicates <- vapply(
    unique(dat[, vars]),
    function(col) anyDuplicated(col[!is.na(col)]) > 0,
    logical(1)
  )
  
  if (any(duplicates)) {
    stop_invalid_data("warniso3dups")
  }
}

# 19.01 Each indicator abbreviation should have a unique indicator name ----
indicator_abbr_unique_names <- function(dat) {
  # this needs to be run before 19 as the problem appears to be a subset of
  # those identified by 19
  
  too_many_names <- dat %>% 
    select(indicator_abbr, indicator_name) %>% 
    group_by(indicator_abbr) %>% 
    summarise(
      more_than_1 = n_distinct(indicator_name) != 1
    ) %>% 
    pull() %>% 
    any()
  
  too_many_abbreviations <- dat %>% 
    select(indicator_name, indicator_abbr) %>% 
    group_by(indicator_name) %>% 
    summarise(
      more_than_1 = n_distinct(indicator_abbr) != 1
    ) %>% 
    pull() %>% 
    any()
  
  if (too_many_names || too_many_abbreviations) {
    stop_invalid_data("warnabbrnamemismatch")
  }
}

# 19 Observations have the same value when grouped by setting, year, source, indicator ----
observations_when_grouped <- function(dat) {
  vars <- c(
    "country", "year", "source", "indicator_abbr", "indicator_name",
    "national", "favourable_indicator", "indicator_scale"
  )
  
  duplicates_in_combos <- dat %>% 
    select(!!vars) %>% 
    distinct() %>% 
    add_count(country, year, source, indicator_name) %>% 
    add_count(country, year, source, indicator_abbr) %>% 
    select(n, nn) %>% 
    vapply(function(col) any(col > 1), logical(1))
  
  if (any(duplicates_in_combos)) {
    stop_invalid_data("warndupobscombo")    
  }
}

# 20 Ordered dimension must be the same for combinations of setting, year, source, indicator, dimension ----
ordered_dim_when_grouped <- function(dat) {
  vars <- c(
    "country", "year", "source", "indicator_abbr", "dimension",
    "ordered_dimension"
  )
  
  duplicates_in_combos <- dat %>% 
    select(!!vars) %>% 
    distinct() %>% 
    add_count(country, year, source, indicator_abbr, dimension) %>% 
    mutate(n = n > 1) %>% 
    pull()
  
  if (any(duplicates_in_combos)) {
    stop_invalid_data("warndupordcombo")
  }
}



# NO NUMBER
# vars_not_all_numbers <- function(dat){
#   res <- list(result = FALSE, msg = NA)
#   myunique <- unique(dat[,c("country", "year", "source",
#                             "indicator_abbr", 
#                             "dimension", "ordered_dimension")])
#   thetest <- any(count(myunique, country, year, source, indicator_abbr, dimension)$n>1)
#   
#   if(thetest){
#     res$result <- TRUE
#     res$msg <- tags$div(HTML("<span class = 'msg_upload'>Data not uploaded</span><br><span>Within a country, year, source, indicator and dimension the ordered_dimension values need to be the same</span>"))
#   }
#   res
# }

# NO NUMBER, git 810 ----
scale_has_zero_or_NA <- function(dat) {
  if (any(dat$indicator_scale == 0 | is.na(dat$indicator_scale))) {
    stop_invalid_data("warnindscale")
  }
}

# helper functions ----
strip_all_NA_rows <- function(dat){

  # dat <- dat2
  dat[dat==""] <- NA
  nas <- rowSums(is.na(dat))
  bad <- which(nas == ncol(dat))
  if(length(bad)>1) dat <- dat[-bad,]
  dat

}


proper_length <- function(var, l){
  n <- unique(nchar(var)) 
  if(length(n)!=1) return(FALSE)
  n == l
}






all_there <- function(var){
  length(var) == sum(!is.na(var))

}

is_numeric <- function(var, test_all = FALSE, test_notneg = FALSE){
  #var <- dat$national
  var <- suppressWarnings(as.numeric(var))
  if(all(is.na(var))) return(FALSE)
  if(test_all && !all_there(var)) return(FALSE)
  # !! careful, I apply this to the version with no NA
  if(test_notneg && any(var[!is.na(var)]<0)) return(FALSE)

  TRUE
}



