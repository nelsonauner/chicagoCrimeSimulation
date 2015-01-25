# Abstracted functions for more customizeable crime simulation
# Still need some work

treatAsVRI <- function(violentCrimeCollapsed,treatedDistricts,treatTime,controlTime) {
    # input: 
    #       violentCrimeCollapsed: collapsed dataset with crime counts
    #       treatedDistricts:     the districts to treat (vector of numbers),
    #       treatTime:            time Period interval() lubridate 
    #       controlTime:          time Period (as above) to treat as the control period
    #treatment dates are feb 2012 through August 2012, and pre-period 6 months before that
    treatPeriod <- interval(ymd(20120201), ymd(20120831))
    sixMonthPre <- int_shift(treatPeriod,-as.duration(treatPeriod))
    #treatedDistricts <- c(7,11)
    # --> end parameters
    violentCrimeCollapsed$treatDistrict = violentCrimeCollapsed$District %in% treatedDistricts
    violentCrimeCollapsed$Period = NA 
    violentCrimeCollapsed$Period[violentCrimeCollapsed$day %within% treatPeriod] <- "treat"
    violentCrimeCollapsed$Period[violentCrimeCollapsed$day %within% sixMonthPre] <- "control"
    primt(table(violentCrimeCollapsed$Period))
    return(violentCrimeCollapsed)
}

crimeddiff <- function(violentCrimeCollapsed) {
    # input: a collapsed dataset with day, count, Period (= treat, control) and treatDistrict
    # output: coeffecient on Period= treat and treatDistrictTRUE
    #standard differences in differences: 
    diffTable <-
        violentCrimeCollapsed %>%
    filter(!is.na(Period)) %>%
    group_by(Period,treatDistrict) %>%
    summarise(totalCount = log(sum(count)))
    # what is the diff-n-diff estimator? 
    with(diffTable,
         lm(totalCount ~ treatDistrict*Period)) %>%
    coefficients %>% `[`(4)
}


placeboWrapper <- function(violentCrimeCollapsed,
                           numDistrictsToTreat,
                           intervalPeriod,
                           iterNumber                         
                           )
{
    #input: 
    #output: a data-frame of coeffecients
    allDistricts <- violentCrimeCollapsed[["District"]] %>% unique
    replicate(n = iterNumber, expr = {
              violentCrimeCollapsed %>%
              treatAsVRI(
                         treatedDistricts = allDistricts %>% sample(numDistrictsToTreat),
                         #let's use defaults:
                         treatTime <- interval(ymd(20120201), ymd(20120831)),
                         controlTime <- int_shift(treatPeriod,-as.duration(treatPeriod))
                         ) %>%
              crimeddiff

                           }

    )
}

quickRes <-
    placeboWrapper(violentCrimeCollapsed,
                   2,
                   0,
                   40)

treatAsVRI <- function(violentCrimeCollapsed,treatedDistricts,treatTime,controlTime) {
    # input: 
    #       violentCrimeCollapsed: collapsed dataset with crime counts
    #       treatedDistricts:     the districts to treat (vector of numbers),
    #       treatTime:            time Period interval() lubridate 
    #       controlTime:          time Period (as above) to treat as the control period
    #treatment dates are feb 2012 through August 2012, and pre-period 6 months before that
    treatPeriod <- interval(ymd(20120201), ymd(20120831))
    sixMonthPre <- int_shift(treatPeriod,-as.duration(treatPeriod))
    #treatedDistricts <- c(7,11)
    # --> end parameters
    violentCrimeCollapsed$treatDistrict = violentCrimeCollapsed$District %in% treatedDistricts
    violentCrimeCollapsed$Period = NA 
    violentCrimeCollapsed$Period[violentCrimeCollapsed$day %within% treatPeriod] <- "treat"
    violentCrimeCollapsed$Period[violentCrimeCollapsed$day %within% sixMonthPre] <- "control"
    print(table(violentCrimeCollapsed$Period))
    return(violentCrimeCollapsed)
}

crimeddiff <- function(violentCrimeCollapsed) {
    # input: a collapsed dataset with day, count, Period (= treat, control) and treatDistrict
    # output: coeffecient on Period= treat and treatDistrictTRUE
    #standard differences in differences: 
    diffTable <-
        violentCrimeCollapsed %>%
    filter(!is.na(Period)) %>%
    group_by(Period,treatDistrict) %>%
    summarise(totalCount = log(sum(count)))
    # what is the diff-n-diff estimator? 
    with(diffTable,
         lm(totalCount ~ treatDistrict*Period)) %>%
    coefficients %>% `[`(4)
}


placeboWrapper <- function(violentCrimeCollapsed,
                           numDistrictsToTreat,
                           intervalPeriod,
                           iterNumber                         
                           )
{
    #input: 
    #output: a data-frame of coeffecients
    allDistricts <- violentCrimeCollapsed[["District"]] %>% unique
    replicate(n = iterNumber, expr = {
              violentCrimeCollapsed %>%
              treatAsVRI(
                         treatedDistricts = allDistricts %>% sample(numDistrictsToTreat),
                         #let's use defaults:
                         treatTime <- interval(ymd(20120201), ymd(20120831)),
                         controlTime <- int_shift(treatPeriod,-as.duration(treatPeriod))
                         ) %>%
              crimeddiff

                           }

    )
}

