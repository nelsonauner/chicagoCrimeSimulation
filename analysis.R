library(reshape2)
library(lubridate)
library(dplyr)

bigd <- read.csv("~/Home/CrimeData/Crimes_-_2001_to_present.csv",stringsAsFactors=FALSE)
#yeah that's big
str(bigd)
head(bigd)
bigd$Primary.Type %>% unique



violentCrime <- bigd %>% 
  filter(Primary.Type %in% c("ASSAULT","BATTERY","HOMICIDE"), Domestic=="false") %>%
  mutate(day = Date %>% parse_date_time(orders="mdyhms") %>% as.Date) %>%
  select(day,Primary.Type,Description,Location.Description,District,Ward,Latitude,Longitude)


homocides <- violentCrime %>%
  filter(Primary.Type %in% c("HOMICIDE"),
         day %within% union(sixMonthPre,treatPeriod)) %>%
  mutate(month = floor_date(day,"month"),
         District = as.factor(District)) %>%
  group_by(month,District) %>%
  summarise(hom = n())

full <- expand.grid(month = unique(homocides$month),District = as.factor(unique(allDistricts)))

complete <- left_join(full,homocides)

complete$hom[is.na(complete$hom)] <- 0

save(complete,file="completeHomicides.RData")


complete$Period[complete$month %within% treatPeriod] <- "treat"
complete$Period[is.na(complete$Period)] <- "control"

complete <- 
  complete %>%
  mutate(VRI = (District %in% c(7,11)))

initialPass <- lm(hom ~ VRI *  Period,data=complete)

hist(quickResults,main="Histogram of diff-n-diff coefficient under resampling",xlab="coefficient",xaxt="n")
axis(side=1,seq(from=-2.5,to=1.5,by=.5))

length(quickResults)
sort(quickResults)[25]

#robust standard errors:
require("sandwich")
require("lmtest")
initialPass$newse<-vcovHC(initialPass)
coeftest(initialPass,initialPass$newse)

# and now, a half-ass resampling method :/ 
quickWrapper <- function(complete) {
  print(toTreat <- complete$District %>% unique %>% sample(2))
  complete$VRI = FALSE
  complete$VRI[complete$District %in% toTreat] = TRUE
  quickReg <- lm(hom ~ VRI * Period, data=complete)
  return(tail(coef(quickReg),1))
}

quickResults <- replicate(10000,quickWrapper(complete))

hist(quickResults,main="Histogram of diff-n-diff coefficient under resampling",xlab="coefficient")

quickdf <- as.data.frame(quickResults)
p<- 
  ggplot(data=quickResults %>% as.data.frame) +
  geom_histogram(fill="grey", binwidth = max(quickResults) / 60) +
  
  geom_histogram(data=subset(quickRe, coef > quantile(coef, .975)), fill="red", alpha=.5, binwidth=max(rand.m5.all.d[[c]]$coef) / 60) +
  geom_histogram(data=subset(rand.m5.all.d[[c]], coef < quantile(coef, .025)), fill="red", alpha=.5, binwidth=max(rand.m5.all.d[[c]]$coef) / 60) +
  
  
  
  
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

quickRes <-
  placeboWrapper(violentCrimeCollapsed,
                 2,
                 0,
                 40)