#' Age-specific Health-related quality of life (HRQOL).
#'
#' A dataset containing HRQOL for 20 age groups and both genders.
#' The age group includes: 0~1, 1~4, and 5-year interval groups, all the way up to 90+.
#' The data comes from http://core.apheo.ca/index.php?pid=223, which is one type of
#' mean Health Utilities Index (mHUI).
#' This HRQOL data represents such an index in the Ontario population, for 1996-1997.
#' 
#'
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{AgeGroup}{20 age groups, starting from 0~1 and 1~4, then abridged in 5 year interval}
#'   \item{Male}{Male HRQOL}
#'   \item{Female}{Female HRQOL}
#'   
#' }
#' @source \url{http://core.apheo.ca/index.php?pid=223/}
"HRQOL"