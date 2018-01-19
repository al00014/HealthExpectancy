#' Age-specific Health-related quality of life (HRQOL).
#'
#' @description A dataset containing HRQOL for 20 age groups and both genders.
#' 
#' @details The age group includes: 0~1, 1~4, and 5-year interval groups, all the way up to 90+. The data comes from http://core.apheo.ca/index.php?pid=223, which is one type of mean Health Utilities Index (mHUI). This HRQOL data represents such an index in the Ontario population, for 1996-1997.
#' 
#' @docType data
#' 
#' @usage data(HRQOL)
#' 
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{AgeGroup}{20 age groups, starting from 0~1 and 1~4, then abridged in 5 year interval}
#'   \item{Male}{Male HRQOL}
#'   \item{Female}{Female HRQOL}
#'   
#' }
#' @source \url{http://core.apheo.ca/index.php?pid=223/}
#' 
#' @references 
#' Chiang CL. The Life Table and Its Applications. Malabar, Florida: Robert E. Krieger Publ. Co.; 1984.
#' Manuel D, Goel V, Williams J. The derivation of life expectancy at the local level. CDIC 1998;19(2):52-6.
#' Péron Y, Strohmneger C. Demographic and Health Indicators - Presentation and Interpretation. Ottawa: Statistics Canada 82-543E; 1992.
#' Hsieh J. A general theory of life table construction and a precise abridged life table method. Biom J 1991;2:143-62.
#' 
"HRQOL"

#' Example data for Ukraine males, 1965, extracted from the Human Mortality Database.
#'
#' @description 5-year adbridged data from the HMD for Deaths (count) Population (exposure) and central death rates, death/exposure.
#'
#' @details All four columns are numeric. Mx provided is not exactly equal to D(x)/N(x) due to rounding.
#' 
#' @name UKRmales1965
#' @docType data
#' 
#' @usage data(UKRmales1965)
#' 
#' @format A data frame with 20 rows and 4 variables:
#' \describe{
#'   \item{age}{20 age groups, starting from 0~1 and 1~4, then abridged in 5 year interval, up to 90+}
#'   \item{Dx}{Male Death counts}
#'   \item{Nx}{Male population, so-called exposure population}
#'   \item{Mx}{central death rates, equal to death/exposure}
#'
#' }
#' @source \url{http://www.mortality.org/} at Human Mortality Database, as downloaded in May, 2010.
#' 
#' @references
#' Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on 10 May, 2010).
#'
NULL


#' Example disability data for Ukraine males.
#'
#' @description 5-year adbridged data prevalence rate data.
#'
#' @details All three columns are numeric.
#' 
#' @name UKR5males1965_disabilityprevalence
#' @docType data
#' 
#' @usage data(UKR5males1965_disabilityprevalence)
#' 
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{AgeGroup}{20 age groups, starting from 0~1 and 1~4, then abridged in 5 year interval, up to 90+}
#'   \item{Male}{Male prevalence rate}
#'   \item{Female}{Female prevalence rate}
#'
#' }
#' @source \url{http://www.mortality.org/} at Human Mortality Database, as downloaded in May, 2010.
#' @references
#' Actually, this data is FAKE, only for the purpose of demonstration. It originally comes from the chapter Session 5.1 Calculating DALYs and HALE, page 12.
NULL