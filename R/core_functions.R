#' HeathExpectancy function
#'
#' @description 
#' This function estimates HALE or DFLE and its uncertainty (if uncertainty_range=TRUE).
#' HALE will only be estimated when input of mHUI is HRQOL.
#' DFLE will only be estimated when input of mHUI is prevalence of disability.
#' mHUI is an important argument for this function, short for mean Health Utilities Index.
#' The function produces age-specific HALE or DFLE estimates.
#' But the default age group should be 20 groups, starting from 0~1, 1~4, and then
#' by 5 years up to 90+. So it can always be altered to suit specific needs in real data analysis.
#' 
#' @details 
#' HeathExpectancy relies heavily on LT() function from LifeTable package, developed by Tim Riffe.
#' Most of the arguments for this function can be referenced to the LifeTable::LT() function.
#' This dependent package should come with the installation of the current package, if not mannual install via the following steps:
#' # install.packages("devtools")
#' library(devtools)
#' install_github("timriffe/LifeTable", subdir = "LifeTable")

#' @param mHUI the mean Health Utilities Index, an important argument for this function, can be HRQOL or prevalence of disability.
#' @param mHUI_input a character indicator for mHUI. Accept either 'HRQOL' or 'prevalence'.
#' @param Nx numeric vector of population exposures by age.
#' @param Dx numeric vector of death counts by age.
#' @param Mx (optional) numeric vector of central death rates (assumed in the function to be the lifetable m(x)), calculated as deaths/exposure.
#' @param ages a numeric vector indicating data age group. (Not an optional argument!)
#' @param axmethod either "keyfitz", "schoen", "preston" or "midpoint". Default = "keyfitz", although this is not recommended for abridged ages. See comparisons in axEstimate examples. The user can also supply a numeric vector of a(x) values here (e.g. from a different estimation procedure or from a different population).
#' @param sex either "male" or "female" (default). It is only necessary to specify this if "preston" is the axmethod. It does not affect any other lifetable calculations.
#' @param mxsmooth logical, default = TRUE. Should the mx vector be smoothed? If TRUE and both Nx and Dx vectors are supplied (the ideal case), smoothing is done using the function Mort1Dsmooth() from Giancarlo Camarda's MortalitySmooth package. In this case, Dx values are smoothed using log(Nx) as an offset, and all other items are the function defaults. If Mx is provided instead of Nx and Dx a loess smoother is used, loess, with span set to .15 for single age data and .4 for 5-year abridged data. If these smoothing procedures are not satisfactory, the user may wish to pre-process the Mx estimate and specify mxsmooth = FALSE, or else leave it rough.
#' @param axsmooth logical, default = TRUE. Ignored if mxsmooth = TRUE. Should the a(x) values be calculated from a smoothed M(x) series? In this case, the M(x) series is smoothed within the axEstimate() function for a(x) estimation, but the smoothed M(x) function that was used is not returned. In general, it is better to smooth the M(x) function prior to putting it in this function, because the loess smoother used here has no weights or offset. If this is not possible, loess M(x) smoothing still produces more consistent and less erratic a(x) estimates. If mxsmooth = FALSE and axsmooth = TRUE, the Mx series is only smoothed for use in a(x) estimation, and does not affect any other lifetable calculations that are dependent on Mx.
#' @param radix The lifetable starting population at age 0, l(0). default = 1. Other common values are 1000 and 100000, although any value may be given.
#' @param verbose logical, default = TRUE. Should informative but possibly annoying messages be returned when the function does something that you might want to know about?
#' @param actual_death_counts Actual counts of death for each age interval, used in calculating variances of life table functions.
#' @param uncertainty_range an argument controlling the output of uncertainty range, default to TRUE.
#' @return A list of HALE or DFLE and its uncertainty (if uncertainty_range=TRUE).
#' @keywords HALE, DFLE
#' @export
#' @examples
#' Example 1: 
#' Data from Human Mortality Database, as downloaded in May, 2010. www.mortality.org.
#'
#' ########### Example for calculating HALE
#'
#' ##### without uncertainty range 
#' HeathExpectancy(mHUI=HRQOL$Male,
#'                 mHUI_input='HRQOL',
#'                 Mx =UKR5males1965_sub$Mx, 
#'                 ages =c(0,1,seq(5,90,by=5)),
#'                 axmethod = "schoen", 
#'                 sex = "male", 
#'                 mxsmooth = TRUE,
#'                 axsmooth = TRUE, 
#'                 radix = 100000, 
#'                 verbose = TRUE,
#'                 uncertainty_range=FALSE)
#'                
#'     
#' ##### with uncertainty range 
#' HeathExpectancy(mHUI=HRQOL$Male,
#'                 mHUI_input='HRQOL',
#'                 Mx =UKR5males1965_sub$Mx, 
#'                 ages =c(0,1,seq(5,90,by=5)),
#'                 axmethod = "schoen", 
#'                 sex = "male", 
#'                 mxsmooth = TRUE,
#'                 axsmooth = TRUE, 
#'                 radix = 100000, 
#'                 verbose = TRUE,
#'                 uncertainty_range=TRUE,
#'                 actual_death_counts=UKR5males1965_sub$Dx)
#'                
#' Example 2: (with uncertainty range)
#' HeathExpectancy(mHUI=Disability_prevalence$Male,
#'                 mHUI_input='prevalence',
#'                 Mx =UKR5males1965_sub$Mx, 
#'                 ages =c(0,1,seq(5,90,by=5)),
#'                 axmethod = "schoen", 
#'                 sex = "male", 
#'                 mxsmooth = TRUE,
#'                 axsmooth = TRUE, 
#'                 radix = 100000, 
#'                 verbose = TRUE,
#'                 uncertainty_range=TRUE,
#'                 actual_death_counts=UKR5males1965_sub$Dx)
#'                 
#'                                                                                   
HeathExpectancy<-function(mHUI=NULL,
                          mHUI_input=NULL,
                          Nx = NULL, 
                          Dx = NULL, 
                          Mx = Dx/Nx, 
                          ages =c(0,1,seq(5,90,by=5)),
                          axmethod = "midpoint", 
                          sex = "male", 
                          mxsmooth = TRUE,
                          axsmooth = TRUE, 
                          radix = 100000, 
                          verbose = TRUE,
                          actual_death_counts=NULL){
        if(missing(ages)){
                stop('Please specify age vector!! Even though LT() from LifeTable package accepts ages optionally, but NOT this package!')
        }
        if(missing(mHUI)){
                stop('Please specify mHUI for this package, otherwise HALE or DFLE cannot be estimated. Argument mHUI accepts HRQOL or prevalence of long-term disability.')
        }
        #### output a list of LT object from LT function using LifeTable developed by timriffe
        LT_obj<-LifeTable::LT(Nx=Nx,Dx=Dx,
                              Mx=Mx, 
                              ages=ages, 
                              axmethod =axmethod, 
                              sex=sex,
                              mxsmooth = mxsmooth, 
                              axsmooth = axsmooth,
                              radix = radix,
                              verbose=verbose)
        
        ##### judging the output indicator type
        if(mHUI_input=='HRQOL'){
                message(paste0('Input of mHUI is ',mHUI_input,'. HALE will be calculated!' ))
                output_indicator<-'HALE'
        } else if (mHUI_input=='prevalence'|mHUI_input=='prevalent'|mHUI_input=='prevalence rate' |mHUI_input=='prevalent rate' | mHUI_input=='pr' | mHUI_input=='p'){
                message(paste0('Input of mHUI is ',mHUI_input,'. DFLE will be calculated!' ))
                output_indicator<-'DFLE'
        }
        LT_obj
        mHUI
        
        
        return_list<-list(Indicator=output_indicator,
                          LT_list=LT_obj)
        
}     