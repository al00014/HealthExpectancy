#' HeaLY function
#'
#' This function estimates HeaLY and its rate, HeaLY due to death and
#' its proportion, HeaLY due to disability and its proportions, based on input
#' of age-specific mortality, prevalence and incidence cases. It can also accept
#' input of incidence rate, case fatality ratio and case disability ratio which
#' summarized for all age groups.
#' The most outstanding feature of this function is that it accepts
#' the above two methods (example below).
#' The default age group should be 19 groups, starting from 0~1, 1~4, and then
#' by 5 years up to 85+.
#' HeaLY is a summarized indicator for all age group, unlike DALY, which can be dissected by age group.
#'
#' @param year an indicator of years covered by data.
#' @param gender a character indicator for gender.
#' @param time_range a number showing the range of years for taking discount.
#' @param Population a vector of age-specific population.
#' @param Incidence_case a vector of incidence cases for a specific disease, by age group.
#' @param Mortality_case a vector of mortality cases for a specific disease, by age group.
#' @param Prevalence_case a vector of prevalence cases for a specific disease, by age group.
#' @param Duration_byage a vector of duration for a specific disease, by age group. This function currently do not take in uncertainty range for Duration_byage.
#' @param Age_onset_byage a vector of age_onset for a specific disease, by age group.
#' @param Standard_LifeExpectancy a vector of age-specific life expectancy, current default standard is the Coale and Demeny West Level 26 life table, can only accept LE for one gender. But if use EAo instead, the HeaLY result may be meaningful for both genders, as shown in example 1.
#' @param P a direct summarized indicator for population of all age groups, for only one gender.
#' @param I Incidence rate per 1000 population (default) per year, a direct summarized indicator for all age groups. per X population is controlled by person_units argument.
#' @param Af Average age at death, a direct summarized indicator for all age groups.
#' @param Ao Average age at onset, a direct summarized indicator for all age groups.
#' @param EAo Expectation of life at age of onset, a direct summarized indicator for all age groups.
#' @param Dt Average duration of disability for those disabled by the disease; a composite of temporary and permanent disability based on the proportion of cases in each category. A direct summarized indicator for all age groups.
#' @param CFR Case fatality ratio:proportion of those developing the disease who die from the disease, range from 0-1, a direct summarized indicator for all age groups
#' @param verbose controls results log in console. Default to True.
#' @param Duration_interval  must comply with Duration_byage or Dt. If use Duration_byage, than Duration_interval must also have the same number of rows by age matching Duration_byage.
#' @param DisabilityWeight_interval must comply with Duration_interval. If Duration_interval is by age, DisabilityWeight_interval must be by age. If Duration_interval is for all age groups, so should DisabilityWeight_interval.
#' @param ...
#' @return A list of HeaLY, rates and proportion. If uncertainty_range=TRUE, a list of interval results would be returned.
#' @keywords HeaLY, mcmc_sample
#' @export
#' @examples
#' Example 1: (Without input of population)
#' Data from "Measuring the burden of disease: healthy life-years."
#' PMCID: PMC1508183
#'
#' ########### Directly input of indicators summarized for all age groups
#'
#' ##### without uncertainty range
#' HeaLY(year="1976-1981",
#'       gender='males and females',
#'       I=40, # method 1
#'       Af=1,
#'       Ao=1,
#'       EAo=81.84,
#'       CDR=1,
#'       Disability_weight=0.9,
#'       Dt=1.48,
#'       CFR=0.02,
#'       time_range=6,
#'       uncertainty_range=FALSE)
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
        
        
        return_list<-list(Indicator=output_indicator,
                          LT_list=LT_obj)
        
}     