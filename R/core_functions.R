HeathExpectancy_core<-function(mHUI=NULL,
                          mHUI_input=NULL,
                          Nx = NULL, 
                          Dx = NULL, 
                          Mx = Dx/Nx, 
                          ages =c(0,1,seq(5,90,by=5)),
                          age_interval=c(diff(c(0,1,seq(5,90,by=5))),5),
                          axmethod = "midpoint", 
                          sex = "male", 
                          mxsmooth = TRUE,
                          axsmooth = TRUE, 
                          radix = 100000, 
                          verbose = TRUE,
                          uncertainty_range=TRUE,
                          alpha=0.05,
                          actual_death_counts=NULL,
                          survival_plots=TRUE,
                          digits=2){
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
        Lx_adj<-LT_obj$Lx*mHUI
        #LT_obj$Tx
        Tx_adj<-rev(cumsum(rev(Lx_adj)))
        HealthExpectancy<-Tx_adj/LT_obj$lx
        HE_output<-data.frame(age=ages,
                              age_interval=age_interval,
                              ex=LT_obj$ex,
                              HealthExpectancy=HealthExpectancy)
        #LT_obj$ex
        #age_interval
        #actual_death_counts
        if(uncertainty_range==TRUE){
                if(missing(actual_death_counts)){
                        stop('The argument actual_death_counts cannot be omitted when uncertainty_range=TRUE. Please CHECK!')
                }
                ### calculate SE for life expectancy ex
                var_qx<-(((LT_obj$qx^2)*(1-LT_obj$qx))/actual_death_counts)*10^8
                Deriv_SE_ex <-unlist(   lapply(2:length(LT_obj$ex), FUN=function(i){
                        (LT_obj$lx[i-1]^2)*((((1-LT_obj$ax[i-1])*age_interval[i-1])+LT_obj$ex[i])^2)*(((LT_obj$qx[i-1]^2)*(1-LT_obj$qx[i-1]))/actual_death_counts[i-1])
                }))
                Deriv_SE_ex_sum<-Deriv_SE_ex
                sum_SE<-c(rev(cumsum(rev(Deriv_SE_ex_sum)[-1])),rev(Deriv_SE_ex)[1])
                SE_ex<-sqrt(sum_SE/LT_obj$lx[length(sum_SE)]^2)
                ex_range<-data.frame(ex_lr=LT_obj$ex[1:length(SE_ex)]-SE_ex*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE),
                           ex_up=LT_obj$ex[1:length(SE_ex)]+SE_ex*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)
                )
                ex_range<-rbind(ex_range,c(NA,NA))
                
                ### calculate SE for HealthExpectancy 
                Deriv_SE_HE <-unlist(   lapply(2:length(HealthExpectancy), FUN=function(i){
                        (LT_obj$lx[i-1]^2)*((((1-LT_obj$ax[i-1])*age_interval[i-1])+HealthExpectancy[i])^2)*(((LT_obj$qx[i-1]^2)*(1-LT_obj$qx[i-1]))/actual_death_counts[i-1])
                }))
                Deriv_SE_HE_sum<-Deriv_SE_HE
                sum_SE_HE<-c(rev(cumsum(rev(Deriv_SE_HE_sum)[-1])),rev(Deriv_SE_HE)[1])
                SE_HE<-sqrt(sum_SE_HE/LT_obj$lx[length(sum_SE_HE)]^2)
                HE_range<-data.frame(HE_lr=HealthExpectancy[1:length(SE_HE)]-SE_HE*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE),
                                     HE_up=HealthExpectancy[1:length(SE_HE)]+SE_HE*qnorm(alpha/2, mean = 0, sd = 1, lower.tail = FALSE)
                )
                HE_range<-rbind(HE_range,c(NA,NA))
                HE_output <-cbind(HE_output[,1:3],ex_range,HE_output$HealthExpectancy,HE_range)
                names(HE_output)[6]<-'HealthExpectancy'
        } else{
                message(paste0('The argument uncertainty_range is set to ',uncertainty_range,'. No uncertainty range would be output! '))
        }
        for(i in 3:ncol(HE_output)){
                HE_output[,i]<-round(HE_output[,i],digits = digits)
        }
        #### also output parameters for survival curve
        Sx_causefree<-LT_obj$lx/LT_obj$lx[1]
        lx_adjusted<-LT_obj$lx*mHUI
        Cause_Sx<-lx_adjusted/LT_obj$lx[1]
        Sx_adj<-Cause_Sx-Sx_causefree
         
        #library(reshape2)
        
        survival_df<-data.frame(age=ages,
                                age_interval=age_interval,
                                Sx_cause_free=Sx_causefree,
                                lx_adj=lx_adjusted,
                                Sx_Cause=Cause_Sx,
                                Sx_adj=Sx_adj)
        if(survival_plots==TRUE){
                survival_gplot<-reshape2::melt(survival_df,
                                               # ID variables - all the variables to keep but not split apart on
                                               id.vars=c('age','age_interval',"lx_adj", "Sx_adj"),
                                               # The source columns
                                               measure.vars=c("Sx_cause_free", "Sx_Cause" ),
                                               # Name of the destination column that will identify the original
                                               # column that the measurement came from
                                               variable.name="Type",
                                               value.name="Rates")
                gplots<-ggplot2::ggplot(data=survival_gplot)+
                        geom_line(aes(x=age,y=Rates,group=Type,color=Type))+theme_bw()+
                        scale_x_continuous(#limits = c(0, 155000),
                                           breaks = ages#,
                                           #labels=ages
                                           )+
                        xlab('Age')+
                        theme(axis.title = element_text(#family='Times',
                                                        size=10,face='bold'),
                              legend.title=element_blank(),
                              panel.grid = element_blank(),
                              #axis.text = element_text(), #angle = 90,
                              #       # family = 'Times'),
                              legend.text = element_text(#family='Times',
                                                         size=10,face='bold'))
                print(gplots)
                return_list<-list(Indicator=output_indicator,
                                  HE_output=HE_output,
                                  LT_list=LT_obj,
                                  survival_data=survival_df,
                                  ggplot_data=list(plot_df=survival_gplot,
                                                   ggplot=gplots))
        } else{
                return_list<-list(Indicator=output_indicator,
                                  HE_output=HE_output,
                                  LT_list=LT_obj,
                                  survival_data=survival_df)#,
                                  #ggplot_data=list(plot_df=survival_gplot,
                                  #                 ggplot=gplots))
        }
       
       # plot(survival_df$age,survival_df$Sx,type = 'l',add.new=TRUE)
       # plot(survival_df$age,survival_df$Cause_free,type = 'o')
        
        return(return_list)
        
}     