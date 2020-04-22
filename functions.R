#load some plotting libraries
library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(reshape2)

#growth function 
exp_growth<-function(nweeks,IIR,doubling_time){
  exponential_growth<-rep(0,nweeks)  
  for (i in 1:length(exponential_growth)){
    if (i == 1) I<-IIR
    exponential_growth[i]<-(2/doubling_time)*I
    I<-exponential_growth[i]
  }
  return(exponential_growth)
} 

##decay_func
exp_decay<-function(nweeks=4,IIR=1e-5,decay_rate=.25)  {
  exponential_decay<-rep(0,nweeks)  
  for (i in 1:length(exponential_decay)){
    if (i == 1) I<-IIR
    print(I)
    exponential_decay[i]<-(decay_rate)*I
    I<-exponential_decay[i]
    
  }
  return(exponential_decay)
}


lockdown_cycle<-function(
  ##### ##### ##### all time scales are in weeks ##### ##### ##### #####
  weeks_lock=4, #how many weeks in each lockdown
  weeks_free=8, #how many weeks between lockdowns
  first_lock_weeks=8, #how many weeks of first lockdown
  nlocks=6, #number of total lockdowns 
  decay_rate=.25, #rate at which virus dwindles
  doubling_time=1,#rate at which virus infectivity doubles 
  total_population=1e5, #total population to be considered
  initial_infect=1 #number of people initial infected
){
  total_time<-(weeks_lock+weeks_free)*nlocks 
  IIR<-initial_infect/total_population ##initial infection rate
  
  initial_decay<-exp_decay(first_lock_weeks,IIR,decay_rate = decay_rate)

  infection_trend<-rep(0, total_time)
  for(i in 1:nlocks){
    if(i==1) I<-initial_decay[length(initial_decay)]
    exponential_growth<-exp_growth(weeks_free, IIR=I,doubling_time = doubling_time)
    exponential_decay<-exp_decay(weeks_lock, exponential_growth[length(exponential_growth)], decay_rate=decay_rate)
    # exponential_growth<-exp_growth(weeks_free, exponential_decay[weeks_lock], doubling_time = doubling_time)
    I<-exponential_decay[weeks_lock]
    infection_trend[((i-1)*(weeks_lock+weeks_free)+(1:(weeks_lock+weeks_free)))]<-c(exponential_growth,exponential_decay)
  }
  infection_trend<-c(IIR,initial_decay,infection_trend)
  #create a dataframe in order to allow for easy plotting   
  infection_trend_df<-data.frame("week"=1:length(infection_trend),
                                 "infected"=infection_trend*total_population) 
  # View(infection_trend_df)
  return(infection_trend_df)
}
