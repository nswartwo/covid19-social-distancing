#load some plotting libraries
library(ggplot2)
library(gridExtra)
library(scales)
library(dplyr)
library(reshape2)
library("expm")
library("msm")
cycle_function<-function(
  periods=11, 
  weeksup=6, 
  weeksdown=6, 
  Rup=2, 
  Rdown=.32, 
  incubationdays=4, 
  infectiousdays=4,
  initialinfrate=.001
){
  #define global parameters (temporarily; will be properly scoped after)
  # periods<<-6;
  # weeksup<<-6;
  # weeksdown <<- 6; Rup <<- 2; Rdown <<- .32;
  # incubationdays <<- 4; infectiousdays <<- 4; initialinfrate<-.001;
  leng<-weeksup+weeksdown
  #define the function for E,I matrix basic 
  
  m<-function(R,
              tx, 
              incdays, 
              infdays){
    s<-MatrixExp(mat=matrix(c(-7/incdays, 7*R/infdays, 
                              7/incdays, -7/infdays), 
                            2,2, byrow="TRUE"), t=tx)
    return(s)
  }
  #matrix for the full period 
  MFullPeriod <- m(R=Rup, tx=weeksup, incdays = incubationdays, infdays = infectiousdays)%*%
    m(R=Rdown, tx=weeksdown, incdays = incubationdays, infdays = infectiousdays);
  #matrix for a partial period 
  MPartialPeriod<-function(r, 
                           len, 
                           Rd, 
                           Ru, 
                           weeksd){
    if(r <(weeksdown/len)){
      s<-m(R=Rd, tx=r*len, incdays = incubationdays, infdays = infectiousdays)
    } else{
      s<-m(R=Ru, tx=(r*len)-weeksd, incdays = incubationdays, infdays = infectiousdays) %*%
        m(R=Rd, tx=weeksd, incdays = incubationdays, infdays = infectiousdays)
    }
    return(s)
  }
  
  #define the time vector
  a<-seq(0,periods*leng,.1)
  #define the trend vector
  trend<-rep(0,length(a))
  for(i in 1:length(a)){
    temp<-c(0,1)%*%MPartialPeriod(r=((a[i]/leng)-floor(a[i]/leng)), 
                                  len=leng, Rd=Rdown, Ru=Rup, weeksd = weeksdown)%*%(MFullPeriod%^%floor(1/leng))
    trend[i]<-initialinfrate*sum(temp*c(1,1))
  }
  
  #add in t=0; 
  trend<-c(initialinfrate,trend)
  # print(length(trend))
  # print(length(0:(periods*leng)))
  plot(trend, type="l", col="darkblue", log="y")
  lines(y=rep(initialinfrate,length(a)),x=a, col="red") #steady moderation line
  #create a dataframe
  trend_df<-data.frame("week"=c(0,a), 
                       "infected"=trend)
  alpha<-seq(0,2,0.1)
  utility_df<-data.frame("alpha"=alpha,
                         "U"=weeksup*periods+((weeksdown*periods)*Rdown^alpha), 
                         "steadyU"=leng*periods*(sqrt(Rdown)^alpha))
  results_list<-list(trend_df, 
                     utility_df)
  return(results_list)
}

