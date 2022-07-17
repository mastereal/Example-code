#library(rstan)
#library(rstanarm)
#library(dplyr)
#source("simpleStan.R")

r_window<- function(data,window=2,team_number=20,pred_week=2,start_year=1,start_week=2){
  ## window 是窗口长度（年）
  ## start_year是数据从第几年开始，第1年开始就是指从头开始，此时如果window=2，则预测数据起始两年后的赛事
  match_number<-(team_number-1)*team_number*window  ## 窗口长度内总赛事数目
  week_match_number<-team_number/2  ## 每周赛事数目
  start_index<-1+(team_number-1)*team_number*(start_year-1)+start_week*week_match_number ## 数据筛选起始行行数
  predlist<-NULL
  if (pred_week==1){
    n=1
    weekindex=c(1)
    data_fit<-data[(start_index+(n-1)*week_match_number):(start_index-1+match_number+(n-1)*week_match_number),]%>% dplyr:: select(Season,home,visitor,hgoal,vgoal)
    data_pred<- data[(start_index+match_number+(n-1)*week_match_number):(start_index-1+match_number+n*week_match_number),]%>% dplyr:: select(home, visitor)
    fit_ <- weekstan(data=data_fit,seasonteams=20,matchpred = data_pred)
    pred_<-foot_prob(fit_,data=data_fit,matchpred = data_pred)
    pred_data<-data.frame(pred_)
    predlist[[n]]<-pred_data
  } else{
  weekindex<-c(1:pred_week)
  for (n in 1:pred_week){
    data_fit<-data[(start_index+(n-1)*week_match_number):(start_index-1+match_number+(n-1)*week_match_number),]%>% dplyr:: select(Season,home,visitor,hgoal,vgoal)
    data_pred<- data[(start_index+match_number+(n-1)*week_match_number):(start_index-1+match_number+n*week_match_number),]%>% dplyr:: select(home, visitor)
    fit_ <- weekstan(data=data_fit,seasonteams=20,matchpred = data_pred)
    pred_<-foot_prob(fit_,data=data_fit,matchpred = data_pred)
    pred_data<-data.frame(pred_)
    predlist[[n]]<-pred_data
    }
  }
  all_pro<-list(prob=predlist,week=weekindex)
  return(all_pro)
}

handicap<- function(data,home_team,away_team,lines=seq(-3,3,0.25)){
  indexa<-data[c("home_team","away_team")]
  indexb<-unique(indexa)
  if (missing(home_team) & missing(away_team)){
    home_team <- indexb$home_team
    away_team <- indexb$away_team    
  }
  ahlist<-NULL
  teamlist<-NULL
  for (i in 1:length(home_team)){
    home<-home_team[i]
    away<-away_team[i]
    df<-data[which(data$home_team==home& data$away_team == away),]
    df$goal_diff<-df$Home-df$Away
    match_prob<-aggregate(df$Prob, by=list(type=df$goal_diff),sum)
    ahh<-lines
    aha<--lines
    hodds<-NULL
    aodds<-NULL
    for (n in 1:length(ahh)){
      l<-ahh[n]
      odd<-(1-match_prob$x[which(match_prob$type+l==0)])/sum(match_prob$x[which(match_prob$type+l>0)])
      aodd<-(1-match_prob$x[which(match_prob$type+l==0)])/sum(match_prob$x[which(match_prob$type+l<0)])
      hodds<-append(hodds,odd)
      aodds<-append(aodds,aodd)
      odd<-(1-match_prob$x[which(match_prob$type+l-0.25==0)]/2)/(match_prob$x[which(match_prob$type+l-0.25==0)]/2+sum(match_prob$x[which(match_prob$type+l-0.25>0)]))
      aodd<-(1-match_prob$x[which(match_prob$type+l-0.25==0)]/2)/sum(match_prob$x[which(match_prob$type+l-0.25<0)])
      hodds<-append(hodds,odd)
      aodds<-append(aodds,aodd)
      odd<-1/(match_prob$x[which(match_prob$type+l-0.5==0)]+sum(match_prob$x[which(match_prob$type+l-0.5>0)]))
      aodd<-1/(match_prob$x[which(match_prob$type+l+0.5==0)]+sum(match_prob$x[which(match_prob$type+l+0.5<0)]))
      hodds<-append(hodds,odd)
      aodds<-append(aodds,aodd)
      odd<-(1-match_prob$x[which(match_prob$type+l+0.25==0)]/2)/sum(match_prob$x[which(match_prob$type+l+0.25>0)])
      aodd<-(1-match_prob$x[which(match_prob$type+l+0.25==0)]/2)/(match_prob$x[which(match_prob$type+l+0.25==0)]/2+sum(match_prob$x[which(match_prob$type+l+0.25<0)]))
      hodds<-append(hodds,odd)
      aodds<-append(aodds,aodd)
      if (length(hodds)!=n){
        odd<-NA
        aodd<-NA
        hodds<-append(hodds,odd)
        aodds<-append(aodds,aodd)
      }
    }
    ah <- data.frame(ahh,hodds,aha,aodds) 
    ahlist[[i]]<-ah
    teamlist[[i]]<-c(home,away)
  }
  all_ah<-list(handicap=ahlist,team=teamlist)
  return(all_ah)
}
#data_all<-read.csv("data.csv",header = TRUE)
handicap_rdata<-function (object,num){
  final<-NULL
  manu_week<-NULL
  for (n in object$week){
    tem<-object$prob[[n]]
    ah<-handicap(tem)
    weeksign<-paste(num,"-",n)
    final[[n]]<-ah
    manu_week<-append(manu_week,weeksign)
  }
  datastructure<-list(handicap=final,weeklist=manu_week)
  return(datastructure)
}

odd_match<-function(object,data,window=2,team_number=20,pred_week=2,start_year=1,start_week=2){
  match_number<-(team_number-1)*team_number*window  
  week_match_number<-team_number/2  
  start_index<-1+(team_number-1)*team_number*(start_year-1)+start_week*week_match_number 
  pred_odds<-foreach (n = 1:pred_week,.combine=rbind) %do% {
    base<- data[(start_index+match_number+(n-1)*week_match_number):(start_index-1+match_number+n*week_match_number),]%>% dplyr:: select(Season,home,visitor,hgoal,vgoal,FTR,AHh,AvgAHH,AvgAHA)
    #print(base)
    for (i in 1:week_match_number){
      AHh_ob<-base$AHh[i]
      ahh_check<-object[["handicap"]][[n]][["handicap"]][[i]][["ahh"]]
      AHh_pred_list<-object[["handicap"]][[n]][["handicap"]][[i]][["hodds"]]
      aha_check<-object[["handicap"]][[n]][["handicap"]][[i]][["aha"]]
      AHa_pred_list<-object[["handicap"]][[n]][["handicap"]][[i]][["aodds"]]
      check_indx<-which(ahh_check==AHh_ob)
      AHH_pred<-AHh_pred_list[check_indx]
      AHa<-aha_check[check_indx]
      AHA_pred<-AHa_pred_list[check_indx]
      base$predAHH[i]<-AHH_pred
      base$AHa[i]<-AHa
      base$predAHA[i]<-AHA_pred
    }
    print(base)
  }
  return(pred_odds)
}

allodd<- function(object,team_number=20,pred_week=1){
  week_match_number<-team_number/2
  pred_odds<-foreach (n = 1:pred_week,.combine=rbind) %do% {
    data_exp_tot <- rep(1,6)
    for (i in 1:week_match_number){
      ahh_l<-object[["handicap"]][[n]][["handicap"]][[i]][["ahh"]]
      AHh_odd<-object[["handicap"]][[n]][["handicap"]][[i]][["hodds"]]
      aha_l<-object[["handicap"]][[n]][["handicap"]][[i]][["aha"]]
      AHa_odd<-object[["handicap"]][[n]][["handicap"]][[i]][["aodds"]]
      hteam<-object[["handicap"]][[n]][["team"]][[i]][[1]]
      ateam<-object[["handicap"]][[n]][["team"]][[i]][[2]]
      exp<-data.frame(ahh=ahh_l,ahhodd=AHh_odd,aha=aha_l,ahaodd=AHa_odd)
      #exp$ahh_odd<-AHh_odd
      #exp$aha<-aha_l
      #exp$aha_odd<-AHa_odd
      exp$home_team <- hteam
      exp$away_team <- ateam
      data_exp_tot <- rbind(data_exp_tot,exp)
    }
    data_exp_tot <- data_exp_tot[-c(1), ]
  }
  return(data_exp_tot)  
}