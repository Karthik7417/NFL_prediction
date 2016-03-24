# NFL_prediction
NFL

#User defined functions to differentiate teams by adding a year tag
"%p%"<-function(left,right=2013){
  paste(left,right,sep="")
}

#function to load offense data based on input year
#THIS FUNCTION MUST BE EXECUTED 4 TIMES, TO LOAD ALL FOUR YEARS OFFENSE DATA
offense_year<- function(year){
  #pulling offense data from internet
  library("XML", lib.loc="~/R/win-library/3.2")
  url<- paste("http://www.pro-football-reference.com/years/",year, "/", sep="")
  passing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[11]]
  rushing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[12]]
  kickNpunt_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[13]]
  official_standings<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[3]]
  official_standings1<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[2]]
  
  #removing the unnecessary data
  passing_offense<-passing_offense[-33,]
  passing_offense<-passing_offense[,-17]
  rushing_offense<-rushing_offense[-33,]
  kickNpunt_offense<-kickNpunt_offense[-33,]
  passing_offense<-passing_offense[,-1]
  rushing_offense<-rushing_offense[,-1]
  kickNpunt_offense<-kickNpunt_offense[,-1]
  official_standings<-official_standings[-1,]
  official_standings<-official_standings[-5,]
  official_standings<-official_standings[-9,]
  official_standings<-official_standings[-13,]
  official_standings1<-official_standings1[-1,]
  official_standings1<-official_standings1[-5,]
  official_standings1<-official_standings1[-9,]
  official_standings1<-official_standings1[-13,]
  
  ##cleaning data & formatting
  passing_offense[,1]<-as.character(passing_offense[,1])
  passing_offense[,2:ncol(passing_offense)]<-apply(passing_offense[,2:ncol(passing_offense)],2,as.numeric)
  passing_offense[is.na(passing_offense)]<-0
  rushing_offense[,1]<-as.character(rushing_offense[,1])
  rushing_offense[,2:ncol(rushing_offense)]<-apply(rushing_offense[,2:ncol(rushing_offense)],2,as.numeric)
  kickNpunt_offense[,1]<-as.character(kickNpunt_offense[,1])
  kickNpunt_offense[,2:ncol(kickNpunt_offense)]<-apply(kickNpunt_offense[,2:ncol(kickNpunt_offense)],2,as.numeric)
  official_standings[,1]<-as.character(official_standings[,1])
  official_standings[,2:ncol(official_standings)]<-apply(official_standings[,2:ncol(official_standings)],2,as.numeric)
  official_standings1[,1]<-as.character(official_standings1[,1])
  official_standings1[,2:ncol(official_standings1)]<-apply(official_standings1[,2:ncol(official_standings1)],2,as.numeric)
  
  ##merging data
  official_standings<-rbind(official_standings,official_standings1)
  offense_data<-merge(passing_offense,rushing_offense,by.x="Tm",by.y="Tm")
  offense_data<-offense_data[,-25] ## cleaning the new table (offense_data)
  offense_data<-merge(offense_data,kickNpunt_offense,by.x="Tm",by.y="Tm")
  
  ##team_year
  for(i in 1:32){
    offense_data[i,1]<-(offense_data[i,1]%p% "_" %p% year)
    offense_data[i,1]
  }
  offense_data
}

#merge offense data
offense_combined<-offense_2011
offense_combined<-rbind(offense_combined,offense_2012)
offense_combined<-rbind(offense_combined,offense_2013)
offense_combined<-rbind(offense_combined,offense_2014)
View(offense_combined)


#Pulling defense data based on years
#THIS FUNCTION MUST BE EXECUTED 4 TIMES, TO LOAD ALL FOUR YEARS DEFENSE DATA    
defense_year<-function(year){
  library("XML", lib.loc="~/R/win-library/3.2")
  url<-paste("http://www.pro-football-reference.com/years/",year,"/opp.htm", sep="")
  passing_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[2]]
  rushing_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[3]]
  kickNpunt_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[4]]
  passing_defense<-passing_defense[-33,]
  rushing_defense<-rushing_defense[-33,]
  kickNpunt_defense<-kickNpunt_defense[-33,]
  
  passing_defense<-passing_defense[,-1]
  rushing_defense<-rushing_defense[,-1]
  kickNpunt_defense<-kickNpunt_defense[,-1]
  
  passing_defense[,1]<-as.character(passing_defense[,1])
  passing_defense[,2:ncol(passing_defense)]<-apply(passing_defense[,2:ncol(passing_defense)],2,as.numeric)
  rushing_defense[,1]<-as.character(rushing_defense[,1])
  rushing_defense[,2:ncol(rushing_defense)]<-apply(rushing_defense[,2:ncol(rushing_defense)],2,as.numeric)
  kickNpunt_defense[,1]<-as.character(kickNpunt_defense[,1])
  kickNpunt_defense[,2:ncol(kickNpunt_defense)]<-apply(kickNpunt_defense[,2:ncol(kickNpunt_defense)],2,as.numeric)
  
  defense_data<-merge(passing_defense,rushing_defense,by.x="Tm",by.y="Tm")
  defense_data<-merge(defense_data,kickNpunt_defense,by.x="Tm",by.y="Tm")
  
  ##team_year
  for(i in 1:32){
    defense_data[i,1]<-(defense_data[i,1]%p% "_" %p% year)
    defense_data[i,1]
  }
  defense_data
}

#merge defense
defense_combined<-defense_2011
defense_combined<-rbind(defense_combined,defense_2012)
defense_combined<-rbind(defense_combined,defense_2013)
defense_combined<-rbind(defense_combined,defense_2014)
View(defense_combined)


#names defense
names(defense_combined)<-c("Tm", "G","Cmp_PD","Att_PD", "Cmp%_PD", "Yds_PD", "TD_PD","TD%_PD", "Int_PD", "Int%_PD", "Y/A_PD", "AY/A_PD", "Y/C_PD", "Y/G_PD", "Rate_PD", "Sk_PD", "LYds_PD", "NY/A_PD", "ANY/A_PD", "Sk%_PD", "Exp_PD", "G.x", "Att_RD", "Yds_RD", "TD_RD","Y/A_RD", "Y/G_RD", "Fmb_RD", "Exp_RD", "G.y", "Ret_PR", "Yds_PR", "TD_PR", "Y/R_PR", "Ret_KR", "Yds_KR", "TD_KR", "Y/R_KR","APYd_KR" )

#names_offense
names(offense_combined)<-c("Tm","G","Cmp_PO","Att_PO","Cmp%_PO","Yds_PO","TD_PO","TD%_PO","Int_PO","Int%_PO", "Lng_PO","Y/A_PO","AY/A_PO","Y/C_PO","Y/G_PO","QBR_PO","Sk_PO","LYds","NY/A_PO","ANY/A_PO","Sk%_PO","4QC_PO","GWD_PO","Exp_PO","Att_RO","Yds_RO","TD_RO","Lng_RO","Y/A_RO","Y/G_RO","Fmb_RO","Exp_RO","G","Ret_OPR","Yds_OPR","TD_OPR","Lng_OPR","Y/R_OPR","Ret_OKR","Yds_OKR","TD_PKR","Lng_OKR","Y/Ret_OKR","APYd")


##offense column deletion
offense_combined<-offense_combined[,-c(5,8,10,12,13,14,15,16,19,20,21,29,30,36,38,43)]

##offense calculations
offense_combined$Cmp_pct_PO<-(offense_combined$Cmp_PO*100)/offense_combined$Att_PO 

offense_combined$TD_pct<-(offense_combined$TD_PO*100)/offense_combined$Att_PO 
 
offense_combined$Int_pct<-(offense_combined$Int_PO*100)/offense_combined$Att_PO

offense_combined$YperA_PO <- (offense_combined$Yds_PO)/offense_combined$Att_PO

offense_combined$AYperA_PO <- (offense_combined$Yds_PO+20*offense_combined$TD_PO-45*offense_combined$Int_PO)/offense_combined$Att_PO

offense_combined$YperC_PO <- (offense_combined$Yds_PO)/offense_combined$Cmp_PO

offense_combined$YperG_PO <- (offense_combined$Yds_PO)/offense_combined$G

offense_combined$NYperAtt_PO <- (offense_combined$Yds_PO-offense_combined$LYds)/offense_combined$Att_PO

offense_combined$ANYperAtt_PO <- (offense_combined$Yds_PO-offense_combined$LYds+(20*offense_combined$TD_PO)-(45*offense_combined$Int_PO))/(offense_combined$Att_PO+offense_combined$Sk_PO)

offense_combined$Sk_pct_PO <- (offense_combined$Sk_PO)/(offense_combined$Att_PO+offense_combined$Sk_PO)

offense_combined$YperA_RO <- (offense_combined$Yds_RO)/offense_combined$Att_RO

offense_combined$YperG_RO <- (offense_combined$Yds_RO)/offense_combined$G

offense_combined$YperRet_OPR <- (offense_combined$Yds_OPR)/offense_combined$Ret_OPR

offense_combined$YperRet_OKR <- (offense_combined$Yds_OKR)/offense_combined$Ret_OKR

#defense column deletion
defense_combined<-defense_combined[,-c(5,8,10,11,12,13,14,18,19,20,22,26,27,28,30,34,38,39)]

##defense calculations

defense_combined$Cmp_pct_PD<-(defense_combined$Cmp_PD*100)/defense_combined$Att_PD 

 defense_combined$TD_pct_PD<-(defense_combined$TD_PD*100)/defense_combined$Att_PD 

 defense_combined$Int_pct_PD<-(defense_combined$Int_PD*100)/defense_combined$Att_PD 
 
 defense_combined$YperG_PD <- (defense_combined$Yds_PD)/defense_combined$G

 defense_combined$YperA_PD<-(defense_combined$Yds_PD)/defense_combined$Att_PD 

 defense_combined$AYperA_PD <- (defense_combined$Yds_PD+20*defense_combined$TD_PD-45*defense_combined$Int_PD)/defense_combined$Att_PD
  
defense_combined$YperC_PD<-(defense_combined$Yds_PD)/defense_combined$Cmp_PD 

defense_combined$NYperAtt_PD <- (defense_combined$Yds_PD-defense_combined$LYds)/defense_combined$Att_PD

defense_combined$ANYperAtt_PD <- (defense_combined$Yds_PD-defense_combined$LYds_PD+(20*defense_combined$TD_PD)-(45*defense_combined$Int_PD))/(defense_combined$Att_PD+defense_combined$`Sk%`)

defense_combined$YperA_RD <- (defense_combined$Yds_RD)/defense_combined$Att_RD

defense_combined$YperG_RD <- (defense_combined$Yds_RD)/defense_combined$G

defense_combined$YperRet_DPR <- (defense_combined$Yds_PR)/defense_combined$Ret_PR

defense_combined$YperRet_DKR <- (defense_combined$Yds_KR)/defense_combined$Ret_KR


##plots
hist(offense_2011$Cmp_pct_PO, breaks=10, main="Pass Completion per team", xlab="Pass Completion",ylab="Number of Teams")

mean(offense_2011$Cmp_pct_PO)
sd(offense_2011$Cmp_pct_PO)
max(offense_2011$Cmp_pct_PO)
min(offense_2011$Cmp_pct_PO)

##plot 2
hist(defense_2011$Cmp_pct_PD, breaks=10, main="Pass Completion of Opponent Team", xlab="Pass Completion",ylab="Number of Teams")

mean(defense_2011$Cmp_pct_PD)
sd(defense_2011$Cmp_pct_PD)
max(defense_2011$Cmp_pct_PD)
min(defense_2011$Cmp_pct_PD)


#plot 3 - defense yards
drypg <- transform(defense_2011,Tm=reorder(Tm,-defense_2011$YperG_RD))
ggplot(drypg,aes(x=Tm, y=YperG_RD)) +
  geom_bar(stat='identity',color="black",fill="red") +
  coord_flip() + labs(x="Team",y="Avg Rushing Yards Gained by Opponenet") +
  ggtitle("Avg Rushing Yards Gained by Oppenent team per Game") + theme(plot.title =
                                                                          element_text(size=18, face="bold"))

#plot 4 - offense yards 
orypg <- transform(offense_2011,Tm=reorder(Tm,-offense_2011$YperG_RO))
ggplot(orypg,aes(x=Tm, y=YperG_RO)) +
geom_bar(stat='identity',color="black",fill="blue") +
  coord_flip() + labs(x="Team",y="Avg Rushing Yards Gained") +
  ggtitle("Avg Rushing Yards Gained per Game") + theme(plot.title =
                                                  element_text(size=18, face="bold"))
