library(XML)
library(RSQLite)
library(stringr)
library(ggplot2) 

#User defined functions to differentiate teams by adding a year tag
"%p%"<-function(left,right=2013){
  paste(left,right,sep="")
}


#function to load offense data based on input year
#THIS FUNCTION MUST BE EXECUTED 4 TIMES, TO LOAD ALL FOUR YEARS OFFENSE DATA
offense_year<- function(year){
  #pulling offense data from internet
  url<- paste("http://www.pro-football-reference.com/years/",year, "/", sep="")
  
  if(year>2002){
  passing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[11]]
  rushing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[12]]
  kickNpunt_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[13]]
  }
  
  else if(year>1969){
    passing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[8]]
    rushing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[9]]
    kickNpunt_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[10]]
  }
  else{
    passing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[7]]
    rushing_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[8]]
    kickNpunt_offense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[9]]
  }
  
  if(year>1999){
    passing_offense$EXP<-NULL
    rushing_offense$EXP<-NULL
  }
  #official_standings<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[3]]
  #official_standings1<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[2]]
  names(passing_offense)<-c("Rk","Tm","G","Cmp_PO","Att_PO","Cmp%_PO","Yds_PO","TD_PO","TD%_PO","Int_PO","Int%_PO", "Lng_PO","Y/A_PO","AY/A_PO","Y/C_PO","Y/G_PO","Rate_PO","QBR_PO","Sk_PO","LYds","NY/A_PO","ANY/A_PO","Sk%_PO","4QC_PO","GWD_PO")
  
  names(rushing_offense)<-c("Rk","Tm","G","Att_RO","Yds_RO","TD_RO","Lng_RO","Y/A_RO","Y/G_RO","Fmb_RO")
  
  names(kickNpunt_offense)<-c("Rk","Tm","G","Ret_OPR","Yds_OPR","TD_OPR","Lng_OPR","Y/R_OPR","Ret_OKR","Yds_OKR","TD_PKR","Lng_OKR","Y/Ret_OKR","APYd")
  
  
  #removing the unnecessary data
  passing_offense<-passing_offense[-nrow(passing_offense),]
  passing_offense$Rk<-NULL
  passing_offense$QBR_PO<-NULL
  rushing_offense$Rk<-NULL
  rushing_offense$G<-NULL
  rushing_offense<-rushing_offense[-nrow(rushing_offense),]
  kickNpunt_offense$Rk<-NULL
  kickNpunt_offense$G<-NULL
  kickNpunt_offense<-kickNpunt_offense[-nrow(kickNpunt_offense),]
  
  
  ##cleaning data & formatting
  passing_offense[,1]<-as.character(passing_offense[,1])
  passing_offense[,2:ncol(passing_offense)]<-apply(passing_offense[,2:ncol(passing_offense)],2,as.numeric)
  passing_offense[is.na(passing_offense)]<-0
  rushing_offense[,1]<-as.character(rushing_offense[,1])
  rushing_offense[,2:ncol(rushing_offense)]<-apply(rushing_offense[,2:ncol(rushing_offense)],2,as.numeric)
  kickNpunt_offense[,1]<-as.character(kickNpunt_offense[,1])
  kickNpunt_offense[,2:ncol(kickNpunt_offense)]<-apply(kickNpunt_offense[,2:ncol(kickNpunt_offense)],2,as.numeric)
 
  
  
  ##merging data

  offense_data<-merge(passing_offense,rushing_offense,by.x="Tm", by.y="Tm")
  
  
  offense_data1<-merge(offense_data,kickNpunt_offense,by.x="Tm", by.y="Tm")
  
  ##team_year
  
  for(i in 1:nrow(offense_data)){
    offense_data1[i,1]<-(offense_data1[i,1]%p% "_" %p% year)
  }
  #View(offense_data1)
  offense_data1
}


#Pulling defense data based on years
#THIS FUNCTION MUST BE EXECUTED 4 TIMES, TO LOAD ALL FOUR YEARS DEFENSE DATA    
defense_year<-function(year){
  url<-paste("http://www.pro-football-reference.com/years/",year,"/opp.htm", sep="")
  
  
  passing_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[2]]
  rushing_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[3]]
  kickNpunt_defense<-readHTMLTable(url,encoding="UTF-8",colClasses ="character")[[4]]
  
  if(year>1999){
    passing_defense$EXP<-NULL
    rushing_defense$EXP<-NULL
  }
  
  names(passing_defense)<-c("Rk","Tm", "G","Cmp_PD","Att_PD", "Cmp%_PD", "Yds_PD", "TD_PD","TD%_PD", "Int_PD", "Int%_PD", "Y/A_PD", "AY/A_PD", "Y/C_PD", "Y/G_PD", "Rate_PD", "Sk_PD", "LYds_PD", "NY/A_PD", "ANY/A_PD", "Sk%_PD")
  
  names(rushing_defense)<-c("Rk","Tm","G", "Att_RD", "Yds_RD", "TD_RD","Y/A_RD", "Y/G_RD", "Fmb_RD")
                            
  names(kickNpunt_defense)<-c("Rk","Tm","G", "Ret_PR", "Yds_PR", "TD_PR", "Y/R_PR", "Ret_KR", "Yds_KR","TD_KR", "Y/R_KR")
  
  
  passing_defense<-passing_defense[-nrow(passing_defense),]
  rushing_defense<-rushing_defense[-nrow(rushing_defense),]
  kickNpunt_defense<-kickNpunt_defense[-nrow(kickNpunt_defense),]
  
  passing_defense$Rk<-NULL
  passing_defense$G<-NULL
  rushing_defense$Rk<-NULL
  rushing_defense$Fmb_RD<-NULL
  rushing_defense$G<-NULL
  kickNpunt_defense$Rk<-NULL
  kickNpunt_defense<-kickNpunt_defense[,-nrow(kickNpunt_defense)]
  kickNpunt_defense$G<-NULL
  
  passing_defense[,1]<-as.character(passing_defense[,1])
  passing_defense[,2:ncol(passing_defense)]<-apply(passing_defense[,2:ncol(passing_defense)],2,as.numeric)
  rushing_defense[,1]<-as.character(rushing_defense[,1])
  rushing_defense[,2:ncol(rushing_defense)]<-apply(rushing_defense[,2:ncol(rushing_defense)],2,as.numeric)
  kickNpunt_defense[,1]<-as.character(kickNpunt_defense[,1])
  kickNpunt_defense[,2:ncol(kickNpunt_defense)]<-apply(kickNpunt_defense[,2:ncol(kickNpunt_defense)],2,as.numeric)
  
  
  defense_data<-merge(passing_defense,rushing_defense,by.x="Tm",by.y="Tm")
  defense_data1<-merge(defense_data,kickNpunt_defense,by.x="Tm", by.y="Tm")
  
  ##team_year
  for(i in 1:nrow(defense_data1)){
    defense_data1[i,1]<-(defense_data1[i,1]%p% "_" %p% year)
  }
  #View(defense_data1)
  defense_data1
}

getdata<-function(year){
  offense<-offense_year(year)
  defense<-defense_year(year)
  #View(offense)
  #View(defense)
  both<-merge(offense,defense,by.x="Tm",by.y="Tm")
  #View(both)
  both
}

start <- readline(prompt="Enter start year: ")
start<- as.integer(start)

end<-readline(prompt="Enter end year: ")
end<- as.integer(end)

range<-start:end

#User defined functions to differentiate teams by adding a year tag
for(year in range){
  data<-getdata(year)
  
  if(year == start){
    NFL.combined<-data
  }
  else{
    NFL.combined<-rbind(NFL.combined,data)
  }
}
NFL.combined<-NFL.combined[,-73]
View(NFL.combined)

