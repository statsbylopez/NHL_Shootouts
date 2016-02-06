rm(list = ls())
require(mosaic)
require(knitr)
require(lme4)
require(XML)
require(ggplot2) 
require(extrafont)
require(broom)
require(dotwhisker)
require(dplyr)
require(RCurl)
loadfonts()

######################
#Theme for the plots
######################


theme.plots<-theme(axis.text.x = element_text(size=rel(1.9)),
                   axis.text.y = element_text(size=rel(1.9)),
                   axis.title.x = element_text(size=rel(1.9), vjust=-1),
                   axis.title.y = element_text(size=rel(1.9)),
                   plot.margin = unit(c(1,0.3,0.5,0), "cm"),legend.position = "none",
                   plot.title = element_text(size=rel(2.0)))


#Get the data
x <- getURL("https://docs.google.com/spreadsheets/d/1oXhcXE4N5MUqv9VifxTXvPjnfhwPGtZTZxvinzyn6OM/pub?output=csv")
so.dat <- read.csv(text = x)


######################
#Descriptive statistics
######################


#Do visitors win more often?
mean(so.dat$VisGoals>so.dat$HomeGoals)



######################
#Year to Year consistency
######################

team.list<-unique(so.dat$Visitor)
df<-matrix(nrow=9*length(unique(team.list)),ncol=6)
colnames(df)<-c("Year1","Year2","Seasons","Team","nYear1","nYear2")


for (i in 1:30){
  for (seas in 2006:2014){
    y1<-filter(so.dat,Home==team.list[i]|Visitor==team.list[i],Year==seas)
    y2<-filter(so.dat,Home==team.list[i]|Visitor==team.list[i],Year==seas+1)
    df[(seas-2005)+9*(i-1),1]<-mean(y1$Winner==team.list[i])
    df[(seas-2005)+9*(i-1),2]<-mean(y2$Winner==team.list[i])
    df[(seas-2005)+9*(i-1),3]<-paste(seas,seas+1)
    df[(seas-2005)+9*(i-1),4]<-as.character(team.list[i]) 
    df[(seas-2005)+9*(i-1),5]<-nrow(y1)
    df[(seas-2005)+9*(i-1),6]<-nrow(y2)
  }}
df1<-data.frame(df, stringsAsFactors = FALSE)
df1$Year1<-as.numeric(as.character(df1$Year1))
df1$Year2<-as.numeric(as.character(df1$Year2))
df1$nYear1<-as.numeric(as.character(df1$nYear1))
df1$nYear2<-as.numeric(as.character(df1$nYear2))


p<-ggplot(df1, aes(x=Year1, y=Year2))+ stat_smooth() + geom_point(position="jitter",aes(size=1.5))+theme_bw()+theme.plots+
  scale_x_continuous(labels=c("0%","25%","50%","75%","100%"),"Year 1 win %")+
  scale_y_continuous(labels=c("0%","25%","50%","75%","100%"),"Year 2 win %")+ggtitle("Year to Year SO win percentages (team)")
p


######################
#Teams with best shooters
######################


#Look at performance of teams with best shooters (Nielsen, Oshie, Toews between 08 and 15)
teams<-c("St. Louis Blues","Chicago Blackhawks","New York Islanders")
tab.teams<-filter(df1,Team%in%teams,Seasons!="2006 2007",Seasons!="2007 2008")
mean(tab.teams$Year2)

#How about the team with the best goalie
teams<-c("Pittsburgh Penguins", "New York Rangers")
tab.teams<-filter(df1,Team%in%teams)
mean(tab.teams$Year2)

#Are there associations between a teams shootout talent and how often they reach the shootout?
cor(df1$Year2,df1$nYear2)
cor(df1$Year1,df1$nYear2)



######################
#Visiting team win %
######################

so.dat$Vis.Win<-so.dat$VisGoals>so.dat$HomeGoals
tab.VisWin<-tab<-so.dat %>% group_by(Year) %>% summarize(WinRate= mean(Vis.Win),size= length(Vis.Win))
tab.VisWin$SE<-sqrt(tab.VisWin$WinRate*(1-tab.VisWin$WinRate)/tab.VisWin$size)

p<-ggplot(tab.VisWin, aes(x=Year, y=WinRate)) + 
  geom_errorbar(aes(ymin=WinRate-1.96*SE, ymax=WinRate+1.96*SE), colour="black", width=.1) +
  geom_line() +
  geom_point(size=3, shape=16) +
  scale_y_continuous("",labels=c("40%","50%","60%","70%"))+
  theme_bw()+theme.plots+
  ggtitle("Visiting team SO win percentage")+geom_hline(aes(yintercept=0.5),col="red",lty=2)+
  scale_x_continuous("Season",breaks=2006:2015,labels=c("2005-06","2006-07","2007-08","2008-09",
                                                        "2009-10","2010-11","2011-12","2012-13",
                                                        "2013-14","2014-15"))
p




######################
#Order choice: shoot first or second?
######################


y <- getURL("https://docs.google.com/spreadsheets/d/1SAByAftxLi8ozisTwERn-IOmHxmksWNqV1tAv2KJlYo/pub?output=csv")
nhl <- read.csv(text = y)

nhl1<-nhl[order(nhl$Year,nhl$GameNumb,nhl$ShotNumber),]
nhl1<-filter(nhl1,Year!="20052006",Year!="20062007")
nhl1$gid<-paste(nhl1$GameNumb,nhl1$Year)
result<-nhl1 %>% 
  group_by(gid) %>% 
  summarize(h.goal = sum(Goal01&ShootingTm=="Home"),v.goal=sum(Goal01&ShootingTm=="Visitor"),
       first.team=ShootingTm[ShotNumber=1],year=Year[ShotNumber=1],game=GameNumb[ShotNumber=1],
       home=Home[ShotNumber=1])
result<-result[order(result$year,result$game),]
result$winner<-ifelse(result$h.goal<result$v.goal,"Visitor","Home")


table(result$winner==result$first.team)

chisq.test(table(result$winner,result$first.team))


result %>% group_by(year) %>% summarize(mean(first.team=="Home"))
#Pretty consistently, home teams go first

result %>% group_by(year) %>% summarize(mean(winner==first.team))
#Consistent about 50%

#Random samples to manually check the data
gids<-sample(result,6)$gid
result[result$gid%in%gids,]
nhl1[nhl1$gid%in%gids,]


#Visitors have won about 52-53% of shootouts since 0708 season
#Home teams go first in about 78% of SOs
#Team going first has won 49.4% of SOs
#No difference in going first by home/away
