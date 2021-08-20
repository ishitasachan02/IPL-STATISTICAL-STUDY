

library(ggplot2)
library(readr) 
library(dplyr)
library(gridExtra)
library(treemap)
library(RColorBrewer)
library(tidyr)
library(radarchart)
system("ls C:/Users/ishit/Downloads/input")
deliveries<-read.csv("C:\\Users\\ishit\\Downloads\\deliveries.csv")
matches<-read.csv("C:\\Users\\ishit\\Downloads\\matches.csv")

matches<-matches[matches$result=="normal",]
matches[which(as.character(matches$team2)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team2)==as.character(matches$winner)),"team1"]
matches[which(as.character(matches$team1)==as.character(matches$winner)),"loser"]<- matches[which(as.character(matches$team1)==as.character(matches$winner)),"team2"]
matches1<-matches[matches$win_by_runs!=0,]
closeness<-function(x,y = "gold" ){
data1<-matches1[matches1$winner==x|matches1$loser==x,]
data1[data1$loser==x,"win_by_runs"]<- -data1[data1$loser==x,"win_by_runs"]
ggplot(data1,aes(1:nrow(data1),win_by_runs))+ geom_area(fill=y)+ggtitle(x)+ylab("Runs")+ xlab("Matches")+ geom_ribbon(aes(ymin=-5, ymax=5),fill="red",alpha=0.4) +geom_ribbon(aes(ymin=-15, ymax=15),fill="red",alpha=0.1)+guides(fill=FALSE)+scale_alpha(guide = 'none')+coord_cartesian(ylim = c(-100, 100))}
a<-closeness("Chennai Super Kings")
b<-closeness("Kolkata Knight Riders","purple")
c<-closeness("Sunrisers Hyderabad","orange")
d<-closeness("Mumbai Indians","blue2")
e<-closeness("Royal Challengers Bangalore","red3")
f<-closeness("Delhi Daredevils","firebrick3")
g<-closeness("Rajasthan Royals","blueviolet")
h<-closeness("Kings XI Punjab","salmon")
grid.arrange(a,b,c,e,d,f,g,h,ncol=2)

#number of matches played by each team
ggplot(as.data.frame(table(matches$team2) + table(matches$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) +geom_bar(stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Teams")+
  ylab("Number of Matches") +guides(fill=FALSE)

#number of matches won by teams
ggplot(matches,aes(winner)) +geom_bar(fill="#0072B2") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
ylab("Matches won")


#number of matches played in  city
ggplot(matches[which(!is.na(matches$city)),],aes(city,fill= city,rm.na=T)) +geom_bar() +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylab("Number of Matches Played") +guides(fill=FALSE)

#Is winning the toss really an advantage?

matches$toss_match<-ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost")
ggplot(matches[which(!is.na(matches$toss_match)),],aes(toss_match, fill = toss_match))+ geom_bar()+ xlab("Toss") +ylab("Number of matches won")+ ggtitle("How much of a advantage is winning the toss")

#top run getter performance

df<-deliveries %>% filter(batsman=="V Kohli"| batsman=="SK Raina" |batsman=="RG Sharma"|batsman=="G Gambhir")  %>% 
 group_by(batsman,bowling_team) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs >100)
  treemap(df, #Your data frame object
          index=c("batsman","bowling_team"),  #A list of your categorical variables
          vSize = "runs", 
          vColor = "bowling_team",
          type="categorical", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(12,"Set3"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 15,
          fontfamily.title = "serif",
          fontfamily.labels = "symbol",
          title = "Runs against diff teams",
          aspRatio = 1,
          border.col="#FFFFFF",bg.labels = "#FFFFFF" ,fontcolor.labels= "black",fontsize.legend = 0
  )


bowl<-function(type){
	x<-df1 %>% 
		filter(bowler==type & dismissal_kind %in% c("caught","bowled","lbw","stumped","caught and bowled", "hit wicket")) %>% 
		group_by(dismissal_kind) %>% 
		summarise(total = n()) %>% 
		arrange(desc(total)) %>% 
		top_n(n= 10, wt = total)
	p<- ggplot(aes(x = reorder(dismissal_kind, -total), y= total), data = x)+
		geom_bar(aes(fill= dismissal_kind), stat = "identity")+
		labs(list(title = type, x = "Dismissal Kind", y = "Total Wickets"))+
        theme(axis.text.x=element_text(angle=75, hjust=1), plot.title = element_text(size = 8, face = "bold"),text = element_text(size=8))
	return(p)
}

#is home advantage really a thing

Data<-matches[matches$season!="2009",]
Data$date<- as.Date(Data$date)
Data1<-Data[Data$date < as.Date("2014-04-16") | Data$date > as.Date("2014-04-30"),]
Data1$home_team[Data1$city=="Bangalore"]<- "Royal Challengers Bangalore"
Data1$home_team[Data1$city=="Chennai"]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Delhi"]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Chandigarh"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Jaipur"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Mumbai"]<- "Mumbai Indians"
Data1$home_team[Data1$city=="Kolkata"]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Kochi"]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season <=2012]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season >2012]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ahmedabad"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Dharamsala"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Visakhapatnam" & Data1$season== 2015]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ranchi" & Data1$season== 2013]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Ranchi" & Data1$season > 2013]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Rajkot" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Kanpur" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Raipur" ]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Nagpur" ]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Indore" ]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Pune" & Data1$season!= 2016]<- "Pune Warriors"
Data1$home_team[Data1$city=="Pune" & Data1$season== 2016]<- "Rising Pune Supergiants"
Data1<-Data1[ which(!is.na(Data1$home_team)),]
Data1$win_host <- ifelse(as.character(Data1$winner)==as.character(Data1$home_team),"Home","Away")

ggplot(Data1[which(!is.na(Data1$win_host)),],aes(win_host,fill= win_host))+geom_bar()+
  ggtitle("Is home advantage a real thing in IPL?")+
  xlab("Team")+
  ylab("Number of Matches won")+labs(aesthetic="Winner")

#number of match won by each team 
ggplot(matches,aes(winner)) +geom_bar(fill="#0072B2") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
ylab("Matches won")

#TOP BATSMEN
df<- deliveries %>% group_by(batsman)%>% summarise(runs=sum(batsman_runs)) %>% arrange(desc(runs)) %>%
  filter(runs > 3000) 
  df %>% ggplot(aes(reorder(batsman,-runs),runs,fill=batsman)) +geom_bar(stat = "identity") +xlab("Batsman")+ ylab("Runs")+
  theme(axis.text.x = element_text(angle = 90, hjust = 3))+ xlab("Player")+ ggtitle("Top Batsmen")+ guides(fill=F)

#Top Bowlers
df<-deliveries %>% group_by(bowler) %>% filter(player_dismissed!="") %>% summarise(wickets= length(player_dismissed)) %>% top_n(n=10,wt=wickets) 
  df %>% ggplot(aes(reorder(bowler,-wickets),wickets,fill=bowler))+geom_bar(stat = "identity") + ylab("Wickets")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Player")+ ggtitle("Top Bowlers")+ guides(fill=F)

#Total number of runs scored in each over of the innings

df <- deliveries %>% group_by(over) %>% filter(is_super_over==0) %>% summarise(Runs= sum(total_runs))
  print(df)
  df %>% ggplot(aes(over,Runs,fill=over))+geom_bar(stat = "identity")+scale_x_continuous(breaks = 1:20)+ 
   guides(fill=F) +xlab("Over") + ylab("Total runs scored") + ggtitle("Total number of runs scored in each over of the innings")





#run scored by top getter against vrious bowlers

Warner<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="DA Warner") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=50,wt=runs)
Rohit<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="RG Sharma") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=50,wt=runs)
Gambhir<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="G Gambhir") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=50,wt=runs)
Raina<- deliveries %>% group_by(batsman,bowler) %>% filter(batsman=="SK Raina") %>% summarise(runs=sum(batsman_runs)) %>% top_n(n=50,wt=runs)
treemap(Warner, #Your data frame object
          index=c("batsman","bowler"),  #A list of your categorical variables
          vSize = "runs",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(7,"Reds"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 12,
          fontfamily.title = "serif",
          fontfamily.labels = "symbol",
          title = "Runs by David Warner against different bowlers",
          fontface.labels = "bold",
          border.col="#FFFFFF",
          fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
          aspRatio= 1.1
 )
treemap(Rohit, #Your data frame object
          index=c("batsman","bowler"),  #A list of your categorical variables
          vSize = "runs",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(7,"Blues"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 12,
          fontfamily.title = "serif",
          fontfamily.labels = "symbol",
          title = "Runs by RG Sharma against different bowlers",
          fontface.labels = "bold",
           border.col="#FFFFFF",
           fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
          aspRatio= 1.1
 )
treemap(Gambhir, #Your data frame object
          index=c("batsman","bowler"),  #A list of your categorical variables
          vSize = "runs",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(4,"Purples"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 12,
          fontfamily.title = "serif",
          fontfamily.labels = "symbol",
          title = "Runs by G Gambhir against different bowlers",
          fontface.labels = "bold",
          border.col="#FFFFFF",
          fontsize.legend = 0,bg.labels = "black",fontcolor.labels= "#FFFFFF",
          aspRatio= 1.1
 )
treemap(Raina, #Your data frame object
          index=c("batsman","bowler"),  #A list of your categorical variables
          vSize = "runs",  #This is your quantitative variable
          type="index", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(3,"YlOrBr"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 12,
          fontfamily.title = "serif",
          fontfamily.labels = "symbol",
          title = "Runs by SK Raina against different bowlers",
          fontface.labels = "bold",
          border.col="#FFFFFF",
          fontsize.legend = 0,
          bg.labels = "black",fontcolor.labels= "#FFFFFF",
           aspRatio= 1.1
 )


#Type of Dismissals

df<-deliveries %>% 
    filter(player_dismissed=="V Kohli"| player_dismissed=="SK Raina" |player_dismissed=="RG Sharma"|player_dismissed=="G Gambhir") %>%
    group_by(player_dismissed,dismissal_kind) %>% summarise(type= length(dismissal_kind))
  
  treemap(df, #Your data frame object
          index=c("player_dismissed","dismissal_kind"),  #A list of your categorical variables
          vSize = "type", 
          vColor = "dismissal_kind",
          type="categorical", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(7,"Set1"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 15,
          fontfamily.title = "serif",
          fontfamily.labels = "bold",
          title = "Type of Dismissals ",
          aspRatio = 1,
          border.col="#FFFFFF",bg.labels = "black" ,fontcolor.labels= "#FFFFFF",fontsize.legend = 0
  )

#Batting with whom have our top run getters been successful?

df<-deliveries %>% filter(batsman=="DA Warner"| batsman=="GJ Maxwell" |batsman=="MS Dhoni"|batsman=="G Gambhir")  %>% 
    group_by(batsman,non_striker) %>% summarise(runs = sum(batsman_runs)) %>% filter(runs >100)
  treemap(df, #Your data frame object
          index=c("batsman","non_striker"),  #A list of your categorical variables
          vSize = "runs", 
          vColor = "batsman",
          type="categorical", #Type sets the organization and color scheme of your treemap
          palette = brewer.pal(5,"Set2"),  #Select your color palette from the RColorBrewer presets or make your own.
          fontsize.title = 19,
          fontfamily.title = "serif",
          fontfamily.labels = "bold",
          title = "Runs with different players at the other end ",
          aspRatio = 1,
          border.col="#FFFFFF",bg.labels = "black" ,fontcolor.labels= "#FFFFFF",fontsize.legend = 0
  )
#inning progression

deliveries %>% filter(batsman=="MS Dhoni") %>%
    group_by(match_id) %>% 
    mutate(cum_run= cumsum(batsman_runs),cum_ball=1:length(match_id)) %>%
    ggplot(aes(cum_ball,cum_run,col= as.factor(batsman_runs))) + 
    geom_point()  + labs(col="Type of Runs") + xlab("Balls") + ylab("Runs")+
    ggtitle("Innings Progression- MS Dhoni") +  coord_cartesian(ylim = c(0, 120))