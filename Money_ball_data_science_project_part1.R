##### Install The Lahman Package to get all the data 
install.packages("Lahman")
library(Lahman)

data('Batting')
Batting
####Read the first 6 columns 
head(Batting)
print(head(Batting))

####Check the structure of the data frame 
str(Batting)
##### Warning there are few column with NA , keep in mine we mine have to manipulate the data 
head(Batting$AB)

###### Features Engineering ###
## We neeed to add three more statistics that were used in MoneyBall!
### Batting Averahe ##### On BAse Percentage ###### Slugging Percentage 
#### AVG = H/AB for BAttling average 

### Battling Average 
Batting$BA <- Batting$H / Batting$AB
tail(Batting$BA, 5)

#### On Base Percentage ###
######OBP = OBP=(H+BB+HBP)/(AB+BB+HBP+SF)
Batting$OBP <-(Batting$H + Batting$BB + Batting$HBP)/
              (Batting$AB + Batting$BB + Batting$HBP+Batting$SF)
Batting$OBP
tail((Batting$OBP))

##### creating the single before we use Slugging Percentage 
Batting$X1B <- Batting$H - Batting$X2B - Batting$X3B - Batting$HR
####Creating Slugging Average ###

###### SLG =(1B)+(2* 2B)+(3 *3B)+(4 * HR)/ (AB)
Batting$SLG <- ((1* Batting$X1B) + (2 * Batting$X2B) + (3 * Batting$X3B) + (4 + Batting$HR)) /
                Batting$AB
str(Batting)


### Read the salaries table 
data("Salaries")
sal <- Salaries
sal
batting <- subset(Batting,yearID >= 1985)
batting
head(batting)
###### Merge the two data frame 
combo <- merge(batting,sal,by=c('playerID','yearID'))
combo$
summary(combo)
##### How much money have been spend since 1985 to 2016
sum(as.numeric(combo$salary,na.rm= T))

##### Create a Data. Frame call lost.player
lost_player <- subset(combo, playerID %in% c('gianbja01','damonjo01','saenzol01'))
lost_player
##### find the  Lost player in 2001 ## using the same lost data.frame 
library(dplyr)
lost_players_2001 <- filter(lost_player, yearID == 2001)
lost_players_2001
head(lost_players_2001)
lost_players_2001 <- lost_players_2001[c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
lost_players_2001
###### Find replacement players for player who were lost during transfer windows 
###### How to find over value players 
#### 1469 AB
# AVG 0.364 OBP and 15 millions for all undervalue players 
combo <- subset(combo, yearID == 2001)
combo
str(combo)
#### another way to solve this problem by us
library(ggplot2)
ggplot(combo, aes(x=OBP,y=salary)) + geom_point(size=2)
##### by viewing the plot we only need track down player who have OBP > 0 and salary < 8 millions 
### remake the combo data.frame so we could specify what we excatly want 
combo_1 <- subset(combo, salary < 8000000 & OBP > 0)
combo
ggplot(combo_1, aes(x=OBP,y=salary)) + geom_point(size=2)
##### we divide the 1469/3 = 489.66 . we can use AB > 450 AB 
combo_2 <- subset(combo_1, AB >= 450)
combo_2
str(combo_2)
combo$playerID
##### arrange it by base percentage 
head(arrange(combo_2,desc(OBP)), 10)
option <- head(arrange(combo_2,desc(OBP)), 10)
sd <- option[,c('playerID','AB','salary','OBP')]
##### choose from option where you can find the three replacement player 
sum(head(sd$salary), 3:6)  ### using column row to 6

####################
##### part 2 

##### you can use sql or dplyr to do part 2 
#### i am using dplyr to get all the answer 
##The first thing that we have to do is load our data. We will do this using R studio packages function 
Batting
str(Batting)
library(RSQLite)
####lets calculate total payroll per year for the Americal League (AL)
sal <- Salaries

library(Lahman)       
data("LahmanData")
library(sqldf)
library(tidyr)
gather(sal,total_payroll,sum(salary))
