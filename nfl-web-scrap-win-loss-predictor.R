library(Metrics) 
library(tidyverse)
library(caret)
library(data.table)
library(stats)
library(rvest)

# Prediciting Wins and Loses for NFL Teams based on Points Scored and Points Allowed, using 2019 data to train the model and make predictions for the 2020 NFL Season 

#Then adding Total Team Turnovers and Takeaways to see if it improves our linear model for determining Wins/Losses for the 2019 and 2020 seasons

#make call to the website you want to parse data from
url <- "https://www.pro-football-reference.com/years/2019/index.htm"
#read in the html
h <- read_html(url)
#check to make sure the class is right
class(h)
#get the table element from the html 
tab <- h %>% html_nodes("table")
# read the two tables in (one for AFC and one for NFC) using html_table()
afc <- html_table(tab[1])
nfc <- html_table(tab[2])
# convert to data frames
afc <- as.data.frame(afc)
nfc <- as.data.frame(nfc)
# get rid of the extra division names in the table and for some reason the OR function is not working
afc <- afc %>% filter(Tm!="AFC North") %>% 
  filter(Tm!="AFC East") %>% 
  filter(Tm!="AFC South") %>% 
  filter(Tm!="AFC West")
nfc <- nfc %>% filter(Tm!="NFC North") %>% 
  filter(Tm!="NFC East") %>% 
  filter(Tm!="NFC South") %>% 
  filter(Tm!="NFC West")
#combine the afc and nfc data frames
nfl_df <- rbind(afc,nfc)

# now we have all the NFL teams into one data fram from the two html tables before

#get rid of special characters and spaces in team names 
nfl_df$Tm<- str_replace_all(nfl_df$Tm, "[^[:alnum:]]", "")

#need to replace washington and las vegas raider team names
nfl_df$Tm<- str_replace_all(nfl_df$Tm, "OaklandRaiders", "LasVegasRaiders")
nfl_df$Tm<- str_replace_all(nfl_df$Tm, "WashingtonRedskins", "WashingtonFootballTeam")

# convert the strings to integers for the columns being analyzed
cols.num <- c("W","L","PF","PA","SoS")
nfl_df[cols.num] <- sapply(nfl_df[cols.num],as.numeric)

# now lets run a linear regression to see how significant certain stats are in determining games won and loss

nfl_lm <- lm(W ~ PF + PA , data = nfl_df)
# with points for and points against we get significant P values so lets continue to predicting wins
summary(nfl_lm)
#set up new data for the prediciton
newdata<- data.frame(PF = nfl_df$PF,PA= nfl_df$PA)
#predict using nfl linear model above and newdata
w_pred <- predict(nfl_lm,newdata)

# calculate the root mean squared error to see how close out prediction was out of prediciting wins in out of 16 games 
RMSE(nfl_df$W,w_pred) # 1.53

# now to run a linear model for projected loses
nfl_lm_l <- lm(L ~ PF + PA , data = nfl_df)#1.47

# from the summary we get simmilar p value that are significant
summary(nfl_lm_l)

# use the same new data as before to make the prediction
l_pred <- predict(nfl_lm_l,newdata)
# here we get a better prediction of losses for the season the wins with the lower RMSE
RMSE(nfl_df$L,l_pred) # 1.47
nfl_df

# this is to see how some of the predicted wins and losses match up to the actuals
compare<- nfl_df %>% select(Tm,W,L,PF,PA)
compare$w_p <- w_pred
compare$l_p <- l_pred
head(compare,n=10)

# now lets used our linear models from the 2019 season and test it on teams in the 2020 nfl season to see if it can predict
#the win losses for this year

# repeat the same data processes as above for 2020

#make call to the website you want to parse data from
url <- "https://www.pro-football-reference.com/years/2020/index.htm"
#read in the html
h <- read_html(url)
#check to make sure the class is right
class(h)
#get the table element from the html 
tab <- h %>% html_nodes("table")
# read the two tables in (one for AFC and one for NFC) using html_table()
afc2 <- html_table(tab[1])
nfc2 <- html_table(tab[2])
# convert to data frames
afc2 <- as.data.frame(afc2)
nfc2 <- as.data.frame(nfc2)
# get rid of the extra division names in the table and for some reason the OR function is not working
afc2 <- afc2 %>% filter(Tm!="AFC North") %>% 
  filter(Tm!="AFC East") %>% 
  filter(Tm!="AFC South") %>% 
  filter(Tm!="AFC West")
nfc2 <- nfc2 %>% filter(Tm!="NFC North") %>% 
  filter(Tm!="NFC East") %>% 
  filter(Tm!="NFC South") %>% 
  filter(Tm!="NFC West")
#combine the afc and nfc data frames, 2 is for the 2020 data 
nfl_df2 <- rbind(afc2,nfc2)
#getting rid of the special characters and spaces in the first column for teams 
nfl_df2$Tm<- str_replace_all(nfl_df2$Tm, "[^[:alnum:]]", "")

# now instead of creating linear models from this data, we are just going to create new datasets to be analyzed

## convert the strings to integers for the columns being analyzed
cols.num <- c("W","L","PF","PA","SoS")
nfl_df2[cols.num] <- sapply(nfl_df2[cols.num],as.numeric)

# newdata with 2020 stats
newdata2 <- data.frame(PF = nfl_df2$PF,PA= nfl_df2$PA)
# running the predicted wins with the 2019 trained model on the 2020 data 
w_pred2 <- predict(nfl_lm,newdata2)
RMSE(nfl_df2$W,w_pred2) # 1.49 , a decrease by .03

# now to predict the losses
l_pred2 <- predict(nfl_lm_l,newdata2)
RMSE(nfl_df2$L,l_pred2) #1.45  an improvment of .02

# seeing how the predicted wins and losses match up to the actuals 
compare2020 <- nfl_df2 %>% select(Tm,W,L,PF,PA)
compare2020$w_p <- w_pred2
compare2020$l_p <- l_pred2
head(compare2020,n=10)

# now we have a good model from 2019, but I want to add offensive/defensive turnovers to see if that effects wins losses any more

#download the csv files from https://www.pro-football-reference.com/years/2019/index.htm, copy and paste the csv data to local files on your computer

# using 2019 data to do training again

# load in the csv with the pathways of the files on you computer 
off_19 <- read.csv("C:/Users/dom/Documents/education/data-science/r/nfl-analysis/off-data-2019.csv",header = T)

def_19 <- read.csv("C:/Users/dom/Documents/education/data-science/r/nfl-analysis/def-data-2019.csv",header = T)

# change column names to the first row
colnames(off_19)<-off_19[1,]
colnames(def_19)<-def_19[1,]
# remove the first row
off_19 <- off_19[-1,]
def_19 <- def_19[-1,]

#remove spaces in Tm (team) to make sure we can use it to join the team win/loss data
off_19$Tm <-  str_replace_all(off_19$Tm,"[^[:alnum:]]", "")
def_19$Tm <-  str_replace_all(def_19$Tm,"[^[:alnum:]]", "")
head(off_19)
head(def_19)

# now we are going to get the turn over data data from the offense and defense by sub setting the data frames into select columns, then joining them
off_to <- off_19 %>% select(Tm,TO)
def_to <- def_19 %>% select(Tm,TO)
TO_df <- inner_join(off_to,def_to,by = "Tm")
# here TO.x is offensive turnover lost, TO.y is defensive takeaways
colnames(TO_df) <- c("Tm","TO","TA")

#make sure numeric columns are numbers 
cols.num <- c("TO","TA")
TO_df[cols.num] <- sapply(TO_df[cols.num],as.numeric)

# now lets join this data with the original NFL data frame of the 2019 data 
nfl_df3 <- inner_join(nfl_df,TO_df,by = "Tm")
head(nfl_df3)

#now we have those, lets see lets run a linear regression to see if its significant enough to effect wins/losses
tot_lm <- lm(W ~ PF + PA + TO + TA, data=nfl_df3)
summary(tot_lm)

#so turn overs and takeaways have somewhat of a significant effect on the linear model, lets see if it improves our RMSE
newdata3 <- data.frame(PF = nfl_df3$PF,PA=nfl_df3$PA,TO = nfl_df3$TO,TA = nfl_df3$TA)
predict_w <- predict(tot_lm,newdata3)

RMSE(nfl_df3$W,predict_w) # 1.27, a significant improvment to our model, the first model being 1.53  

# now with losses 
totL_lm <- lm(L ~ PF + PA + TO + TA, data=nfl_df3)
summary(totL_lm)

predict_l <- predict(totL_lm,newdata3)
RMSE(nfl_df3$L,predict_l) # 1.17, an even bigger improvement in accuracy, by .3 after 1.47 in the original linear model

# here we improved lose predictability by .3 than before

# copy the 2020 csv files to a local directory and access them

# set the data up to perform it on the 2020 teams 
off_20 <- read.csv("C:/Users/dom/Documents/education/data-science/r/nfl-analysis/off-2020.csv",header = T)

def_20 <- read.csv("C:/Users/dom/Documents/education/data-science/r/nfl-analysis/def-2020.csv",header = T)

# change column names to the first row
colnames(off_20)<-off_20[1,]
colnames(def_20)<-def_20[1,]
# remove the first row
off_20 <- off_20[-1,]
def_20 <- def_20[-1,]
#remove spaces in Tm to make sure we can use it to join the team win/loss data
off_20$Tm <-  str_replace_all(off_20$Tm,"[^[:alnum:]]", "")
def_20$Tm <-  str_replace_all(def_20$Tm,"[^[:alnum:]]", "")

#select the 2020 turnover data
off_to2 <- off_20 %>% select(Tm,TO)
def_to2 <- def_20 %>% select(Tm,TO)
# join the offense and def tables into one table 
TO_df2 <- inner_join(off_to2,def_to2,by = "Tm")

# here TO.x is offensive turnover lost, TO.y is defensive takeaways
colnames(TO_df2) <- c("Tm","TO","TA")

#make sure columns are numbers 
cols.num <- c("TO","TA")
TO_df2[cols.num] <- sapply(TO_df2[cols.num],as.numeric)

# now lets join this data with the original NFL data frame

nfl_df4 <- inner_join(nfl_df2,TO_df2,by = "Tm")

# create a new data frame to make predictions from with the 2020 data 
newdata4 <- data.frame(PF = nfl_df4$PF,PA=nfl_df4$PA,TO = nfl_df4$TO,TA = nfl_df4$TA)
# predict the 2020 games with the 2019 linear model that includes turnover and takeaway variables
predict_w2 <- predict(tot_lm,newdata4)
RMSE(nfl_df4$W,predict_w2) # 1.50 rmse, a slight increase instead of a significant decrease like seen before when modeling against the 2019 data, .07 increast from original

predict_l2 <- predict(totL_lm,newdata4)
RMSE(nfl_df4$L,predict_l2)# 1.52 rmse, a slight increase unfortunatly, it seems that with the 2020 season, turnovers and takeaways do not matter as much in determining wins 

# this shows that both the RMSEs got worse when adding the turn over and take away data, so when it comes to predicting the next season win losses with 2019 regression,
# its about .1 rsme closer using just Points for and Points against, but using it on the 2019 season we see a lower RMSEs, meaning accuracy was improved 

## to see if the turnover and takeaway affect Wins and losses for 2020, we'll create a 5th linear model to see if its better than the 2019 PF PA linear model

lm2020 <- lm(W ~ PF + PA + TA + TO, data = nfl_df4)
summary(lm2020)

anova(nfl_lm,tot_lm)
anova(nfl_lm_l,totL_lm)
# running the linear regression for this season, we see that turnovers do not exactly effect wins and losses it seems like it has in the past, with very hight P values that are surprising , interesting, much less significant P values than 2019
# , Turnovers and takeaways to not be a factor in determining overall season wins and losses in 2020, unlike 2019


