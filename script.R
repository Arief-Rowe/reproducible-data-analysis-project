####READ IN LIBRARY
library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)


##Read in Player stats and salaries
player_stat <- read.csv("data/2018-19_nba_player-statistics.csv")
player_salary <- read.csv("data/2018-19_nba_player-salaries.csv")


##read in Team Stats and salaries
team_stat1 <- read.csv("data/2018-19_nba_team-statistics_1.csv")
team_stat2 <- read.csv("data/2018-19_nba_team-statistics_2.csv")
team_payroll <- read.csv("data/2019-20_nba_team-payroll.csv")




#####CHECKING FOR MISSING VALUES
##checking the money 
sum(is.na(team_payroll)) #no missing values 
sum(is.na(player_salary)) #no missing values


##checking the player Statistics
sum(is.na(player_stat)) #117 missing values within the player_stats dfd
naniar::vis_miss(player_stat) #visual representation of missing values
  #missing values based in shooting percentage. Don't need to remove 


##Checking the Team statistics 
sum(is.na(team_stat1)) #90 missing values
naniar::vis_miss(team_stat1)
  #3 columns the end with NO values, need to remove 
  #We should remove the spare columns

sum(is.na(team_stat2)) #No missing Values 



####DATA TRANSFORMATION
##Remove blank data from team_stat1
team_stat1 <- select(team_stat1, 1:22)
sum(is.na(team_stat1)) #missing values have been dealt with


##combine players that have been on multiple teams
player_stat <- player_stat %>%
  group_by(player_name, Pos, Age) %>%
  summarise(across(4:26, funs(sum)))

player_stat$player_name <- player_stat$player_name %>%
  str_replace_all(pattern = "\\.",
                  replacement = "")

names(player_stat) <- str_replace_all(string = names(player_stat),
                                    pattern = "_sum",
                                    replacement = "")


##rename variable names in player_statistics
player_stat <-rename(player_stat,
                    FG_per = `FG.`, 
                    x3P = `X3P`, 
                    x3PA = `X3PA`, 
                    x3P_per = `X3P.`, 
                    x2P = `X2P`, 
                    x2PA = `X2PA`, 
                    x2P_per = `X2P.`, 
                    eFG_per = `eFG.`, 
                    FT_per = `FT.`)

##rename variable names for team statistics 1
team_stat1 <-rename(team_stat1, 
                           TS_per = `TS.`, 
                           eFG_per = `eFG.`, 
                           TOV_per = `TOV.`, 
                           ORB_per = `ORB.`, 
                           FTpFGA = `FT.FGA`, 
                           DRB_per = `DRB.`)

##rename variable names for team statistics 2
team_stat2 <-rename(team_stat2,
                           FGp = `FG.`, 
                           x3_per = `X3P`, 
                           x3PA = `X3PA`, 
                           x3P_per = `X3P.`, 
                           x2P = `X2P`, 
                           x2PA = `X2PA`, 
                           x2P_per = `X2P.`,
                           FT_per = `FT.`)


####EXPLORATORY ANALYSIS
##checking offensive rating
ggplot(team_stat1, aes(x = ORtg, y = W))+ 
  geom_point(alpha = 0.5, colour = "red")+
  geom_smooth(method = "lm")
    #teams with better offense have higher chance of winning


##defensive rating
ggplot(team_stat1, aes(x = DRtg, y = W))+ 
  geom_point(alpha = 0.5, colour = "red")+
  geom_smooth(method = "lm")
  #let in less than 111 points to win more than 40 points


##Net rating
ggplot(team_stat1, aes(x = NRtg, y = W))+ 
  geom_point(alpha = 0.5, colour = "red")+ 
  geom_smooth(method = "lm")
    #strong positive correlation


##checking turnovers per game 
ggplot(team_stat1, aes(x = TOV_per, y = W))+ 
  geom_point(alpha = 0.5, colour = "red")+ 
  geom_smooth(method = "lm")
  #more turnover results is less wins


##check correlation of Ntrg ~wins
cor(x = team_stat1$NRtg, y = team_stat1$W, method = "pearson")



####So we should take a look at those statistics to see if we get a linear relationship.

##Merge the two team datasets together
team_statistics <- merge(team_stat1,team_stat2, by  = "Team")

##plot pts against NRtg
ggplot(team_statistics, aes(x = PTS, y = NRtg))+ 
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")


##NRgt ~ TRB
ggplot(team_statistics, aes(x = TRB, y = NRtg))+
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")

##NRgt ~ AST
ggplot(team_statistics, aes(x = AST, y = NRtg))+
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")

##NRgt ~ STL
ggplot(team_statistics, aes(x = STL, y = NRtg))+
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")

#NRgt ~ BLK
ggplot(team_statistics, aes(x = BLK, y = NRtg))+
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")

##NRgt ~ TOV
ggplot(team_statistics, aes(x = TOV, y = NRtg))+
  geom_point(alpha = 0.5, colour = "red") + 
  geom_smooth(method = "lm")


#####DATA TRANSFORMATION 

##add new variable for PTS/REB/AST/STL/BLK per game
team_statistics <- team_statistics %>%
  mutate(PTS_PG = PTS / G,
         REB_PG = TRB / G,
         AST_PG = AST / G, 
         STL_PG = STL / G,
         BLK_PG = BLK / G,
         TOV_PG = TOV / G)

####MULTIPLE LINEAR REGRESSION
fit <- lm(NRtg ~ PTS_PG + REB_PG + AST_PG + STL_PG + BLK_PG + TOV_PG, 
          data = team_statistics)
tidy(fit, conf.int = TRUE)
  #use estimate to show if increase/decrease by one, when NRgt for Rmd 


####PREDICTED VALUES
  #predicted values based of the estimates 


#####TACKLING THE ASSUMPTIONS OF LINEAR REGRESSION 

#1. Continuous Response variables
  #NRgt is ratio, therefor a continuous variable

#2. +2 exploratory variable are continuous/categorical
  #all explanatory variables are continuous

#3. Variable independence
car::durbinWatsonTest(fit)
  #D-W stat of 1.237792 been no correlation and they are independent

#4. Linearity between response and explanatory variables
car::avPlots(fit)
  #all variables have a relationship with the response variable

#5. No Outliers
std_res <- rstandard(fit)
points <- 1:length(std_res)

ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point(colour = "black") +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "black", linetype = "dashed")
    #no outliers all within 3 SD from the mean

#6. No Leverage points
hats <- hatvalues(fit)
hats_labels <- if_else(hats >= 0.4, paste(points), "")

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point(colour = "black") +
  geom_text(aes(label = hats_labels), nudge_x =1)
    #points 6,11,21 worth investigating, other than that no leverage points

#7. Cooks Distance (influential points)
cook <-cooks.distance(fit)
cook_labels <- if_else(cook >= 0.2, paste(points), "")

ggplot(data = NULL, aes(x =points, y = cook)) +
  geom_point(colour = "black") +
  geom_text(aes(label = cook_labels), nudge_x =1)
  #point 11 seems to be a point of influence and leverage, need to investigate

  #look against PTS_PG
ggplot(team_statistics, aes(x = PTS_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue") +
  geom_text(aes(label = cook_labels), nudge_y = 0.5)

  #agasint REB_PG
ggplot(team_statistics, aes(x = REB_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue") +
  geom_text(aes(label = cook_labels), nudge_y = 0.5)

  #against AST_PG
ggplot(team_statistics, aes(x = AST_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue") +
  geom_text(aes(label = cook_labels), nudge_y = 0.5)


  #Against STL_PG
ggplot(team_statistics, aes(x = STL_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "Black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)

  #against BLK_PG
ggplot(team_statistics, aes(x = BLK_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)

  #against TOV_PG
ggplot(team_statistics, aes(x = TOV_PG, y = NRtg))+
  geom_point(alpha = 1, colour = "black")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")+
  geom_smooth(method = "lm", colour = "blue") +
  geom_text(aes(label = cook_labels), nudge_y = 0.5)

  #after further invesigate point 11 and 20 are not points of leverage or influence

#7. Homoscedasticity 
res<- residuals(fit)
fitted <- predict(fit)

ggplot(team_statistics, aes(x = fitted, y = res))+
  geom_point(colour = "blue")+
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  geom_smooth(se = FALSE, colour = "black")
  #no clear pattern present, homoscadasticity is present

#8. Normality
ggplot(data =NULL, aes(x= res))+
  geom_histogram( colour = "Black", fill = "Blue", binwidth = 1.5)
    #normally distributed

ggplot(data = NULL, aes(sample =res))+
  stat_qq() + stat_qq_line()
    #Q-Q shows normal distribution as well

#9. Multicollinearity 
pairs(formula = ~ PTS_PG + REB_PG + AST_PG + STL_PG + 
        BLK_PG + TOV_PG, data = team_statistics)
  #no clear pattern between variables, assumption is met

car::vif(fit)
sqrt(car::vif(fit))


#####MODEL TESTING
##expected net rating
team_statistics<- mutate(team_statistics, 
                         exp_NRtg_PG = predict(fit,
                                               newdata = team_statistics))

##exp NRgt ~ NRgt
ggplot(team_statistics, aes(exp_NRtg_PG, NRtg, label = Team)) + 
  geom_point(colour = "black") +
  geom_text(nudge_x = -1.5, cex = 1.75) +
  geom_abline(linetype = "solid", colour = "black")

#exp NRgt ~ wins 
ggplot(team_statistics, aes(x = W, y = exp_NRtg_PG, label = Team)) + 
  geom_point(colour = "black") +
  geom_text(nudge_x = 3.5, cex = 2) +
  geom_abline(linetype = "solid", colour = "black")


####CREATING PLAYER METRICS
##Making a Mintues per Game Metric
min_PG <- team_statistics %>%
  group_by(Team) %>%
  summarise(min_PG = (MP/G)) %>%
  pull(min_PG) %>%
  mean 
min_PG


##create new df for games and key metrics
players <- player_stat %>%
  group_by(player_name) %>%
  summarise(G = sum(MP) / min_PG,
            PTS_PG = sum(PTS) / G,
            REB_PG = sum(TRB) / G,
            AST_PG= sum(AST) / G, 
            STL_PG = sum(STL) / G,
            BLK_PG = sum(BLK) / G,
            TOV_PG = sum(TOV) / G,
            MP = sum(MP)) %>%
  filter(MP >= 200) %>%
  select(-G) 

head(players)


####PREDICTED NET RATING
#new predict variable for expected net rating for each player
players <- mutate(players, exp_NRtg = predict(fit, newdata = players))

players %>%
  ggplot(aes(x = exp_NRtg)) +
  geom_histogram(binwidth = 0.5, colour = "red", fill = "black")


#####ADDING PLAYER POSISTION AND THEIR SALARIES

##adding player salary
players <- player_salary %>%
  select(player_name, salary) %>% 
  right_join(players, by = "player_name")

head(players)

##adding player positions
players <- player_stat %>%
  select(player_name, Pos) %>% 
  right_join(players, by = "player_name")

head(players)


####Player recuitment based on the net rating
players %>%
  select(player_name, Pos, salary, exp_NRtg) %>% 
  arrange(desc(exp_NRtg), salary) %>%
  top_n(30)
    #centres seem to have the best salaries to net rating ratio
    #top being Anthony Davis

##visualization
players %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = Pos))+
  geom_point()+ 
  xlab("Salary (Millions)") +
  ylab("Expected Net Rating")


#####################POINT GUARD
players %>%
  filter(Pos == 'PG') %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_NRtg > 5,as.character(player_name),'')),hjust=0,vjust=-1) +
  expand_limits(x = 60, y = 45)+
  theme(legend.position = "none")+
  xlab("Salary (Millions)")  +
  ylab("Expected Net Rating")




####################SHOOTING GUARD
players %>%
  filter(Pos == 'SG' | Pos == 'SG-SF' | Pos == 'PF-SG' | Pos == 'SG-SF') %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_NRtg > 4,as.character(player_name),'')),hjust=-0.2,vjust=0.5) +
  expand_limits(x = 50)+
  theme(legend.position = "none")+
  xlab("Salary (Millions)")
  


######################SMALL FORWARD
players %>%
  filter(Pos == 'PF-SF' | Pos == 'SF' | Pos == 'SF-SG' | Pos == "SG-SF") %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_NRtg > 10,as.character(player_name),'')),hjust=1.1,vjust=0.5) +
  theme(legend.position = "none")+
  expand_limits(x = -10)+
  xlab("Salary (Millions)") 



######################POWER FORWARD
players %>%
  filter(Pos == 'C-PF' | Pos == 'PF-C' | Pos == 'PF' | Pos == 'PF-SF' | Pos == "SG-PF") %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_NRtg > 30,as.character(player_name),'')),hjust=-0.05,vjust=0.5) +
  expand_limits(x = 35)+
  theme(legend.position = "none")+
  xlab("Salary (Millions)") 




######################CENTRE
players %>%
  filter(Pos == 'C-PF' | Pos == 'PF-C'| Pos == 'C') %>%
  ggplot(aes(x = salary/1000000, y = exp_NRtg, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_NRtg > 45,as.character(player_name),'')),hjust=1.2,vjust=0.5) +
  expand_limits(x = -25)+
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Net Rating")


##########THE STARTING 5
starting_5 <- players %>%
  filter(player_name %in% c("Karl-Anthony Towns", "Giannis Antetokounmpo", "Kawhi Leonard", "Luka Doncic", "Michael Carter-Williams")) %>%
  arrange(desc(salary))

sum(starting_5$salary)

118-63
