#set working directory
setwd("C:/Users/alevitt/Desktop/Data Science/Projects/Hearthstone")

#read in data file
WarriorJan2017 <- read.csv("WarriorJan2017.csv", stringsAsFactors = FALSE)

#loading packages
library(car)
library(tidyr)
library(dplyr)
library(ggplot2)

#checking data
str(WarriorJan2017)
View(WarriorJan2017)

#converting class win % vars from character to numeric
WarriorJan2017$DruidWinPct <- as.numeric(WarriorJan2017$DruidWinPct)
WarriorJan2017$HunterWinPct <- as.numeric(WarriorJan2017$HunterWinPct)
WarriorJan2017$MageWinPct <- as.numeric(WarriorJan2017$MageWinPct)
WarriorJan2017$PaladinWinPct <- as.numeric(WarriorJan2017$PaladinWinPct)
WarriorJan2017$PriestWinPct <- as.numeric(WarriorJan2017$PriestWinPct)
WarriorJan2017$RogueWinPct <- as.numeric(WarriorJan2017$RogueWinPct)
WarriorJan2017$ShamanWinPct <- as.numeric(WarriorJan2017$ShamanWinPct)
WarriorJan2017$WarlockWinPct <- as.numeric(WarriorJan2017$WarlockWinPct)
WarriorJan2017$WarriorWinPct <- as.numeric(WarriorJan2017$WarriorWinPct)

#converting ordinal rank var to continuous
WarriorJan2017$contRank <- recode(WarriorJan2017$Rank, 
                                  "17.0 = 17.75; 17.1 = 17.5; 17.2 = 17.25; 17.3 = 17.0;
                                   16.0 = 16.75; 16.1 = 16.5; 16.2 = 16.25; 16.3 = 16.0;
                                   15.0 = 15.8; 15.1 = 15.6; 15.2 = 15.4; 15.3 = 15.2; 15.4 = 15.0; 
                                   14.0 = 14.8; 14.1 = 14.6; 14.2 = 14.4; 14.3 = 14.2; 14.4 = 14.0;
                                   13.0 = 13.8; 13.1 = 13.6; 13.2 = 13.4; 13.3 = 13.2; 13.4 = 13.0;
                                   12.0 = 12.8; 12.1 = 12.6; 12.2 = 12.4; 12.3 = 12.2; 12.4 = 12.0;
                                   11.0 = 11.8; 11.1 = 11.6; 11.2 = 11.4; 11.3 = 11.2; 11.4 = 11.0;
                                   10.0 = 10.85; 10.1 = 10.68; 10.2 = 10.51; 10.3 = 10.34; 10.4 = 10.17; 10.5 = 10.0; 
                                   9.0 = 9.85; 9.1 = 9.68; 9.2 = 9.51; 9.3 = 9.34; 9.4 = 9.17; 9.5 = 9.0;
                                   8.0 = 8.85; 8.1 = 8.68; 8.2 = 8.51; 8.3 = 8.34; 8.4 = 8.17; 8.5 = 8.0;
                                   7.0 = 7.85; 7.1 = 7.68; 7.2 = 7.51; 7.3 = 7.34; 7.4 = 7.17; 7.5 = 7.0;
                                   6.0 = 6.85; 6.1 = 6.68; 6.2 = 6.51; 6.3 = 6.34; 6.4 = 6.17; 6.5 = 6.0;
                                   5.0 = 5.85; 5.1 = 5.68; 5.2 = 5.51; 5.3 = 5.34; 5.4 = 5.17; 5.5 = 5.0;
                                   4.0 = 4.85; 4.1 = 4.68; 4.2 = 4.51; 4.3 = 4.34; 4.4 = 4.17; 4.5 = 4.0;
                                   3.0 = 3.85; 3.1 = 3.68; 3.2 = 3.51; 3.3 = 3.34; 3.4 = 3.17; 3.5 = 3.0;
                                   2.0 = 2.85; 2.1 = 2.68; 2.2 = 2.51")


#counting number of games played per class
totalVsDruid <- (sum(WarriorJan2017$DruidWin, na.rm = TRUE)
                  + sum(WarriorJan2017$DruidLoss, na.rm = TRUE))

totalVsHunter <- (sum(WarriorJan2017$HunterWin, na.rm = TRUE)
                  + sum(WarriorJan2017$HunterLoss, na.rm = TRUE))

totalVsMage <- (sum(WarriorJan2017$MageWin, na.rm = TRUE)
                  + sum(WarriorJan2017$MageLoss, na.rm = TRUE))

totalVsPaladin <- (sum(WarriorJan2017$PaladinWin, na.rm = TRUE)
                  + sum(WarriorJan2017$PaladinLoss, na.rm = TRUE))

totalVsPriest <- (sum(WarriorJan2017$PriestWin, na.rm = TRUE)
                  + sum(WarriorJan2017$PriestLoss, na.rm = TRUE))

totalVsRogue <- (sum(WarriorJan2017$RogueWin, na.rm = TRUE)
                  + sum(WarriorJan2017$RogueLoss, na.rm = TRUE))

totalVsShaman <- (sum(WarriorJan2017$ShamanWin, na.rm = TRUE)
                  + sum(WarriorJan2017$ShamanLoss, na.rm = TRUE))

totalVsWarlock <- (sum(WarriorJan2017$WarlockWin, na.rm = TRUE)
                  + sum(WarriorJan2017$WarlockLoss, na.rm = TRUE))

totalVsWarrior <- (sum(WarriorJan2017$WarriorWin, na.rm = TRUE)
                  + sum(WarriorJan2017$WarriorLoss, na.rm = TRUE))


#Total Games vs. Rank line smooth plot
ggplot(WarriorJan2017, aes(x = Game, y = contRank)) +
  geom_line() +
  geom_smooth() +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800)) +
  scale_y_reverse(breaks = c(17:2)) +
  labs(x = "Games", y = "Rank") +
  ggtitle("Rank Trajectory (January 2017 Season)") +
  theme(
    text = element_text(size = 15),
    plot.title = element_text(face = "bold", hjust = c(0,0)))


#creating overall win % by class dataframe
classWinPct <- tail(WarriorJan2017, 1)
classWinPct <- select(classWinPct, contains("WinPct"))
classWinPct <- rename(classWinPct, Overall = TotalWinPct, 
                      Druid = DruidWinPct,
                      Hunter = HunterWinPct,
                      Mage = MageWinPct,
                      Paladin = PaladinWinPct,
                      Priest = PriestWinPct,
                      Rogue = RogueWinPct,
                      Shaman = ShamanWinPct,
                      Warlock = WarlockWinPct,
                      Warrior = WarriorWinPct)
classWinPctLong <- gather(classWinPct, Class, Percentage)
classWinPctLong$Percentage <- round(classWinPctLong$Percentage, 2)

#creating value var for color mapping
classWinPctLong <- mutate(classWinPctLong, 
                          value = ifelse(Percentage < 0.51, 1, 
                                         ifelse(Percentage == 0.51, 2, 3)))
classWinPctLong$value <- as.factor(classWinPctLong$value)

#Overall win % by class plot
ggplot(classWinPctLong, aes(x = Class, y = Percentage, fill = value)) +
  geom_bar(stat = "identity", col = "black") +
  coord_flip() +
  ggtitle("Win Rates Versus Class (January 2017 Season)") +
  scale_fill_brewer(palette = "RdBu") +
  scale_x_discrete(
    limits = c("Warrior", "Shaman", "Warlock", "Mage", "Paladin",
               "Overall", "Druid", "Rogue", "Priest", "Hunter")) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    text = element_text(size = 15),
    plot.title = element_text(face = "bold", hjust = c(0,0))) +
  annotate("text", x = "Druid", y = 0.05, label = "n = 51", hjust = 0) +
  annotate("text", x = "Hunter", y = 0.05, label = "n = 5", hjust = 0) +
  annotate("text", x = "Mage", y = 0.05, label = "n = 70", hjust = 0) +
  annotate("text", x = "Overall", y = 0.05, label = "n = 771", hjust = 0) +
  annotate("text", x = "Paladin", y = 0.05, label = "n = 16", hjust = 0) +
  annotate("text", x = "Priest", y = 0.05, label = "n = 90", hjust = 0) +
  annotate("text", x = "Rogue", y = 0.05, label = "n = 102", hjust = 0) +
  annotate("text", x = "Shaman", y = 0.05, label = "n = 220", hjust = 0) +
  annotate("text", x = "Warlock", y = 0.05, label = "n = 85", hjust = 0) +
  annotate("text", x = "Warrior", y = 0.05, label = "n = 132", hjust = 0)


#creating class prevalence dataframe
classMetaPct <- tail(WarriorJan2017, 1)
classMetaPct <- select(classMetaPct, contains("Meta"))
classMetaPct <- rename(classMetaPct,  
                       Druid = DruidMetaPct,
                       Hunter = HunterMetaPct,
                       Mage = MageMetaPct,
                       Paladin = PaladinMetaPct,
                       Priest = PriestMetaPct,
                       Rogue = RogueMetaPct,
                       Shaman = ShamanMetaPct,
                       Warlock = WarlockMetaPct,
                       Warrior = WarriorMetaPct)
classMetaPctLong <- gather(classMetaPct, Class, Percentage)
classMetaPctLong$Percentage <- round(classMetaPctLong$Percentage, 2)
classMetaPctLong$rankGroup <- 1

#Class prevalence plot
ggplot(classMetaPctLong, aes(x = Class, y = Percentage)) +
  geom_bar(stat = "identity", fill = "sky blue", col = "black") +
  coord_flip() +
  ggtitle("Class Prevalence in January 2017 Meta") +
  scale_x_discrete(
    limits = c("Hunter", "Paladin", "Druid", "Mage", "Warlock",
               "Priest", "Rogue", "Warrior", "Shaman")) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    text = element_text(size = 15),
    plot.title = element_text(face = "bold", hjust = c(0,0)))


#creating rank 17-11 class prevalence dataframe

rank17_11 <- filter(WarriorJan2017, contRank >= 11)

druid17_11 <- (sum(rank17_11$DruidWin, na.rm = TRUE) + sum(rank17_11$DruidLoss, na.rm = TRUE))/nrow(rank17_11)
hunter17_11 <- (sum(rank17_11$HunterWin, na.rm = TRUE) + sum(rank17_11$HunterLoss, na.rm = TRUE))/nrow(rank17_11)
mage17_11 <- (sum(rank17_11$MageWin, na.rm = TRUE) + sum(rank17_11$MageLoss, na.rm = TRUE))/nrow(rank17_11)
paladin17_11 <- (sum(rank17_11$PaladinWin, na.rm = TRUE) + sum(rank17_11$PaladinLoss, na.rm = TRUE))/nrow(rank17_11)
priest17_11 <- (sum(rank17_11$PriestWin, na.rm = TRUE) + sum(rank17_11$PriestLoss, na.rm = TRUE))/nrow(rank17_11)
rogue17_11 <- (sum(rank17_11$RogueWin, na.rm = TRUE) + sum(rank17_11$RogueLoss, na.rm = TRUE))/nrow(rank17_11)
shaman17_11 <- (sum(rank17_11$ShamanWin, na.rm = TRUE) + sum(rank17_11$ShamanLoss, na.rm = TRUE))/nrow(rank17_11)
warlock17_11 <- (sum(rank17_11$WarlockWin, na.rm = TRUE) + sum(rank17_11$WarlockLoss, na.rm = TRUE))/nrow(rank17_11)
warrior17_11 <- (sum(rank17_11$WarriorWin, na.rm = TRUE) + sum(rank17_11$WarriorLoss, na.rm = TRUE))/nrow(rank17_11)

rank17_11wide <- as.data.frame(cbind(druid17_11, hunter17_11, mage17_11, paladin17_11, priest17_11, rogue17_11, shaman17_11, warlock17_11, warrior17_11))

rank17_11wide <- rename(rank17_11wide,  
                          Druid = druid17_11,
                          Hunter = hunter17_11,
                          Mage = mage17_11,
                          Paladin = paladin17_11,
                          Priest = priest17_11,
                          Rogue = rogue17_11,
                          Shaman = shaman17_11,
                          Warlock = warlock17_11,
                          Warrior = warrior17_11)
rank17_11long <- gather(rank17_11wide, Class, Percentage)
rank17_11long$Percentage <- round(rank17_11long$Percentage, 2)
rank17_11long$rankGroup <- 2


#creating rank 10-6 prevalence dataframe

rank10_6 <- filter(WarriorJan2017, contRank < 11 & contRank >= 6)

druid10_6 <- (sum(rank10_6$DruidWin, na.rm = TRUE) + sum(rank10_6$DruidLoss, na.rm = TRUE))/nrow(rank10_6)
hunter10_6 <- (sum(rank10_6$HunterWin, na.rm = TRUE) + sum(rank10_6$HunterLoss, na.rm = TRUE))/nrow(rank10_6)
mage10_6 <- (sum(rank10_6$MageWin, na.rm = TRUE) + sum(rank10_6$MageLoss, na.rm = TRUE))/nrow(rank10_6)
paladin10_6 <- (sum(rank10_6$PaladinWin, na.rm = TRUE) + sum(rank10_6$PaladinLoss, na.rm = TRUE))/nrow(rank10_6)
priest10_6 <- (sum(rank10_6$PriestWin, na.rm = TRUE) + sum(rank10_6$PriestLoss, na.rm = TRUE))/nrow(rank10_6)
rogue10_6 <- (sum(rank10_6$RogueWin, na.rm = TRUE) + sum(rank10_6$RogueLoss, na.rm = TRUE))/nrow(rank10_6)
shaman10_6 <- (sum(rank10_6$ShamanWin, na.rm = TRUE) + sum(rank10_6$ShamanLoss, na.rm = TRUE))/nrow(rank10_6)
warlock10_6 <- (sum(rank10_6$WarlockWin, na.rm = TRUE) + sum(rank10_6$WarlockLoss, na.rm = TRUE))/nrow(rank10_6)
warrior10_6 <- (sum(rank10_6$WarriorWin, na.rm = TRUE) + sum(rank10_6$WarriorLoss, na.rm = TRUE))/nrow(rank10_6)

rank10_6wide <- as.data.frame(cbind(druid10_6, hunter10_6, mage10_6, paladin10_6, priest10_6, rogue10_6, shaman10_6, warlock10_6, warrior10_6))

rank10_6wide <- rename(rank10_6wide,  
                        Druid = druid10_6,
                        Hunter = hunter10_6,
                        Mage = mage10_6,
                        Paladin = paladin10_6,
                        Priest = priest10_6,
                        Rogue = rogue10_6,
                        Shaman = shaman10_6,
                        Warlock = warlock10_6,
                        Warrior = warrior10_6)
rank10_6long <- gather(rank10_6wide, Class, Percentage)
rank10_6long$Percentage <- round(rank10_6long$Percentage, 2)
rank10_6long$rankGroup <- 3


#creating rank 5-2 prevalence dataframe

rank5_2 <- filter(WarriorJan2017, contRank < 6)

druid5_2 <- (sum(rank5_2$DruidWin, na.rm = TRUE) + sum(rank5_2$DruidLoss, na.rm = TRUE))/nrow(rank5_2)
hunter5_2 <- (sum(rank5_2$HunterWin, na.rm = TRUE) + sum(rank5_2$HunterLoss, na.rm = TRUE))/nrow(rank5_2)
mage5_2 <- (sum(rank5_2$MageWin, na.rm = TRUE) + sum(rank5_2$MageLoss, na.rm = TRUE))/nrow(rank5_2)
paladin5_2 <- (sum(rank5_2$PaladinWin, na.rm = TRUE) + sum(rank5_2$PaladinLoss, na.rm = TRUE))/nrow(rank5_2)
priest5_2 <- (sum(rank5_2$PriestWin, na.rm = TRUE) + sum(rank5_2$PriestLoss, na.rm = TRUE))/nrow(rank5_2)
rogue5_2 <- (sum(rank5_2$RogueWin, na.rm = TRUE) + sum(rank5_2$RogueLoss, na.rm = TRUE))/nrow(rank5_2)
shaman5_2 <- (sum(rank5_2$ShamanWin, na.rm = TRUE) + sum(rank5_2$ShamanLoss, na.rm = TRUE))/nrow(rank5_2)
warlock5_2 <- (sum(rank5_2$WarlockWin, na.rm = TRUE) + sum(rank5_2$WarlockLoss, na.rm = TRUE))/nrow(rank5_2)
warrior5_2 <- (sum(rank5_2$WarriorWin, na.rm = TRUE) + sum(rank5_2$WarriorLoss, na.rm = TRUE))/nrow(rank5_2)

rank5_2wide <- as.data.frame(cbind(druid5_2, hunter5_2, mage5_2, paladin5_2, priest5_2, rogue5_2, shaman5_2, warlock5_2, warrior5_2))

rank5_2wide <- rename(rank5_2wide,  
                       Druid = druid5_2,
                       Hunter = hunter5_2,
                       Mage = mage5_2,
                       Paladin = paladin5_2,
                       Priest = priest5_2,
                       Rogue = rogue5_2,
                       Shaman = shaman5_2,
                       Warlock = warlock5_2,
                       Warrior = warrior5_2)
rank5_2long <- gather(rank5_2wide, Class, Percentage)
rank5_2long$Percentage <- round(rank5_2long$Percentage, 2)
rank5_2long$rankGroup <- 4

#joining class prevalence dataframes
classPrevAll <- bind_rows(classMetaPctLong, rank17_11long, rank10_6long, rank5_2long)

#creating version with no "overall"
classPrevRanks <- filter(classPrevAll, rankGroup >= 2)

#making rankGroup factor
classPrevAll$rankGroup <- as.factor(classPrevAll$rankGroup)
classPrevRanks$rankGroup <- as.factor(classPrevRanks$rankGroup)


#Class prevalence by rank plot
ggplot(classPrevRanks, aes(x = Class, y = Percentage, fill = rankGroup)) +
  geom_bar(position = "dodge", stat = "identity", col = "black") +
  coord_flip() +
  ggtitle("Class Prevalence By Rank (January 2017 Season)") +
  scale_x_discrete(
    limits = c("Hunter", "Paladin", "Druid", "Mage", "Warlock",
               "Priest", "Rogue", "Warrior", "Shaman")) +
  scale_fill_brewer(palette = "RdBu", labels = c("Rank 11-17", "Rank 6-10", "Rank 2-5")) +
  scale_y_continuous(breaks = c(.0, .05, .1, .15, .2, .25, .3)) +
  theme(
    legend.justification = c(1,0),
    legend.position = c(.95,.07),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 15),
    plot.title = element_text(face = "bold", hjust = c(0,0))) +
  guides(fill = guide_legend(reverse = TRUE))






