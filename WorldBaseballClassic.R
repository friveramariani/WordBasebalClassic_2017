#---Questions----------------------------------------------
# 1) Which teams are leaders in each offensive category?
# 2) How teams group in offensive category per 9 innings?
# 3) Which teams are leaders in each pitching category?
# 4) How teams group in pitching stats per 9 inning?


#---set wd and load dataset--------------------------------
#---set wd and load dataset--------------------------------
library(tidyverse)
library(knitr)
library(gridExtra)

wbc2017 <- read_tsv(paste0("C:/Users/Felix/Dropbox/DataScience/Projects/WordBasebalClassic_2017/data/", "WorldBaseballClassic.txt"), col_types="icccdddddddddddddd")

names(wbc2017)[9:10] <- c("X2B", "X3B")

pitching <- "https://github.com/friveramariani/WordBasebalClassic_2017/blob/master/data/WorldBaseballClassicPitching.txt"

wbc2017_pit <- read_tsv(paste0("C:/Users/Felix/Dropbox/DataScience/Projects/WordBasebalClassic_2017/data/", "WorldBaseballClassicPitching.txt"), col_types = "iccdddddddddddddd")

#---Question 1 and 2---------------------------------------------
wbc_wins_by_team <- wbc2017_pit %>% group_by(TEAM) %>% summarise(Wins = sum(W))

wbc_hit_by_team <- wbc2017 %>% 
    select(TEAM, R, H, X2B, X3B, HR, SO, SB, AVG, OBP, OPS) %>% 
    group_by(TEAM) %>%
    summarise(R = sum(R, na.rm=TRUE),
              H = sum(H, na.rm=TRUE),
              X2B = sum(X2B, na.rm=TRUE),
              X3B = sum(X3B, na.rm=TRUE),
              HR = sum(HR, na.rm=TRUE),
              SO = sum(SO, na.rm=TRUE),
              SB = sum(SB, na.rm=TRUE),
              AVG = mean(AVG, na.rm=TRUE),
              OBP = mean(OBP, na.rm=TRUE),
              OPS = mean(OPS, na.rm=TRUE))

wbc_hit_by_team_joined  <- right_join(wbc_hit_by_team, wbc_wins_by_team, by = 'TEAM')

opar <- par(no.readonly = TRUE)
options(digits = 3)

kable(wbc_hit_by_team_joined %>% select(TEAM, Wins, R, H, X2B, X3B, HR, SO, SB,
                                        AVG, OBP, OPS) %>% arrange(desc(Wins)), 
      caption = "Table 1: Offensive Summary per Team")

par(opar)

wbc_hit_by_team_p9 <- wbc_hit_by_team %>% 
    select(TEAM, R, H, X2B, X3B, HR, SO, SB) %>% 
    group_by(TEAM) %>%
    summarise(R_p9 = R/9,
              H_p9 = H/9, 
              X2B_p9 = X2B/9, 
              X3B_p9 = X3B/9,
              HR_p9 = HR/9,
              SO_p9 = SO/9,
              SB_p9 = SB/9)

wbc_hit_by_team_p9_joined  <- right_join(wbc_wins_by_team, wbc_hit_by_team_p9, by = 'TEAM')

opar <- par(no.readonly = TRUE)
options(digits = 3)
kable(wbc_hit_by_team_p9_joined %>% select(TEAM, Wins, R_p9, H_p9, X2B_p9, X3B_p9, HR_p9, SO_p9, SB_p9) %>% 
          arrange(desc(Wins)), caption = "Table 2: Offensive Summary per Team per 9 Innings")
par(opar)

runs_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -R_p9), y=R_p9)) + geom_bar(stat="identity") +
    xlab("") + ggtitle ("Runs per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

hits_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -H_p9), y=H_p9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("Hits per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

X2B_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -X2B_p9), y=X2B_p9)) + geom_bar(stat="identity") +
    xlab("") + ggtitle("Doubles per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) 

X3B_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -X3B_p9), y=X3B_p9)) + geom_bar(stat="identity") +
    xlab("") + ggtitle("Triples per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) 

HR_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -HR_p9), y=HR_p9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("HR per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

SO_plot <- ggplot(wbc_hit_by_team_p9_joined, aes(x=reorder(TEAM, -SO_p9), y=SO_p9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("SO per 9 innings") + ylab("") +
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) 


grid.arrange(runs_plot, hits_plot, X2B_plot, X3B_plot, HR_plot, SO_plot, nrow=2, ncol=3)

#---Question 3 and 4---------------------------------------------
wbc_pit_per_team_inn9 <- wbc2017_pit %>% group_by(TEAM) %>% 
    summarise(W = sum(W, na.rm=TRUE),
              L = sum(L, na.rm=TRUE),
              ERA = mean(ERA, na.rm=TRUE),
              Rp9 = sum(R, na.rm=TRUE)/9,
              ERp9 = sum(ER, na.rm=TRUE)/9,
              ER_R_pcnt = ((Rp9 - ERp9)/Rp9)*100,
              SV = sum(SV, na.rm=TRUE),
              Hp9 = sum(H, na.rm=TRUE)/9,
              HRp9 = sum(HR, na.rm=TRUE)/9,
              BBp9 = sum(BB, na.rm=TRUE)/9,
              SOp9 = sum(SO, na.rm=TRUE)/9,
              WHIP = mean(WHIP, na.rm=TRUE))

options(digits = 3)
kable(wbc_pit_per_team_inn9 %>% select(TEAM, W, L, ERA, Rp9, ERp9, ER_R_pcnt, Hp9, HRp9, BBp9, SOp9, WHIP) %>% 
          arrange(desc(W)), caption = "Table 3: Pitching Summary per Team per 9 Innings")
par(opar)

era_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -ERA), y=ERA)) + geom_bar(stat="identity") +
    xlab("") + ggtitle ("ERA") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

runs_all_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -Rp9), y=Rp9)) + geom_bar(stat="identity") +
    xlab("") + ggtitle ("Runs per allowed 9 inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

era_pcnt_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -ER_R_pcnt), y=ER_R_pcnt)) + 
    geom_bar(stat="identity") + 
    xlab("") + ggtitle("Pcnt of earned runs") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

hits_all_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -Hp9), y=Hp9)) + geom_bar(stat="identity") +
    xlab("") + ggtitle("Hits allowed per 9 inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

HR_all_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -HRp9), y=HRp9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("HR allowed per 9 inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

bb_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -BBp9), y=BBp9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("BB allowed per 9 inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

SO_opp_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -SOp9), y=SOp9)) + geom_bar(stat="identity") + 
    xlab("") + ggtitle("SO per 9 inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

whip_plot <- ggplot(wbc_pit_per_team_inn9, aes(x=reorder(TEAM, -WHIP), y=WHIP)) + geom_bar(stat="identity") +
    xlab("") + ggtitle ("Walks + hits per inn.") + ylab("") + 
    theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))

library(gridExtra)
grid.arrange(era_plot, runs_all_plot, era_pcnt_plot, hits_all_plot, HR_all_plot, bb_plot, SO_opp_plot, 
             whip_plot, nrow=3, ncol=3)

