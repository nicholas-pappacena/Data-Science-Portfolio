library(BasketballAnalyzeR)
RNGkind(sample.kind = "Rounding")
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

remotes::install_github(repo = "abresler/nbastatR",
                        dependencies = TRUE,
                        upgrade = TRUE)
library(nbastatR)

library(tidyverse)
library(knitr)
library(rvest) 
library(janitor)
library(gridExtra)
library(dplyr)

season21 <- c(2021)
tlog_reg_21 <- suppressWarnings(game_logs(seasons = season21, league = "NBA", result_types = "team", season_types = "Regular Season"))

# Team Boxscore Reg Season
tbox_reg_21 <- tlog_reg_21 %>%
  group_by("Season"=yearSeason, "Team"=slugTeam) %>%
  summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
            PTS=sum(ptsTeam),
            W=sum(outcomeGame=="W"), L=sum(outcomeGame=="L"),
            P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
            P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
            FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
            OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
            TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
            PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

# Opponent Boxscore Reg Season
obox_reg_21 <- tlog_reg_21 %>%
  group_by("Season"=yearSeason, "Team"=slugOpponent) %>%
  summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
            PTS=sum(ptsTeam),
            W=sum(outcomeGame=="L"), L=sum(outcomeGame=="W"),
            P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
            P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
            FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
            OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
            TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
            PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

# Opponent Boxscore Reg Season
obox_reg_21 <- tlog_reg_21 %>%
  group_by("Season"=yearSeason, "Team"=slugOpponent) %>%
  summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
            PTS=sum(ptsTeam),
            W=sum(outcomeGame=="L"), L=sum(outcomeGame=="W"),
            P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
            P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
            FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
            OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
            TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
            PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

# Four Factors - 2021 Regular Season by Team 
FF21 <- fourfactors(tbox_reg_21, obox_reg_21)

# Add Wins Column
Wins_21 <- tbox_reg_21$W
FF21 <- cbind(FF21,Wins_21)
View(FF21)
FF21 <- FF21 %>%
  mutate(rank(-ORtg)) %>%
  mutate(rank (DRtg)) %>%
  mutate(rank(-F1.Off)) %>%
  mutate(rank(F1.Def)) %>%
  mutate(rank(F2.Off)) %>%
  mutate(rank(-F2.Def)) %>%
  mutate(rank(-F3.Off)) %>%
  mutate(rank(-F3.Def)) %>%
  mutate(rank(-F4.Off)) %>%
  mutate(rank(F4.Def))
colnames(FF21) <- c("Team", "Off Poss", "Def Poss", "Off Pace", "Def Pace",
                    "ORtg", "DRtg", "eFG", "TOV%", "ORB%", "FTR",
                    "Opp eFG", "Opp TOV%", "Opp DRB%", "Opp FTR", "Wins",
                    "Rank ORtg", "Rank DRtg", "Rank eFG", "Rank TOV%", 
                    "Rank ORB%", "Rank FTR", "Rank Opp eFG", "Rank Opp TOV%",
                    "Rank Opp ORB%", "Rank Opp FTR")

# Subset Four Factor Stats Columns 
FF21_stats <- round(FF21[,c(6,7,8,12,9,13,10,14,11,15)],1)
FF21_stats <- cbind(FF21[,c(1,16)],FF21_stats)
view(FF21_stats)

# Calculate Column Means
Mean_stats_21 <- round(colMeans(FF21_stats[,c(2:12)]),1)
kable(Mean_stats_21, digits = 1)

# Subset Rankings Columns and Add Team Name Column Back
FF21_ranks <- round(FF21[,c(17,18,19,23,20,24,21,25,22,26)])
FF21_ranks <- cbind(FF21[,c(1,16)],FF21_ranks)
View(FF21_ranks)





### 2022 Analysis Four Factors Analysis ###
# Team Logs for 2022 Regular Season
season22 <- c(2022)
tlog_reg_22 <- suppressWarnings(game_logs(seasons = season22, league = "NBA", 
                                          result_types = "team", season_types = "Regular Season"))

# Team Boxscore Reg Season
tbox_reg_22 <- tlog_reg_22 %>%
  group_by("Season"=yearSeason, "Team"=slugTeam) %>%
  summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
            PTS=sum(ptsTeam),
            W=sum(outcomeGame=="W"), L=sum(outcomeGame=="L"),
            P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
            P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
            FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
            OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
            TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
            PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

# Opponent Boxscore Reg Season
obox_reg_22 <- tlog_reg_22 %>%
  group_by("Season"=yearSeason, "Team"=slugOpponent) %>%
  summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
            PTS=sum(ptsTeam),
            W=sum(outcomeGame=="L"), L=sum(outcomeGame=="W"),
            P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
            P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
            FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
            OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
            TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
            PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

# Four Factors - 2022 Regular Season by Team 
FF22 <- fourfactors(tbox_reg_22, obox_reg_22)

# Add Wins Column
Wins_22 <- tbox_reg_22$W
FF22 <- cbind(FF22,Wins_22)

FF22 <- FF22 %>%
  mutate(rank(-ORtg)) %>%
  mutate(rank (DRtg)) %>%
  mutate(rank(-F1.Off)) %>%
  mutate(rank(F1.Def)) %>%
  mutate(rank(F2.Off)) %>%
  mutate(rank(-F2.Def)) %>%
  mutate(rank(-F3.Off)) %>%
  mutate(rank(-F3.Def)) %>%
  mutate(rank(-F4.Off)) %>%
  mutate(rank(F4.Def))
colnames(FF22) <- c("Team", "Off Poss", "Def Poss", "Off Pace", "Def Pace",
                    "ORtg", "DRtg", "eFG", "TOV%", "ORB%", "FTR",
                    "Opp eFG", "Opp TOV%", "Opp DRB%", "Opp FTR", "Wins",
                    "Rank ORtg", "Rank DRtg", "Rank eFG", "Rank TOV%", 
                    "Rank ORB%", "Rank FTR", "Rank Opp eFG", "Rank Opp TOV%",
                    "Rank Opp ORB%", "Rank Opp FTR")
View(FF22)
# Subset Four Factor Stats Columns 
FF22_stats <- round(FF22[,c(6,7,8,12,9,13,10,14,11,15)],1)
FF22_stats <- cbind(FF22[,c(1,16)],FF22_stats)
View(FF22_stats)

# Calculate Column Means
Mean_stats_22 <- round(colMeans(FF22_stats[,c(2:12)]),1)
kable(Mean_stats_22, digits = 1)

# Subset Rankings Columns and Add Team Name Column Back
FF22_ranks <- round(FF22[,c(17,18,19,23,20,24,21,25,22,26)])
FF22_ranks <- cbind(FF22[,c(1,16)],FF22_ranks)

View(FF22_ranks)

### Miami Heat Analysis ###
# Team Ranks
FF21_stats_MIA <- FF21_stats %%
  filter(Team == "MIA")
FF22_stats_MIA <- FF22_stats %%
  filter(Team == "MIA")
FF21_MIA <- FF21_ranks %>%
  filter(Team == "MIA")
FF22_MIA <- FF22_ranks %>%
  filter(Team == "MIA")

View(FF21_stats_MIA)
View(FF22_stats_MIA)
View(FF21_MIA)
View(FF22_MIA)

# Division ranks
FF21_div <- FF21_ranks %>%
  filter(Team == "MIA" | Team == "ATL" | Team == "CHA" | Team ==  "WAS" | Team == "ORL")
FF22_div <- FF22_ranks %>%
  filter(Team == "MIA" | Team == "ATL" | Team == "CHA" | Team ==  "WAS" | Team == "ORL")

View(FF21_div)
View(FF22_div)

### Correlation and Regression, 2021 ### 
# Remove Team, ORtg, Dtg from Stats for Correlation Matrix
FF_corr <- (FF21_stats[,c(-2, -3, -4)])

# Four Factors Correlation Matrix 
kable(round(cor(FF_corr[,unlist(lapply(FF_corr,is.numeric))]),3))

# Regression Analysis for Predicting Wins
eFG_diff <- (FF21_stats$eFG - FF21_stats$`Opp eFG`)/100
TOV_diff <- (FF21_stats$`TOV%` - FF21_stats$`Opp TOV%`)/100
RB_diff <-  (FF21_stats$`ORB%` - FF21_stats$`Opp DRB%`)/100
FTR_diff <- (FF21_stats$FTR - FF21_stats$`Opp FTR`)/100

Regression_21 <- lm(FF21_stats$Wins ~ eFG_diff + TOV_diff + RB_diff + FTR_diff)
summary(Regression_21)

# Correlation between Four Factors and Wins 
cor(FF21_stats$Wins, eFG_diff)
cor(FF21_stats$Wins, TOV_diff)
cor(FF21_stats$Wins, RB_diff)
cor(FF21_stats$Wins, FTR_diff)

# Filter Miami Heat 2021 Stats and FF Differentials
MIA_21_stats <- FF21_stats %>%
  filter(Team == "MIA")

MIA_eFG_diff <- (MIA_21_stats$eFG - MIA_21_stats$`Opp eFG`)/100
MIA_TOV_diff <- (MIA_21_stats$`TOV%` - MIA_21_stats$`Opp TOV%`)/100
MIA_RB_diff <-  (MIA_21_stats$`ORB%` - MIA_21_stats$`Opp DRB%`)/100
MIA_FTR_diff <- (MIA_21_stats$FTR - MIA_21_stats$`Opp FTR`)/100

# Predict Miami Heat Number of Wins and Compare to Actual
MIA_21_WPredict <- round(predict(Regression_21, newdata = list(eFG_diff = MIA_eFG_diff, 
                                                              TOV_diff = MIA_TOV_diff,
                                                              RB_diff  = MIA_RB_diff,
                                                              FTR_diff = MIA_FTR_diff)))
summary(MIA_21_WPredict)

MIA_21_Windiff <- MIA_21_WPredict - MIA_21_stats$Wins
MIA_21_Windiff



# Filter Miami Heat 2022 Stats and FF Differentials
MIA_22_stats <- FF22_stats %>%
  filter(Team == "MIA")

MIA_22_eFG_diff <- (MIA_22_stats$eFG - MIA_22_stats$`Opp eFG`)/100
MIA_22_TOV_diff <- (MIA_22_stats$`TOV%` - MIA_22_stats$`Opp TOV%`)/100
MIA_22_RB_diff <-  (MIA_22_stats$`ORB%` - MIA_22_stats$`Opp DRB%`)/100
MIA_22_FTR_diff <- (MIA_22_stats$FTR - MIA_22_stats$`Opp FTR`)/100

# Predict Miami Heat Number of Wins and Compare to Actual
# Remove Team, ORtg, Dtg from Stats for Correlation Matrix
FF_22_corr <- (FF22_stats[,c(-2, -3, -4)])

# Four Factors Correlation Matrix 
kable(round(cor(FF_22_corr[,unlist(lapply(FF_22_corr,is.numeric))]),3))

# Regression Analysis for Predicting Wins
eFG_22_diff <- (FF22_stats$eFG - FF22_stats$`Opp eFG`)/100
TOV_22_diff <- (FF22_stats$`TOV%` - FF22_stats$`Opp TOV%`)/100
RB_22_diff <-  (FF22_stats$`ORB%` - FF22_stats$`Opp DRB%`)/100
FTR_22_diff <- (FF22_stats$FTR - FF22_stats$`Opp FTR`)/100

Regression_22 <- lm(FF22_stats$Wins ~ eFG_22_diff + TOV_22_diff + RB_22_diff + FTR_22_diff)
summary(Regression_22)

MIA_22_WPredict <- round(predict(Regression_22, newdata = list(eFG_22_diff = MIA_22_eFG_diff, 
                                                               TOV_22_diff = MIA_22_TOV_diff,
                                                               RB_22_diff  = MIA_22_RB_diff,
                                                               FTR_22_diff = MIA_22_FTR_diff)))
summary(MIA_22_WPredict)

MIA_22_Windiff <- MIA_22_WPredict - MIA_22_stats$Wins
MIA_22_Windiff




### Clusters ### 
scrape_stats <- function(season){
  # Player Total Stats 
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_tot <- stats_tot %>% 
    remove_empty(which = "cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  # Player Per 100 Poss Stats 
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_poss.html")
  stats_p100 <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_p100 <- stats_p100 %>% 
    remove_empty(which = "cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename_at(vars(9:29),funs(paste0(.,"_p100"))) %>% 
    select(-rk)
  
  # Player Advanced Stats
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
  stats_adv <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_adv <- stats_adv %>% 
    remove_empty(which = "cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  player_stats <- full_join(player_stats_tot,player_stats_p100,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
    full_join(player_stats_adv,
              by = c("player", "pos", "age", "tm", "g", "mp"))
  return(player_stats)
}

# Scrape last three seasons of Player Stats
player_stats <- map_dfr(2019:2021, scrape_stats)

# Filter and Summarize Stats for Cluster Analysis
player_stats_final <- player_stats %>%
  filter (mp >= 1500) %>%
  group_by (player) %>%
  summarize(
    Team = last(tm),
    Games = sum(g),
    Mins = sum(mp),
    eFG = mean(e_fg_percent),
    P3M_100 = mean(x3p_p100),
    P3p = mean(x3p_percent_p100),
    FTp = mean(ft_percent_p100),
    PTS_100 = mean(pts_p100),
    ORtg = mean(o_rtg),
    DRtg = mean(d_rtg),
    P3Ar = mean(x3p_ar),
    FTR = mean(f_tr),
    ORBp = mean(orb_percent),
    DRBp= mean(drb_percent),
    ASTp = mean(ast_percent),
    STLp = mean(stl_percent),
    BLKp = mean(blk_percent),
    TOVp = mean(tov_percent),
    USGp = mean(usg_percent),
    PER = mean(per),
    BPM = mean(bpm),
    VORP = mean(vorp)
  )

### Basketball Data Science Clustering Approach ### 

# Create stats-only vector
player_stats_clu <- player_stats_final[,c(5:23)]

# Identify Cluster Number 
findk <- hclustering(player_stats_clu, nclumax = 12)
plot(findk)

# Radial Plots by Cluster
radials <-hclustering(player_stats_clu, labels = player_stats_final$player, k=10)
plot(radials, profiles = TRUE)

#Dendrogram
plot(radials, rect = TRUE, colored.branches = TRUE, cex.labels = .25)

# Subset for Plots
clu_subset <- subset(player_stats_final, Mins >= 1500)

Mins <- clu_subset$Mins

Scale_stats <- data.frame(radials$Subjects, scale(player_stats_clu), Mins)

dvar <- c("eFG","P3M_100","P3p","FTp","PTS_100","ORtg","DRtg","P3Ar","FTR",
          "ORBp","DRBp","ASTp","STLp","BLKp","TOVp", "USGp", "PER", "BPM", "VORP")
svar <- "Mins"
yRange <- range(Scale_stats[,dvar])
sizeRange <- c(1500,7500)
no.clu <- 10

p <- vector(no.clu, mode = "list")

for(k in 1:no.clu) {
  Clusters <- subset(Scale_stats, Cluster == k)
  vrb <- variability(Clusters[,3:22], data.var = dvar,
                     size.var = svar, weight = FALSE, VC = FALSE)
  title <- paste ("Cluster", k)
  p[[k]] <- plot(vrb, size.lim = sizeRange, ylim = yRange, title = title,
                 leg.pos = c(0,1), leg.just = c(-0.5,0),
                 leg.box = "vertical", leg.brk = seq(1500,7500,1500),
                 leg.title.pos = "left", leg.nrow = 1, max.circle = 7)
}

grid.arrange(grobs = p, ncol = 2)

C1 <- Scale_stats %>%
  filter(Cluster == 1)
C1_Players <- C1$Label

C2 <- Scale_stats %>%
  filter(Cluster == 2)
C2_Players <- C2$Label

C3 <- Scale_stats %>%
  filter(Cluster == 3)
C3_Players <- C3$Label

C4 <- Scale_stats %>%
  filter(Cluster == 4)
C4_Players <- C4$Label

C5 <- Scale_stats %>%
  filter(Cluster == 5)
C5_Players <- C5$Label

C6 <- Scale_stats %>%
  filter(Cluster == 6)
C6_Players <- C6$Label

C7 <- Scale_stats %>%
  filter(Cluster == 7)
C7_Players <- C7$Label

C8 <- Scale_stats %>%
  filter(Cluster == 8)
C8_Players <- C8$Label

C9 <- Scale_stats %>%
  filter(Cluster == 9)
C9_Players <- C9$Label

C10 <- Scale_stats %>%
  filter(Cluster == 10)
C10_Players <- C10$Label

kable(C1_Players, col.names = "Cluster 1")
kable(C2_Players, col.names = "Cluster 2")
kable(C3_Players, col.names = "Cluster 3")
kable(C4_Players, col.names = "Cluster 4")
kable(C5_Players, col.names = "Cluster 5")
kable(C6_Players, col.names = "Cluster 6")
kable(C7_Players, col.names = "Cluster 7")
kable(C8_Players, col.names = "Cluster 8")
kable(C9_Players, col.names = "Cluster 9")
kable(C10_Players, col.names = "Cluster 10")

Player_Cluster <-Scale_stats[,2] 
player_stats_all <- cbind(player_stats_final, Player_Cluster)

# Boxplot Comparisons by Cluster
par(mfrow = c(1,3))
boxplot(Mins ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Minutes Played by Cluster", xlab = "Cluster", 
        ylab = "Minutes Played")
boxplot(eFG ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Effective FG% by Cluster", xlab = "Cluster", 
        ylab = "eFG%")
boxplot(PTS_100 ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Points Per 100 Poss. by Cluster", xlab = "Cluster", 
        ylab = "Points Per 100 Poss.")

boxplot(P3Ar ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "3-Pt Att. Rate by Cluster", xlab = "Cluster", 
        ylab = "3-Pt Att. Rate")
boxplot(P3M_100 ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "3s Made Per 100 Poss. by Cluster", xlab = "Cluster", 
        ylab = "3s Made Per 100 Poss.")
boxplot(P3p ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "3-Pt % by Cluster", xlab = "Cluster", 
        ylab = "3-Pt %")

par(mfrow = c(1,2))
boxplot(FTR ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Free Throw Rate by Cluster", xlab = "Cluster", 
        ylab = "Free Throw Rate")
boxplot(FTp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Free Throw % by Cluster", xlab = "Cluster", 
        ylab = "Free Throw %")

par(mfrow = c(1,3))
boxplot(ORtg ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Off. Rating by Cluster", xlab = "Cluster", 
        ylab = "Off. Rating")
boxplot(DRtg ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Def. Rating by Cluster", xlab = "Cluster", 
        ylab = "Def. Rating")
boxplot(USGp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Usage % by Cluster", xlab = "Cluster", 
        ylab = "Usage %")

boxplot(ORBp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Off. Rebound % by Cluster", xlab = "Cluster", 
        ylab = "Off. Rebound %")
boxplot(DRBp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Def. Rebound % by Cluster", xlab = "Cluster", 
        ylab = "Def. Rebound %")
boxplot(BLKp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Block Rate by Cluster", xlab = "Cluster", 
        ylab = "Block Rate")

boxplot(ASTp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Assist Rate by Cluster", xlab = "Cluster", 
        ylab = "Assist Rate")
boxplot(STLp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Steal Rate by Cluster", xlab = "Cluster", 
        ylab = "Steal Rate")
boxplot(TOVp ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "Turnover Rate by Cluster", xlab = "Cluster", 
        ylab = "Turnover Rate")

boxplot(PER ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "PER by Cluster", xlab = "Cluster", 
        ylab = "PER")
boxplot(BPM ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "BPM by Cluster", xlab = "Cluster", 
        ylab = "BPM")
boxplot(VORP ~ Player_Cluster, data = player_stats_all, col = rainbow(12),
        main = "VORP by Cluster", xlab = "Cluster", 
        ylab = "VORP")

par(mfrow = c(1,1))





