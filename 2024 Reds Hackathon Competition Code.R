library(tidyverse)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cowplot)
library(gghighlight)

library(readr)
Savant <- read_csv("Desktop/Sports Analytics/Reds Hackathon/savant_pitch_level.csv")

Fangraphs <- read_csv("Desktop/Sports Analytics/Reds Hackathon/fangraphs_season_level (1).csv")

Fangraphs_Year_Data <- Fangraphs %>% 
  mutate(Stuff_plus = replace(Stuff_plus, is.na(Pitching_plus), 0),
         Stf_plus_FA = replace(Stf_plus_FA, is.na(Stf_plus_FA), 0),
         Stf_plus_CH = replace(Stf_plus_CH, is.na(Stf_plus_CH), 0),
         Stf_plus_SL = replace(Stf_plus_SL, is.na(Stf_plus_SL), 0),
         Stf_plus_CU = replace(Stf_plus_CU, is.na(Stf_plus_CU), 0),
         Stf_plus_SI = replace(Stf_plus_SI, is.na(Stf_plus_SI), 0),
         Stf_plus_FS = replace(Stf_plus_FS, is.na(Stf_plus_FS), 0),
         Stf_plus_FC = replace(Stf_plus_FC, is.na(Stf_plus_FC), 0)) %>% 
  group_by(MLBAMID, Name, Season, Role) %>% 
  summarise(Innings_Pitched = sum(IP, na.rm = TRUE),
            ERA = sum(ERA * IP, na.rm = TRUE) / sum(IP, na.rm = TRUE),
            K_pct = sum(K_pct * TBF, na.rm = TRUE) / sum(TBF, na.rm = TRUE),
            BB_pct = sum(BB_pct * TBF, na.rm = TRUE) / sum(TBF, na.rm = TRUE),
            GB_pct = sum(GB_pct * Events, na.rm = TRUE) / sum(Events, na.rm = TRUE),
            CSW_pct = sum(CSW_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            FStrike_pct = sum(FStrike_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            Zone_pct = sum(Zone_pct * Pitches, na.rm = TRUE) / sum(Pitches, na.rm = TRUE),
            LOB_pct = mean(LOB_pct, na.rm = TRUE),
            BABIP = sum(BABIP * Events, na.rm = TRUE) / sum(Events, na.rm = TRUE),
            WHIP = mean(WHIP, na.rm = TRUE),
            FIP = mean(FIP, na.rm = TRUE),
            SIERA = mean(SIERA, na.rm = TRUE),
            Pace = mean(Pace, na.rm = TRUE ),
            WPA = mean(WPA, na.rm = TRUE),
            Stuff_plus = mean(Stuff_plus, na.rm = TRUE),
            FB_plus = mean(Stf_plus_FA, na.rm = TRUE),
            CH_plus = mean(Stf_plus_CH, na.rm = TRUE),
            SL_plus = mean(Stf_plus_SL, na.rm = TRUE),
            CB_plus = mean(Stf_plus_CU, na.rm = TRUE),
            SI_plus = mean(Stf_plus_SI, na.rm = TRUE),
            SPLIT_plus = mean(Stf_plus_FS, na.rm = TRUE),
            CUT_plus = mean(Stf_plus_FC, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(Innings_Pitched >= 20) %>%
  mutate(FB_Score = ifelse(FB_plus >= 98, 1, 0),
         CH_Score = ifelse(CH_plus >= 86, 1, 0),
         SL_Score = ifelse(SL_plus >= 111, 1, 0),
         CB_Score = ifelse(CB_plus >= 105, 1, 0),
         SI_Score = ifelse(SI_plus >= 92, 1, 0),
         SPLIT_Score = ifelse(SPLIT_plus >= 105, 1, 0),
         CUT_Score = ifelse(CUT_plus >= 107, 1, 0)) %>% 
  rowwise() %>% 
  mutate(Pitch_Arsenal = sum(FB_Score, CH_Score, SL_Score, CB_Score, SI_Score, SPLIT_Score, CUT_Score))


Fangraphs_Data <- Fangraphs_Year_Data %>% 
  group_by(MLBAMID, Name, Role) %>% 
  summarise(Innings_Pitched = mean(Innings_Pitched, na.rm = TRUE),
            ERA = mean(ERA , na.rm = TRUE),
            K_pct = mean(K_pct, na.rm = TRUE),
            BB_pct = mean(BB_pct, na.rm = TRUE),
            GB_pct = mean(GB_pct, na.rm = TRUE),
            CSW_pct = mean(CSW_pct, na.rm = TRUE),
            Zone_pct = mean(Zone_pct, na.rm = TRUE),
            FStrike_pct = mean(FStrike_pct, na.rm=TRUE),
            Pace = mean(Pace, na.rm = TRUE),
            WHIP = mean(WHIP, na.rm = TRUE),
            LOB_pct = mean(LOB_pct, na.rm = TRUE),
            BABIP = mean(BABIP, na.rm = TRUE),
            FIP = mean(FIP, na.rm= TRUE),
            SIERA = mean(SIERA, na.rm = TRUE),
            WPA = mean(WPA, na.rm = TRUE),
            Stuff_plus = mean(Stuff_plus, na.rm = TRUE),
            FB_plus = mean(FB_plus, na.rm = TRUE),
            CH_plus = mean(CH_plus, na.rm = TRUE),
            SL_plus = mean(SL_plus, na.rm = TRUE),
            CB_plus = mean(CB_plus, na.rm = TRUE),
            SI_plus = mean(SI_plus, na.rm = TRUE),
            SPLIT_plus = mean(SPLIT_plus, na.rm = TRUE),
            CUT_plus = mean(CUT_plus, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(K_pct = K_pct * 100,
         BB_pct = BB_pct * 100,
         GB_pct = GB_pct * 100,
         CSW_pct = CSW_pct * 100,
         BABIP = BABIP * 100,
         FB_Score = ifelse(FB_plus >= 97, 1, 0),
         CH_Score = ifelse(CH_plus >= 98, 1, 0),
         SL_Score = ifelse(SL_plus >= 106, 1, 0),
         CB_Score = ifelse(CB_plus >= 103, 1, 0),
         SI_Score = ifelse(SI_plus >= 95, 1, 0),
         SPLIT_Score = ifelse(SPLIT_plus >= 105, 1, 0),
         CUT_Score = ifelse(CUT_plus >= 98, 1, 0)) %>% 
  rowwise() %>% 
  mutate(Pitch_Arsenal = sum(FB_Score, CH_Score, SL_Score, CB_Score, SI_Score, SPLIT_Score, CUT_Score)) %>% 
  dplyr::select(-FB_plus, -CH_plus, -SL_plus, -CB_plus, -SI_plus, -SPLIT_plus, -CUT_plus, -FB_Score, -CH_Score, -SL_Score, -CB_Score, -SI_Score, -SPLIT_Score, -CUT_Score, -Stuff_plus)


Savant_Data <- Savant %>% 
  dplyr::select(player_name, pitcher, release_speed) %>% 
  group_by(pitcher, player_name) %>% 
  summarise(Velocity = mean(release_speed, na.rm = TRUE)) %>% 
  ungroup()

MLB <- right_join(Savant_Data, Fangraphs_Data, by = c("pitcher" = "MLBAMID")) %>% 
  dplyr::select(-Name) %>% 
  mutate(player_name = replace(player_name, pitcher == "660636", "Diego Castillo 2"),
         player_name = replace(player_name, pitcher == "671106", "Logan Allen 2"),
         player_name = replace(player_name, pitcher == "622491", "Luis Castillo 2"),
         player_name = replace(player_name, pitcher == "642770", "Javy Guerra 2"))

MLB_Name <- MLB$player_name
MLB_ID <- MLB$pitcher

MLB_pca <- MLB %>% 
  dplyr::select(-pitcher, -player_name, -Role)

MLB_tree <- MLB %>% 
  dplyr::select(-pitcher, -player_name)

rownames(MLB_pca) <- make.unique(MLB_Name)
rownames(MLB_tree) <- make.unique(MLB_Name)

MLB_pca <- prcomp(MLB_pca, center = TRUE, scale = TRUE)

get_eigenvalue(MLB_pca)

fviz_eig(MLB_pca, addlabels = TRUE) +
  xlab("Principal Component") +
  ylab("% of Variance Explained") +
  labs(title = "**PCA Analysis: Scree Plot**")

pc1 <- fviz_contrib(MLB_pca, choice = "var", axes = 1)
pc2 <- fviz_contrib(MLB_pca, choice = "var", axes = 2)
pc3 <- fviz_contrib(MLB_pca, choice = "var", axes = 3)

plot_grid(pc1, pc2, pc3)

k <- 3

pca_scores <- MLB_pca$x

set.seed(1928)
MLB_kmeans <- kmeans(pca_scores, centers = k)

MLB_kmeans$cluster

cluster_assignment <- MLB_kmeans$cluster

MLB$cluster <- cluster_assignment

kmean_dataviz <- MLB %>%
  rename(c("Velocity" = Velocity,
           #"Innings Pitched" = Innings_Pitched,
           "Earned Run Average" = ERA,
           "Strikeout Percentage" = K_pct,
           "Walk Percentage" = BB_pct,
           "Pitching Plus" = Pitching_plus))

kmean_dataviz <- kmean_dataviz %>%
  mutate(cluster = case_when(
    cluster == 1 ~ "Cluster 1",
    cluster == 2 ~ "Cluster 2",
    cluster == 3 ~ "Cluster 3"))

kmean_data_long <- kmean_dataviz %>%
  gather("Variable", "Value", -player_name, -pitcher, -cluster)

ggplot(kmean_data_long, aes(x = Variable, y = Value, color = cluster)) +
  geom_point(size = 3) +
  facet_wrap(~ cluster) +
  scale_color_brewer(palette = "Set1") +
  gghighlight(use_direct_label = FALSE) +
  theme(axis.text = element_text(angle = 90, size = 8),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

Cluster_1 <- MLB %>% 
  filter(cluster == 1)

Cluster_2 <- MLB %>% 
  filter(cluster == 2)

Cluster_3 <- MLB %>% 
  filter(cluster == 3)


########### DECISION TREES #####################
library(rpart)

attach(MLB_tree)

#variable_weights <- c(7, 2, 18, 500, 0, 1, 4)
#variable_weights <- data.frame(variable_weights)

# Create a weights vector based on variable_weights
#weights_vector <- rep(1, nrow(MLB_tree))
#weights_vector[MLB_tree$ERA] <- variable_weights[1]
#weights_vector[variable_weights$K_pct] <- variable_weights[2]
#weights_vector[MLB_tree$BB_pct] <- variable_weights[3]
#weights_vector[MLB_tree$Pitch_Arsenal] <- variable_weights[4]
#weights_vector[MLB_tree$Innings_Pitched] <- variable_weights[5]
#weights_vector[MLB_tree$Pitching_plus] <- variable_weights[6]
#weights_vector[MLB_tree$Velocity] <- variable_weights[7]


tree.MLB <- rpart(Role ~ Innings_Pitched+ERA+Velocity+K_pct+BB_pct+Pitch_Arsenal, 
                  data=MLB_tree, 
                  method="class")


plot(tree.MLB, uniform=TRUE, main="Decision Tree for MLB Data", margin=0.1); text(tree.MLB, cex=0.8)

group<-predict(tree.MLB, type="class")
tab.tree <- table(group,Role)

1-( sum(diag(tab.tree)) / sum(tab.tree) )

Predicted_Role <- data.frame(group)

d <- Predicted_Role
names <- rownames(d)
rownames(d) <- NULL
Predicted_Role_Data <- cbind(names,d)


MLB_Group <- left_join(MLB, Predicted_Role_Data, by = c("player_name" = "names"))



