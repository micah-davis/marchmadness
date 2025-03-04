library(cbbdata)
library(tidyverse)
library(ggrepel)

kenpom14 <- read.csv("kenpom14.csv")
kenpom15 <- read.csv("kenpom15.csv")
kenpom16 <- read.csv("kenpom16.csv")
kenpom17 <- read.csv("kenpom17.csv")
kenpom18 <- read.csv("kenpom18.csv")
kenpom19 <- read.csv("kenpom19.csv")
kenpom21 <- read.csv("kenpom21.csv")
kenpom22 <- read.csv("kenpom22.csv")
kenpom23 <- read.csv("kenpom23.csv")
kenpom24 <- read.csv("kenpom24.csv")

kenpom_last_10 <- rbind(kenpom14, kenpom15, kenpom16, kenpom17,
             kenpom18, kenpom19, kenpom21, kenpom22,
             kenpom23, kenpom24)
kenpom_last_10$seed <- as.numeric(kenpom_last_10$seed)

kenpom_last_10 <- kenpom_last_10 |> 
  filter(seed != "NULL")

resume <- cbd_torvik_resume_database(min_year = 2014, max_year = 2024,
                                     n = 1800)

final_four <- resume |> 
  filter(rd %in% c("Champs", "Finals", "F4"))
final_four$year <- as.numeric(final_four$year)

final_four <- left_join(kenpom_last_10, final_four, 
                by = c("TeamName" = "team", "Season" = "year"))
final_four <- final_four |> 
  filter(rd != "NA")

final_rds <- final_four |> 
  group_by(rd) |> 
  summarize(off_rank = mean(RankOE),
            def_rank = mean(RankDE))
champs <- final_four |> 
  filter(rd == "Champs")

ggplot(final_four, aes(x = RankOE, y = RankDE, team = TeamName)) +
  geom_cbb_teams() +
  geom_mean_lines(aes(x0 = AdjOE, y0 = AdjDE), color = "black") +
  xlim(225, 0) +
  ylim(275, 0) +
  labs(title = "Final Four Teams KenPom Adj Efficiencies Ranks",
       subtitle = "Since 2014 NCAA Tournament",
       x = "Adjusted Offense Rank",
       y = "Adjusted Defense Rank",
       caption = "By Micah Davis
       data from cbbdata")
