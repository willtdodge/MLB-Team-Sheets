## Update Depth Charts with Offseason moves

library(dplyr)
df <- df %>%
  mutate(Team = ifelse(Name == "Kyle Hendricks", "LAA", Team)) %>%
  mutate(Team = ifelse(Name == "T.J. McFarland", "OAK", Team)) %>%
  mutate(Pos = ifelse(Name == "Darell Hernaiz", "3B", Pos)) %>%
  mutate(Pos = ifelse(Name == "Miguel Amaya", "C", Pos)) %>%
  mutate(Pos = ifelse(Name == "Willson Contreras", "1B", Pos)) %>%
  mutate(Pos = ifelse(Name == "Iván Herrera", "C", Pos)) %>%
  mutate(Pos = ifelse(Name == "Alec Burleson", "DH", Pos)) %>%
  mutate(Pos = ifelse(Name == "Nolan Gorman", "Bench", Pos)) %>%
  mutate(`2025wOBA` = ifelse(Name == "Luke Ritter", 0.279, `2025wOBA`))
