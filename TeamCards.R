library(ggplot2)
library(GeomMLBStadiums)
library(dplyr)
library(tidyverse)
library(magick)
library(grid)
library(scales)
library(ggtext)
library(randomForest)
library(caret)
library(car)

# Read the CSV data
df1 <- read.csv("Season - Team Score.csv")

# Team vectors as defined with statistical projected values
TOR<-c(.723,	4.08,	4.14,	12.6,
       .324,	.408,	.321,	76.1,29.8)
### INSERT REMAINING TEAMS AS SUCH

# Subset the data frame to get X_df
X_df <- df1[, 2:(ncol(df1) - 1)]
X_df <- subset(X_df, select = -c(BB.9, K.9))

# Extract y_df
y_df <- df1$W

# Set the seed for reproducibility
set.seed(4)

# Split the data into training and testing sets
train_index <- createDataPartition(y_df, p = 0.8, list = FALSE)
X_train <- X_df[train_index, ]
X_test <- X_df[-train_index, ]
y_train <- y_df[train_index]
y_test <- y_df[-train_index]

# Initialize, train, and predict
model <- randomForest(X_train, y_train, ntree = 100, mtry = 4, 
                      nodesize = 1, random.seed = 5)
df1$Wins_pred <- predict(model, X_df)

# Apply stats
r2 <- cor(df1$W, df1$Wins_pred)^2
rmse <- sqrt(mean((df1$W - df1$Wins_pred)^2))

# Visualize
title_text <- sprintf("RMSE: %.3f", rmse)
ggplot(df1, aes(x = W, y = Wins_pred)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = title_text)

# Define the teams
teams <- c('BAL', 'TOR', 'TBR', 'BOS', 'NYY', 'CLE', 'KCR', 'DET', 'MIN', 'CHW',
           'LAA', 'HOU', 'OAK', 'SEA', 'TEX', 'ATL', 'NYM', 'WSH', 'MIA', 'PHI', 
           'MIL', 'STL', 'CHC', 'PIT', 'CIN', 'ARI', 'SDP', 'SFG', 'COL', 'LAD')

# Predict wins for each team
team_wins <- c(round(predict(model, BAL), 1),
               round(predict(model, TOR), 1),
               round(predict(model, TBR), 1),
               round(predict(model, BOS), 1),
               round(predict(model, NYY), 1),
               round(predict(model, CLE), 1),
               round(predict(model, KCR), 1),
               round(predict(model, DET), 1),
               round(predict(model, MIN), 1),
               round(predict(model, CHW), 1),
               round(predict(model, LAA), 1),
               round(predict(model, HOU), 1),
               round(predict(model, OAK), 1),
               round(predict(model, SEA), 1),
               round(predict(model, TEX), 1),
               round(predict(model, ATL), 1),
               round(predict(model, NYM), 1),
               round(predict(model, WSH), 1),
               round(predict(model, MIA), 1),
               round(predict(model, PHI), 1),
               round(predict(model, MIL), 1),
               round(predict(model, STL), 1),
               round(predict(model, CHC), 1),
               round(predict(model, PIT), 1),
               round(predict(model, CIN), 1),
               round(predict(model, ARI), 1),
               round(predict(model, SDP), 1),
               round(predict(model, SFG), 1),
               round(predict(model, COL), 1),
               round(predict(model, LAD), 1))

# Create the initial `preds2025` data frame without rankings
# Create the initial `preds2025` data frame with explicit types
preds2025 <- data.frame(
  Team = teams,
  Wins = team_wins,
  Losses = 162 - team_wins,
  Div = as.character(c(rep("AL East", 5), rep("AL Central", 5), rep("AL West", 5),
                       rep("NL East", 5), rep("NL Central", 5), rep("NL West", 5))),
  Salary24 = NA_real_,  # initialize as numeric
  Salary25 = NA_real_   # initialize as numeric
)

# Calculate division ranks by splitting, ranking, and recombining
ranked_divisions <- preds2025 %>%
  group_split(Div) %>%
  lapply(function(df) {
    df <- df %>%
      arrange(desc(Wins)) %>%
      mutate(Division_Rank = seq_len(nrow(df)))  # Rank within each division
  }) %>%
  bind_rows()  # Recombine all divisions

# Assign `ranked_divisions` back to `preds2025` for use in other parts of code
preds2025 <- ranked_divisions

# Manual overrides for Salary23 and Salary24 for specific rows
# Create a vector with team names in the desired order
team_names <- c("MIN", "CLE", "KCR", "DET", "CHW", 
                "NYY", "BAL", "TOR", "TBR", "BOS", 
                "HOU", "TEX", "SEA", "OAK", "LAA", 
                "CHC", "STL", "MIL", "CIN", "PIT", 
                "ATL", "PHI", "NYM", "MIA", "WSH", 
                "LAD", "SDP", "ARI", "SFG", "COL")

# Assign Salary24 values based on team names
preds2025$Salary24 <- setNames(c(129800000, 106200000, 124700000, 91600000, 138800000, 
                                 309800000, 104400000, 216200000, 87600000, 183800000,
                                 242400000, 240600000, 176300000, 146700000, 64000000, 
                                 225500000, 179900000, 125400000, 94200000, 86400000, 
                                 237000000, 341800000, 248800000, 92400000, 128600000,
                                 288800000, 172700000, 177100000, 206600000, 148800000), 
                               team_names)

# Assign Salary25 values based on team names
preds2025$Salary25 <- setNames(c(137100000, 81500000, 108300000, 76200000, 63500000, 
                                 248700000, 106700000, 199100000, 80500000, 148400000,
                                 211900000, 210000000, 149300000, 189900000, 57300000,
                                 185200000, 140600000, 108300000, 92100000, 74500000,
                                 208600000, 242200000, 270200000, 65800000, 77100000, 
                                 288800000, 204100000, 138800000, 180100000, 126200000), 
                               team_names)

# Explicitly assign divisions
preds2025$Div <- c(rep("AL Central", 5), rep("AL East", 5), rep("AL West", 5),
                   rep("NL Central", 5), rep("NL East", 5), rep("NL West", 5))


# Load player data from a CSV (ensure the file is in the working directory)
df <- read_csv("2025Players.csv")

# List of MLB teams with full names and corresponding ESPN logo URLs
mlb_team_logos <- data.frame(
  team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
           "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
           "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WSH"),
  full_name = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", 
                "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians", 
                "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers", 
                "Minnesota Twins", "New York Mets", "New York Yankees", "Athletics", 
                "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants", 
                "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", 
                "Toronto Blue Jays", "Washington Nationals"),
  logo_url = c(
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/ari.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/atl.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/bal.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/bos.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/chc.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/chw.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/cin.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/cle.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/col.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/det.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/hou.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/kc.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/laa.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/lad.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/mia.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/mil.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/min.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/nym.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/nyy.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/oak.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/phi.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/pit.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sd.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sf.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sea.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/stl.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tb.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tex.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tor.png&h=500&w=500",
    "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/wsh.png&h=500&w=500"
  )
)

# Define teams and divisions
teams <- c('BAL', 'TOR', 'TBR', 'BOS', 'NYY', 'CLE', 'KCR', 'DET', 'MIN', 'CHW',
           'LAA', 'HOU', 'OAK', 'SEA', 'TEX', 'ATL', 'NYM', 'WSH', 'MIA', 'PHI',
           'MIL', 'STL', 'CHC', 'PIT', 'CIN', 'ARI', 'SDP', 'SFG', 'COL', 'LAD')
divisions <- data.frame(
  Team = teams,
  League = c(rep("AL", 15), rep("NL", 15)),
  Division = c("AL_East", "AL_East", "AL_East", "AL_East", "AL_East", 
               "AL_Central", "AL_Central", "AL_Central", "AL_Central", "AL_Central", 
               "AL_West", "AL_West", "AL_West", "AL_West", "AL_West",
               "NL_East", "NL_East", "NL_East", "NL_East", "NL_East", 
               "NL_Central", "NL_Central", "NL_Central", "NL_Central", "NL_Central",
               "NL_West", "NL_West", "NL_West", "NL_West", "NL_West")
)

# Define team talent calculations (feature importances already given)
feature_importances <- c(LOB. = 0.118, ERA = 0.122, FIP = 0.032, pWAR = 0.167,
                         OBP = 0.047, SLG = 0.029, wOBA = 0.043, Off = 0.077,
                         WAR = 0.364)

# Team stats data (assuming data is preloaded per team as in the initial structure)
team_stats <- do.call(rbind, lapply(teams, function(team) {
  data.frame(Team = team, t(get(team)))
}))

colnames(team_stats) <- c("Team", "LOB.", "ERA", "FIP", "pWAR", "OBP", "SLG",
                          "wOBA", "Off", "WAR")
team_stats_standardized <- as.data.frame(scale(team_stats[,
                                                          names(feature_importances)]) * 2)
team_stats_standardized <- team_stats_standardized %>%
  mutate(
    ERA = -1 * ERA,   # Multiply ERA by -1
    FIP = -1 * FIP    # Multiply FIP by -1
  )
team_stats$Talent <- rowSums(team_stats_standardized * feature_importances)
team_talent <- team_stats[, c("Team", "Talent")]
colnames(team_stats) <- c("Team", "LOB.", "ERA", "FIP", "pWAR", "OBP", "SLG", "wOBA", "Off", "WAR")

team_name_map <- c(
  "Arizona Diamondbacks" = "ARI", "D-backs" = "ARI",
  "Atlanta Braves" = "ATL", "Braves" = "ATL",
  "Baltimore Orioles" = "BAL", "Orioles" = "BAL",
  "Boston Red Sox" = "BOS", "Red Sox" = "BOS",
  "Chicago Cubs" = "CHC", "Cubs" = "CHC",
  "Chicago White Sox" = "CHW", "White Sox" = "CHW",
  "Cincinnati Reds" = "CIN", "Reds" = "CIN",
  "Cleveland Guardians" = "CLE", "Guardians" = "CLE",
  "Colorado Rockies" = "COL", "Rockies" = "COL",
  "Detroit Tigers" = "DET", "Tigers" = "DET",
  "Houston Astros" = "HOU", "Astros" = "HOU",
  "Kansas City Royals" = "KCR", "Royals" = "KCR",
  "Los Angeles Angels" = "LAA", "Angels" = "LAA",
  "Los Angeles Dodgers" = "LAD", "Dodgers" = "LAD",
  "Miami Marlins" = "MIA", "Marlins" = "MIA",
  "Milwaukee Brewers" = "MIL", "Brewers" = "MIL",
  "Minnesota Twins" = "MIN", "Twins" = "MIN",
  "New York Mets" = "NYM", "Mets" = "NYM",
  "New York Yankees" = "NYY", "Yankees" = "NYY",
  "Oakland Athletics" = "OAK", "Athletics" = "OAK",
  "Philadelphia Phillies" = "PHI", "Phillies" = "PHI",
  "Pittsburgh Pirates" = "PIT", "Pirates" = "PIT",
  "San Diego Padres" = "SDP", "Padres" = "SDP",
  "San Francisco Giants" = "SFG", "Giants" = "SFG",
  "Seattle Mariners" = "SEA", "Mariners" = "SEA",
  "St. Louis Cardinals" = "STL", "Cardinals" = "STL",
  "Tampa Bay Rays" = "TBR", "Rays" = "TBR",
  "Texas Rangers" = "TEX", "Rangers" = "TEX",
  "Toronto Blue Jays" = "TOR", "Blue Jays" = "TOR",
  "Washington Nationals" = "WSH", "Nationals" = "WSH"
)

# Load the schedule and apply team name mappings
schedule <- read.csv("mlb_schedule_2025.csv")

# Ensure Home.Team and Away.Team columns are character type
schedule <- schedule %>%
  mutate(
    Home.Team = as.character(Home.Team),
    Away.Team = as.character(Away.Team)
  ) %>%
  # Apply team name mappings to convert full names to abbreviations using `recode`
  mutate(
    Home.Team = dplyr::recode(Home.Team, !!!team_name_map),
    Away.Team = dplyr::recode(Away.Team, !!!team_name_map)
  )


# Ensure Team column in team_talent is character for consistent joining
team_talent <- team_talent %>% mutate(Team = as.character(Team))

# Join team_talent with schedule to add Talent_Home and Talent_Away
schedule <- schedule %>%
  left_join(team_talent, by = c("Home.Team" = "Team")) %>%
  left_join(team_talent, by = c("Away.Team" = "Team")) %>%
  dplyr::rename(Talent_Home = Talent.x, Talent_Away = Talent.y)

# Update simulate_series to handle missing or NA values
simulate_series <- function(team1, team2, games_to_win) {
  # Check if team1 and team2 are valid and have necessary data
  if (is.null(team1) || is.null(team2) || 
      is.na(team1$Talent) || is.na(team2$Talent)) {
    return(NA)  # Return NA if team data is incomplete
  }
  
  wins_team1 <- 0
  wins_team2 <- 0
  while (wins_team1 < games_to_win && wins_team2 < games_to_win) {
    prob_team1 <- exp(0.2 * team1$Talent) / 
      (exp(0.2 * team1$Talent) + exp(0.2 * team2$Talent))
    outcome <- rbinom(1, 1, prob_team1)
    if (outcome == 1) {
      wins_team1 <- wins_team1 + 1
    } else {
      wins_team2 <- wins_team2 + 1
    }
  }
  return(ifelse(wins_team1 > wins_team2, team1$Team, team2$Team))
}

# Simplified playoff simulation
simulate_playoffs <- function(playoff_teams) {
  # Helper function to simulate a series between two teams
  simulate_series <- function(team1, team2, games_to_win) {
    if (is.na(team1$Talent) || is.na(team2$Talent)) return(NA)
    
    # Calculate win probability for team1 based on talents
    prob_team1 <- exp(0.17 * team1$Talent) / (exp(0.17 * team1$Talent) + exp(0.17 * team2$Talent))
    
    # Simulate wins needed to win the series
    wins_team1 <- sum(rbinom(games_to_win * 2 - 1, 1, prob_team1))
    if (wins_team1 >= games_to_win) {
      return(team1$Team)
    } else {
      return(team2$Team)
    }
  }
  
  # Set up the rounds in a bracket-style knockout
  rounds <- list(
    wild_card = list(games_to_win = 2, teams = playoff_teams),
    division_series = list(games_to_win = 3),
    championship_series = list(games_to_win = 4),
    world_series = list(games_to_win = 4)
  )
  
  # Simulate each round
  for (round_name in names(rounds)) {
    round_info <- rounds[[round_name]]
    
    if (round_name == "wild_card") {
      # Filter and pair teams directly for the wildcard round
      round_teams <- round_info$teams
      al_teams <- round_teams %>% filter(League == "AL")
      nl_teams <- round_teams %>% filter(League == "NL")
      
      # Simulate AL and NL Wild Card winners
      al_winner <- simulate_series(al_teams[1, ], al_teams[2, ], round_info$games_to_win)
      nl_winner <- simulate_series(nl_teams[1, ], nl_teams[2, ], round_info$games_to_win)
      playoff_teams <- playoff_teams %>% filter(Team %in% c(al_winner, nl_winner))
      
    } else {
      # For other rounds, match remaining teams directly
      if (nrow(playoff_teams) < 2) break
      team1 <- playoff_teams[1, ]
      team2 <- playoff_teams[2, ]
      
      # Determine the winner of the round
      winner <- simulate_series(team1, team2, round_info$games_to_win)
      playoff_teams <- playoff_teams %>% filter(Team == winner)
    }
  }
  
  # The last remaining team is the winner
  if (nrow(playoff_teams) == 1) {
    return(playoff_teams$Team[1])
  } else {
    return(NA)
  }
}

# Integrate the playoffs into the season simulation function
simulate_season <- function(schedule, divisions) {
  amplification_factor <- 0.13
  
  # Calculate win probabilities for each game in the season
  schedule <- schedule %>%
    mutate(
      prob_Home = exp(amplification_factor * Talent_Home) / 
        (exp(amplification_factor * Talent_Home) + exp(amplification_factor * Talent_Away)),
      prob_Home = pmin(pmax(prob_Home, 0.01), 0.99),
      outcome = rbinom(nrow(schedule), 1, prob_Home)
    )
  
  # Calculate season wins and losses
  home_wins <- schedule %>%
    filter(outcome == 1) %>%
    group_by(Home.Team) %>%
    tally(name = "Wins")
  away_wins <- schedule %>%
    filter(outcome == 0) %>%
    group_by(Away.Team) %>%
    tally(name = "Wins")
  
  results <- bind_rows(
    home_wins %>% rename(Team = Home.Team),
    away_wins %>% rename(Team = Away.Team)
  ) %>%
    group_by(Team) %>%
    summarise(Wins = sum(Wins, na.rm = TRUE)) %>%
    mutate(Losses = 162 - Wins) %>%
    left_join(divisions, by = "Team") %>%
    left_join(team_talent, by = "Team")
  
  # Determine playoff teams
  division_winners <- results %>%
    group_by(Division) %>%
    filter(Wins == max(Wins)) %>%
    ungroup()
  
  wild_cards <- results %>%
    filter(!Team %in% division_winners$Team) %>%
    group_by(League) %>%
    top_n(3, Wins) %>%
    ungroup()
  
  playoff_teams <- bind_rows(
    division_winners %>% arrange(desc(Wins)),
    wild_cards %>% arrange(desc(Wins))
  ) %>%
    select(Team, League, Division, Wins, Talent)
  
  # Run the playoffs
  world_series_winner <- simulate_playoffs(playoff_teams)
  
  list(
    season_results = results,
    playoff_teams = playoff_teams$Team,
    world_series_winner = world_series_winner
  )
}

# Running the simulation and tracking results
num_sims <- 3500
# Initialize playoff and World Series counts with consistent team names
playoff_counts <- setNames(numeric(length(teams)), teams)
ws_counts <- setNames(numeric(length(teams)), teams)
total_wins <- setNames(numeric(length(teams)), teams)
total_losses <- setNames(numeric(length(teams)), teams)

# Run the simulations
for (i in 1:num_sims) {
  sim <- simulate_season(schedule, divisions)
  
  # Update win/loss totals, handling any name mismatches
  matched_teams <- intersect(sim$season_results$Team, names(total_wins))
  
  total_wins[matched_teams] <- total_wins[matched_teams] + sim$season_results$Wins[sim$season_results$Team %in% matched_teams]
  total_losses[matched_teams] <- total_losses[matched_teams] + sim$season_results$Losses[sim$season_results$Team %in% matched_teams]
  
  # Track playoff appearances
  playoff_counts[sim$playoff_teams] <- playoff_counts[sim$playoff_teams] + 1
  
  # Track World Series appearances if there is a valid winner
  if (!is.na(sim$world_series_winner)) {
    ws_counts[sim$world_series_winner] <- ws_counts[sim$world_series_winner] + 1
  }
}

results_df <- data.frame(
  Team = as.character(names(playoff_counts)),
  Avg_Wins = total_wins / num_sims,
  Avg_Losses = total_losses / num_sims,
  Playoff_Odds = playoff_counts * 100 / num_sims,
  WS_Odds = ws_counts * 100 / num_sims
) %>%
  arrange(desc(Playoff_Odds))

preds2025 <- preds2025 %>%
  left_join(results_df, by = "Team") %>%
  mutate(
    Wins = round(Avg_Wins,1),          # Use Avg_Wins from results_df for Wins
    Losses = round(Avg_Losses,1),      # Use Avg_Losses from results_df for Losses
    Playoff_Odds = Playoff_Odds,     # Use playoff odds from results_df
    WS_Odds = WS_Odds                # Use World Series odds from results_df
  )

preds2025 <- preds2025 %>%
  group_by(Div) %>%
  arrange(desc(Wins)) %>%
  mutate(Division_Rank = row_number()) %>%
  ungroup()

team_stadium_map <- c(
  "ARI" = "diamondbacks", "ATL" = "braves", "BAL" = "orioles", "BOS" = "red_sox", 
  "CHC" = "cubs", "CHW" = "white_sox", "CIN" = "reds", "CLE" = "guardians", 
  "COL" = "rockies", "DET" = "tigers", "HOU" = "astros", "KCR" = "royals", 
  "LAA" = "angels", "LAD" = "dodgers", "MIA" = "marlins", "MIL" = "brewers", 
  "MIN" = "twins", "NYM" = "mets", "NYY" = "yankees", "OAK" = "athletics", 
  "PHI" = "phillies", "PIT" = "pirates", "SDP" = "padres", "SFG" = "giants", 
  "SEA" = "mariners", "STL" = "cardinals", "TBR" = "rays", "TEX" = "rangers", 
  "TOR" = "blue_jays", "WSH" = "nationals"
)

plot_team_depth_chart <- function(team_symbol) {
  
  # Retrieve specific team record from ranked preds2025 data
  team_record <- preds2025 %>%
    filter(Team == team_symbol) %>%
    select(Wins, Losses, Salary24, Salary25, Div, Division_Rank) %>%
    as.list()
  
  # Retrieve the playoff and WS odds for the team from the simulation results
  team_odds <- results_df %>%
    filter(Team == team_symbol) %>%
    select(Playoff_Odds, WS_Odds) %>%
    as.list()
  
  # Retrieve full team name based on the team symbol
  team_name <- mlb_team_logos %>% 
    filter(team == team_symbol) %>% 
    pull(full_name)
  stadium_id <- team_stadium_map[[team_symbol]]
  
  # Determine the color for '23 and '24 salaries
  color23 <- if (team_record$Salary24 < 236900000) "forestgreen" else if (team_record$Salary24 <= 237000000) "gold" else "maroon"
  color24 <- if (team_record$Salary25 < 240900000) "forestgreen" else if (team_record$Salary25 <= 241000000) "gold" else "maroon"
  
  # Filter data for the specific team
  team_data <- df %>%
    filter(Team == team_symbol)
  
  # Select top players for Bench and MiLB
  top_bench_players <- team_data %>%
    filter(Pos == "Bench") %>%
    arrange(desc(`2025wOBA`)) %>%
    slice_max(order_by = `2025wOBA`, n = 4)
  
  top_milb_players <- team_data %>%
    filter(Pos == "MiLB") %>%
    arrange(desc(`2025wOBA`)) %>%
    slice_max(order_by = `2025wOBA`, n = 5)
  
  # Combine Bench and MiLB players into formatted labels for vertical display
  bench_label <- paste(top_bench_players$Name, "wOBA:", top_bench_players$`2025wOBA`, sep = " ", collapse = "\n")
  milb_label <- paste(top_milb_players$Name, "wOBA:", top_milb_players$`2025wOBA`, sep = " ", collapse = "\n")
  
  # Select top pitchers (Ace, SP, RP, CL) for vertical display
  top_ace <- team_data %>%
    filter(Pos == "ACE") %>%
    slice_max(order_by = `2025FIP`, n = 1)
  
  top_sp <- team_data %>%
    filter(Pos == "SP") %>%
    arrange(desc(`2025FIP`)) %>%
    slice_min(order_by = `2025FIP`, n = 4)
  
  top_rp <- team_data %>%
    filter(Pos == "RP") %>%
    arrange(desc(`2025FIP`)) %>%
    slice_min(order_by = `2025FIP`, n = 7)
  
  top_cl <- team_data %>%
    filter(Pos == "CL") %>%
    slice_max(order_by = `2025FIP`, n = 1)
  
  # Create formatted labels for pitchers with vertical alignment
  ace_label <- paste(top_ace$Name, "\nFIP:", round(top_ace$`2025FIP`, 2), sep = " ", collapse = "\n")
  sp_label <- paste(top_sp$Name, "FIP:", round(top_sp$`2025FIP`, 2), sep = " ", collapse = "\n")
  rp_label <- paste(top_rp$Name, "FIP:", round(top_rp$`2025FIP`, 2), sep = " ", collapse = "\n")
  cl_label <- paste(top_cl$Name, "FIP:", round(top_cl$`2025FIP`, 2), sep = " ", collapse = "\n")
  
  # Define field positions and their coordinates for better plotting
  position_coords <- data.frame(
    Pos = c("ACE", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "DH",
            "SP", "RP", "CL", "Bench", "MiLB"),
    X = c(125, 125, 180, 150, 70, 95, 70, 120, 175, 20, 225, 225, 225, 0, 0), 
    Y = c(150, 210, 150, 110, 150, 110, 77, 52, 79, 140, 43, 198, 170, 190, 55)
  )
  
  # Merge player data with position coordinates
  plot_data <- merge(team_data, position_coords, by = "Pos", all.y = TRUE)
  
  # Update plot_data with formatted labels for Ace, SP, RP, CL, Bench, and MiLB positions
  plot_data <- plot_data %>%
    mutate(
      Name = ifelse(
        Pos == "Bench", bench_label, 
        ifelse(Pos == "MiLB", milb_label,
               ifelse(Pos == "ACE", ace_label,
                      ifelse(Pos == "SP", sp_label,
                             ifelse(Pos == "RP", rp_label,
                                    ifelse(Pos == "CL", cl_label,
                                           paste(Name, "\nwOBA:", round(`2025wOBA`, 3)))))))),
      `2025wOBA` = ifelse(Pos %in% c("Bench", "MiLB", "ACE", "SP", "RP", "CL"), NA, `2025wOBA`),
      `2025FIP` = ifelse(Pos %in% c("Bench", "MiLB", "ACE", "SP", "RP", "CL"), NA, `2025FIP`)
    )
  
  # Get the logo URL for the team
  logo_url <- mlb_team_logos %>% filter(team == team_symbol) %>% pull(logo_url)
  logo_image <- image_read(logo_url)
  
  # Create the plot with player names and stats
  p <- ggplot() + 
    geom_mlb_stadium(stadium_ids = stadium_id) + 
    coord_fixed() + 
    theme_void() +
    xlim(-50, 250) + 
    ylim(-50, 250) + 
    scale_y_reverse() +
    
    # Add player names and stats to the corresponding positions
    geom_text(data = plot_data, aes(x = X, y = Y, label = ifelse(is.na(Name), "N/A", Name)),
              size = 3.5, color = "black", fontface = "bold", hjust = 0.5, vjust = 0.5, lineheight = 1.2) +
    
    # Titles for Bench and MiLB sections
    geom_text(aes(x = 0, y = 172, label = "Bench Players:"), size = 4.5) +
    geom_text(aes(x = 0, y = 32, label = "MiLB Hitter Options:"), size = 4.5) +
    geom_text(aes(x = 225, y = 164, label = "RP Options:"), size = 4.5) +
    geom_text(aes(x = 225, y = 25, label = "Starting Pitchers:"), size = 4.5) +
    
    # Set title, subtitle, and labels with unique Wins-Losses record and conditional salary colors
    labs(title = paste("2025", team_name, "Team Sheet:"),
         subtitle = paste0(
           "'24 Salary: <span style='color:", color23, ";'>",
           dollar(team_record$Salary24),
           "</span> | '25 Salary: <span style='color:", color24, ";'>",
           dollar(team_record$Salary25), "</span><br>",
           "Projected Record: ", team_record$Wins, "-", team_record$Losses, 
           " | Finish: ", ordinal(team_record$Division_Rank), " in ",
           team_record$Div, "<br>",
           "Postseason Odds: ", round(team_odds$Playoff_Odds, 1),
           "% | World Series Odds: ", round(team_odds$WS_Odds, 1), "%<br>",
           "Data: RosterResource & Cots Contracts | @willtdodge | Field Design: GeomMLBStadiums"
         ),
         x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          plot.subtitle = element_markdown(hjust = 0.5, size = 14, face = "italic"))
  
  # Display the plot
  print(p)
  
  # Overlay the team logo
  grid.raster(logo_image, x = .9, y = .89, width = 0.11, height = .2, interpolate = TRUE)
}

# Example usage
plot_team_depth_chart("CHW")

