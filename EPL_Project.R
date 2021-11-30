EPL_Standings <- function(Date, Season) {
  #load library
  library(tidyverse)
  #set Date argument = mdy or dmy (Date)
  Date = lubridate::mdy(Date)
  EPL_season_list <- c("2018/19", "2019/20", "2020/21", "2021/22")
  
  for (i in EPL_season_list) {
      EPL_1819 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1819/E0.csv"))
      EPL_1920 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1920/E0.csv"))
      EPL_2021 <- read_csv(url("http://www.football-data.co.uk/mmz4281/2021/E0.csv"))
      EPL_2122 <- read_csv(url("http://www.football-data.co.uk/mmz4281/2122/E0.csv"))
      break
  }
  if (Season == EPL_season_list[1]) {
    EPL_1819_trim <- select(EPL_1819, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_1819_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
  } else if (Season == EPL_season_list[2]) {
    EPL_1920_trim <- select(EPL_1920, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_1920_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
  } else if (Season == EPL_season_list[3]) {
    EPL_2021_trim <- select(EPL_2021, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_2021_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
  } else if (Season == EPL_season_list[4]) {
    EPL_2122_trim <- select(EPL_2122, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_2122_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
  } else {
    "Please choose season from available list"
  }
  
    #Create TeamName and Record column
    EPL_record_dummies <- mutate(EPL_filtered, home_wins = ifelse(FTR == "H", 1, 0),
                                 away_wins = ifelse(FTR == "A", 1, 0),
                                 home_losses = ifelse(FTR == "A", 1, 0),
                                 away_losses = ifelse(FTR == "H", 1, 0),
                                 home_draw = ifelse(FTR == "D", 1, 0),
                                 away_draw = ifelse(FTR == "D", 1, 0))
    #Calculate Wins
    Home_Wins <- EPL_record_dummies %>% 
      group_by(HomeTeam) %>% 
      summarize(wins = sum(home_wins))
    
    Away_Wins <- EPL_record_dummies %>% 
      group_by(AwayTeam) %>% 
      summarise(wins = sum(away_wins))
    #Calculate Losses
    Home_Losses <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = sum(home_losses))
    Away_Losses <- EPL_record_dummies %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarise(losses = sum(away_losses))
    #Calculate Draws
   Home_Draws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(home_draw))
   Away_Draws <- EPL_record_dummies %>% 
     group_by(TeamName = AwayTeam) %>% 
     summarise(draws = sum(away_draw))
    
    #bring data frames together
    Record <- mutate(Home_Wins, combine_record_columns = cbind(Home_Wins, Home_Losses, Home_Draws,
                                                               Away_Wins, Away_Losses, Away_Draws))
    
    #set data type as character for wins, losses, draws
    Record_chr <- as.character(Home_Wins$wins,
                               Home_Losses$losses,
                               Home_Draws$draws,
                               Away_Wins$wins,
                               Away_Losses$losses,
                               Away_Draws$draws)

    #Combine results totals as character string
    Record_combined <- mutate(Record, Wins = (Home_Wins$wins + Away_Wins$wins),
                                                Losses = (Home_Losses$losses + Away_Losses$losses),
                                                Draws = (Home_Draws$draws + Away_Draws$draws))
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record_combined, record = str_c(Wins,"-",
                                                            Losses,"-",
                                                            Draws))
    
    #Clean to show TeamName and Record
    Record_cleaned <- transmute(Record_concat, TeamName = HomeTeam, Record = record)
    
    #Points Earned
    Points_earned <- mutate(Record_combined, Wins_PE = Wins * 3,
                                             Draw_PE = Draws * 1)
    #Compute points earned
    EPL_PE<- Points_earned %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(points = sum(Wins_PE, Draw_PE, na.rm = TRUE))
    
    #Add PE to Record_cleaned
    Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points)
    
    #Home Record
    Home_Ws <- Home_Wins %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(wins))
    
    Home_Ls <- Home_Losses %>% 
      group_by(TeamName) %>% 
      summarize(losses = sum(losses))
    
    Home_Ds <- Home_Draws %>% 
      group_by(TeamName) %>% 
      summarize(draws = sum(draws))
    
    Record_Home <- mutate(Home_Ws, combine_record_columns = cbind(Home_Ws, Home_Ls, Home_Ds))
    
    #set data type as character for wins, losses, draws
    Record_chr_Home <- as.character(Home_Ws$wins,
                                    Home_Ls$losses,
                                    Home_Ds$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record_Home, TeamName, record = str_c(Home_Ws$wins,"-",
                                                                  Home_Ls$losses,"-",
                                                                  Home_Ds$draws))
    #Add PE to Record_cleaned
    Home_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points, Home_Record = Record_concat$record)
    
    #Away Record
    Away_Ws <- Away_Wins %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(wins = sum(wins))
    
    Away_Ls <- Away_Losses %>% 
      group_by(TeamName) %>% 
      summarize(losses = sum(losses))
    
    Away_Ds <- Away_Draws %>% 
      group_by(TeamName) %>% 
      summarize(draws = sum(draws))
    
    Record_Away <- mutate(Away_Ws, combine_record_columns = cbind(Away_Ws, Away_Ls, Away_Ds))
    
    #set data type as character for wins, losses, draws
    Record_chr_Away <- as.character(Away_Ws$wins,
                                    Away_Ls$losses,
                                    Away_Ds$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Away_Record_concat <- mutate(Record_Away, TeamName, record = str_c(Away_Ws$wins,"-",
                                                                       Away_Ls$losses,"-",
                                                                       Away_Ds$draws))
    #Add PE to Record_cleaned
    Away_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                        Points = EPL_PE$points, HomeRecord = Record_concat$record,
                                        AwayRecord = Away_Record_concat$record)
    #Matches Played
    Home_Matches <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(matches = n())
    Away_Matches <- EPL_record_dummies %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarise(matches = n())
    Matches_played <- mutate(Home_Matches, Matches = Home_Matches$matches + Away_Matches$matches)
    
    #Add Matches Played to Away_Record_cleaned_PE
    Matches_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                           HomeRecord = Record_concat$record,
                                           AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches_played$Matches,
                                           Points = EPL_PE$points)
    
    #Points per match (PPM)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    Points_Per_Match <- mutate(EPL_PE, PPM = Matches_Record_cleaned_PE$Points / Matches_Record_cleaned_PE$MatchesPlayed)
    
    
    PPM_cleaned <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                             HomeRecord = Record_concat$record,
                             AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches_played$Matches,
                             Points = EPL_PE$points,
                             PPM = Points_Per_Match$PPM)
    
    #Point Percentage = points / 3 * number of games played (PtPct)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    PtPct <- mutate(PPM_cleaned, PtPct = (EPL_PE$points / (3*Matches_played$Matches)))
    
    #Goal Scored (GS) Sum FTHG and FTAG grouped by team name
    Goal_scored_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(Goals_home = sum(FTHG))
    
    Goal_scored_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(Goals_Away = sum(FTAG))
    
    Goal_scored <- mutate(PtPct, GS = Goal_scored_home$Goals_home + Goal_scored_away$Goals_Away)
    
    #Goals Scored per match (GSM)
    Goals_per_match <- mutate(Goal_scored, GSM = GS / Matches_played$Matches)
    
    #Goals allowed
    #Calculate goals allowed as the home team
    Goals_allowed_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(gs_allowed = sum(FTAG))
    #Calculate goals allowed as the away team
    Goals_allowed_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(gs_allowed_away = sum(FTHG))
    #Sum the totals of goals allowed as home and away, add to data frame
    Goals_allowed <- mutate(Goals_per_match, 
                            GA = Goals_allowed_home$gs_allowed + Goals_allowed_away$gs_allowed_away)
    
    #Goals allowed per match (GAM)
    Goals_allowed_pm <- mutate(Goals_allowed, GAM = Goals_allowed$GA / Matches_played$Matches)
    
    #Arrange output 
    Final_output <- arrange(Goals_allowed_pm, desc(Goals_allowed_pm$PPM), desc(Record_combined$Wins), desc(Goals_allowed_pm$GSM), Goals_allowed_pm$GAM)
    
    return(Final_output)
}

#Test
EPL_Standings("06/01/2021", "2020/21")
#Date = "06/01/2021"
#Season = "2020/21"

