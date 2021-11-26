library(tidyverse)
#Programmers note:
#If we could use multiple functions, I would create a single function to do
#the analysis, then a second function to determine the time frame to evaluate on
#Or make use of object classes to pull function in one call
#eliminating about 250 - 300 lines of code

#Date = "12/15/2019"

EPL_Standings("04/15/2019", "2018/19")

#Start function
EPL_Standings <- function(Date, Season) {
  #set Date argument = mdy or dmy (Date)
  Date = lubridate::mdy(Date)

#Use control flow to read correct file
    if (Season == "2021/22") {
    EPL_current <- read_csv(url("http://www.football-data.co.uk/mmz4281/2122/E0.csv"))
#Clean date using mdy or dmy
    EPL_current_trim <- select(EPL_current, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_current_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

#filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
    
#Create TeamName and Record column
    EPL_record_dummies <- mutate(EPL_filtered, home_wins = ifelse(FTR == "H", 1, 0),
                                 away_wins = ifelse(FTR == "A", 1, 0),
                                 draw = ifelse(FTR == "D", 1, 0))
    #Calculate Wins
    Wins <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins, away_wins))
    #Calculate Losses
    Losses <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                 (sum(home_wins, away_wins) - sum(draw) + 1)))
    #Calculate Draws
    Draws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
    #bring data frames together
    Record <- mutate(Wins, combine_record_columns = cbind(Wins, Losses, Draws))
    
    #set data type as character for wins, losses, draws
    Record_chr <- as.character(Wins$wins,
                               Losses$losses,
                               Draws$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record, TeamName, record = str_c(Wins$wins,"-",
                                                             Losses$losses,"-",
                                                             Draws$draws))
    
    #Clean to show TeamName and Record
    Record_cleaned <- transmute(Record_concat, TeamName, Record = record)

#Points Earned
    Points_earned <- mutate(EPL_filtered, Home_PE = ifelse(FTR == "H",3,0),
                                Away_PE = ifelse(FTR == "A",3,0),
                                Draw_PE = ifelse(FTR == "D",1,0))
    #Compute points earned
    EPL_PE<- Points_earned %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(points = sum(Home_PE, Away_PE, Draw_PE, na.rm = TRUE))
    
    #Add PE to Record_cleaned
    Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points)
  
#Home Record
    Home_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins))
    
    Home_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins) - sum(draw) + 1)))
    
    Home_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Away_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(away_wins))
    
    Away_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(away_wins) - sum(draw) + 1) < 0,0,
                                (sum(away_wins) - sum(draw) + 1)))
    
    Away_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Matches <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(matches = sum(home_wins, away_wins, draw) + (ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                                                    (sum(home_wins, away_wins) - sum(draw)))) + 1)
    
    #Add Matches Played to Away_Record_cleaned_PE
    Matches_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                        HomeRecord = Record_concat$record,
                                        AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                                        Points = EPL_PE$points)
    
#Points per match (PPM)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    Points_Per_Match <- mutate(EPL_PE, PPM = Matches_Record_cleaned_PE$Points / Matches_Record_cleaned_PE$MatchesPlayed)

    
    PPM_cleaned <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                           HomeRecord = Record_concat$record,
                                           AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                                           Points = EPL_PE$points,
                                           PPM = Points_Per_Match$PPM)
    
#Point Percentage = points / 3 * number of games played (PtPct)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    PtPct <- mutate(PPM_cleaned, PtPct = (EPL_PE$points / (3*Matches$matches)))
  
#Goal Scored (GS) Sum FTHG and FTAG grouped by team name
    Goal_scored_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(Goals_home = sum(FTHG))
    
    Goal_scored_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(Goals_Away = sum(FTAG))
    
    Goal_scored <- mutate(PtPct, GS = Goal_scored_home$Goals_home + Goal_scored_away$Goals_Away)
    
#Goals Scored per match (GSM)
    Goals_per_match <- mutate(Goal_scored, GSM = GS / Matches$matches)
    
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
    Goals_allowed_pm <- mutate(Goals_allowed, GAM = Goals_allowed$GA / Matches$matches)
    
#Arrange output 
    Final_output <- arrange(Goals_allowed_pm, desc(Goals_allowed_pm$PPM), desc(Wins$wins), desc(Goals_allowed_pm$GSM), Goals_allowed_pm$GAM)
    
    
  } else if (Season == "2020/21") {
    EPL_20_21 <- read_csv(url("http://www.football-data.co.uk/mmz4281/2021/E0.csv"))
    #Clean date using mdy or dmy
    EPL_current_trim <- select(EPL_20_21, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_current_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
    
    #Create TeamName and Record column
    EPL_record_dummies <- mutate(EPL_filtered, home_wins = ifelse(FTR == "H", 1, 0),
                                 away_wins = ifelse(FTR == "A", 1, 0),
                                 draw = ifelse(FTR == "D", 1, 0))
    #Calculate Wins
    Wins <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins, away_wins))
    #Calculate Losses
    Losses <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins, away_wins) - sum(draw) + 1)))
    #Calculate Draws
    Draws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
    #bring data frames together
    Record <- mutate(Wins, combine_record_columns = cbind(Wins, Losses, Draws))
    
    #set data type as character for wins, losses, draws
    Record_chr <- as.character(Wins$wins,
                               Losses$losses,
                               Draws$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record, TeamName, record = str_c(Wins$wins,"-",
                                                             Losses$losses,"-",
                                                             Draws$draws))
    
    #Clean to show TeamName and Record
    Record_cleaned <- transmute(Record_concat, TeamName, Record = record)
    
    #Points Earned
    Points_earned <- mutate(EPL_filtered, Home_PE = ifelse(FTR == "H",3,0),
                            Away_PE = ifelse(FTR == "A",3,0),
                            Draw_PE = ifelse(FTR == "D",1,0))
    #Compute points earned
    EPL_PE<- Points_earned %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(points = sum(Home_PE, Away_PE, Draw_PE, na.rm = TRUE))
    
    #Add PE to Record_cleaned
    Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points)
    
    #Home Record
    Home_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins))
    
    Home_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins) - sum(draw) + 1)))
    
    Home_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Away_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(away_wins))
    
    Away_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(away_wins) - sum(draw) + 1) < 0,0,
                                (sum(away_wins) - sum(draw) + 1)))
    
    Away_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Matches <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(matches = sum(home_wins, away_wins, draw) + (ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                                                    (sum(home_wins, away_wins) - sum(draw)))) + 1)
    
    #Add Matches Played to Away_Record_cleaned_PE
    Matches_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                           HomeRecord = Record_concat$record,
                                           AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                                           Points = EPL_PE$points)
    
    #Points per match (PPM)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    Points_Per_Match <- mutate(EPL_PE, PPM = Matches_Record_cleaned_PE$Points / Matches_Record_cleaned_PE$MatchesPlayed)
    
    
    PPM_cleaned <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                             HomeRecord = Record_concat$record,
                             AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                             Points = EPL_PE$points,
                             PPM = Points_Per_Match$PPM)
    
    #Point Percentage = points / 3 * number of games played (PtPct)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    PtPct <- mutate(PPM_cleaned, PtPct = (EPL_PE$points / (3*Matches$matches)))
    
    #Goal Scored (GS) Sum FTHG and FTAG grouped by team name
    Goal_scored_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(Goals_home = sum(FTHG))
    
    Goal_scored_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(Goals_Away = sum(FTAG))
    
    Goal_scored <- mutate(PtPct, GS = Goal_scored_home$Goals_home + Goal_scored_away$Goals_Away)
    
    #Goals Scored per match (GSM)
    Goals_per_match <- mutate(Goal_scored, GSM = GS / Matches$matches)
    
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
    Goals_allowed_pm <- mutate(Goals_allowed, GAM = Goals_allowed$GA / Matches$matches)
    
    #Arrange output 
    Final_output <- arrange(Goals_allowed_pm, desc(Goals_allowed_pm$PPM), desc(Wins$wins), desc(Goals_allowed_pm$GSM), Goals_allowed_pm$GAM)
    
    
  } else if (Season == "2019/20") {
    EPL_19_20 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1920/E0.csv"))
    #Clean date using mdy or dmy
    EPL_current_trim <- select(EPL_19_20, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_current_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
    
    #Create TeamName and Record column
    EPL_record_dummies <- mutate(EPL_filtered, home_wins = ifelse(FTR == "H", 1, 0),
                                 away_wins = ifelse(FTR == "A", 1, 0),
                                 draw = ifelse(FTR == "D", 1, 0))
    #Calculate Wins
    Wins <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins, away_wins))
    #Calculate Losses
    Losses <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins, away_wins) - sum(draw) + 1)))
    #Calculate Draws
    Draws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
    #bring data frames together
    Record <- mutate(Wins, combine_record_columns = cbind(Wins, Losses, Draws))
    
    #set data type as character for wins, losses, draws
    Record_chr <- as.character(Wins$wins,
                               Losses$losses,
                               Draws$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record, TeamName, record = str_c(Wins$wins,"-",
                                                             Losses$losses,"-",
                                                             Draws$draws))
    
    #Clean to show TeamName and Record
    Record_cleaned <- transmute(Record_concat, TeamName, Record = record)
    
    #Points Earned
    Points_earned <- mutate(EPL_filtered, Home_PE = ifelse(FTR == "H",3,0),
                            Away_PE = ifelse(FTR == "A",3,0),
                            Draw_PE = ifelse(FTR == "D",1,0))
    #Compute points earned
    EPL_PE<- Points_earned %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(points = sum(Home_PE, Away_PE, Draw_PE, na.rm = TRUE))
    
    #Add PE to Record_cleaned
    Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points)
    
    #Home Record
    Home_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins))
    
    Home_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins) - sum(draw) + 1)))
    
    Home_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Away_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(away_wins))
    
    Away_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(away_wins) - sum(draw) + 1) < 0,0,
                                (sum(away_wins) - sum(draw) + 1)))
    
    Away_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Matches <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(matches = sum(home_wins, away_wins, draw) + (ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                                                    (sum(home_wins, away_wins) - sum(draw)))) + 1)
    
    #Add Matches Played to Away_Record_cleaned_PE
    Matches_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                           HomeRecord = Record_concat$record,
                                           AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                                           Points = EPL_PE$points)
    
    #Points per match (PPM)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    Points_Per_Match <- mutate(EPL_PE, PPM = Matches_Record_cleaned_PE$Points / Matches_Record_cleaned_PE$MatchesPlayed)
    
    
    PPM_cleaned <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                             HomeRecord = Record_concat$record,
                             AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                             Points = EPL_PE$points,
                             PPM = Points_Per_Match$PPM)
    
    #Point Percentage = points / 3 * number of games played (PtPct)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    PtPct <- mutate(PPM_cleaned, PtPct = (EPL_PE$points / (3*Matches$matches)))
    
    #Goal Scored (GS) Sum FTHG and FTAG grouped by team name
    Goal_scored_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(Goals_home = sum(FTHG))
    
    Goal_scored_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(Goals_Away = sum(FTAG))
    
    Goal_scored <- mutate(PtPct, GS = Goal_scored_home$Goals_home + Goal_scored_away$Goals_Away)
    
    #Goals Scored per match (GSM)
    Goals_per_match <- mutate(Goal_scored, GSM = GS / Matches$matches)
    
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
    Goals_allowed_pm <- mutate(Goals_allowed, GAM = Goals_allowed$GA / Matches$matches)
    
    #Arrange output 
    Final_output <- arrange(Goals_allowed_pm, desc(Goals_allowed_pm$PPM), desc(Wins$wins), desc(Goals_allowed_pm$GSM), Goals_allowed_pm$GAM)
    
  } else if (Season == "2018/19") {
    EPL_18_19 <- read_csv(url("http://www.football-data.co.uk/mmz4281/1819/E0.csv"))
    #Clean date using mdy or dmy
    EPL_current_trim <- select(EPL_18_19, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    EPL_date_clean <- mutate(EPL_current_trim, Date_dmy = lubridate::dmy(Date))
    EPL_date_trim <- select(EPL_date_clean, Date_dmy, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
    
    #filter table_Date <= Date argument
    EPL_filtered <- filter(EPL_date_trim, Date_dmy <= Date)
    
    #Create TeamName and Record column
    EPL_record_dummies <- mutate(EPL_filtered, home_wins = ifelse(FTR == "H", 1, 0),
                                 away_wins = ifelse(FTR == "A", 1, 0),
                                 draw = ifelse(FTR == "D", 1, 0))
    #Calculate Wins
    Wins <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins, away_wins))
    #Calculate Losses
    Losses <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins, away_wins) - sum(draw) + 1)))
    #Calculate Draws
    Draws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
    #bring data frames together
    Record <- mutate(Wins, combine_record_columns = cbind(Wins, Losses, Draws))
    
    #set data type as character for wins, losses, draws
    Record_chr <- as.character(Wins$wins,
                               Losses$losses,
                               Draws$draws)
    
    #string concatenate to separate wins,losses,draws with dash -
    Record_concat <- mutate(Record, TeamName, record = str_c(Wins$wins,"-",
                                                             Losses$losses,"-",
                                                             Draws$draws))
    
    #Clean to show TeamName and Record
    Record_cleaned <- transmute(Record_concat, TeamName, Record = record)
    
    #Points Earned
    Points_earned <- mutate(EPL_filtered, Home_PE = ifelse(FTR == "H",3,0),
                            Away_PE = ifelse(FTR == "A",3,0),
                            Draw_PE = ifelse(FTR == "D",1,0))
    #Compute points earned
    EPL_PE<- Points_earned %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(points = sum(Home_PE, Away_PE, Draw_PE, na.rm = TRUE))
    
    #Add PE to Record_cleaned
    Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, Points = EPL_PE$points)
    
    #Home Record
    Home_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(home_wins))
    
    Home_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(home_wins) - sum(draw) + 1) < 0,0,
                                (sum(home_wins) - sum(draw) + 1)))
    
    Home_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Away_Ws <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(wins = sum(away_wins))
    
    Away_Ls <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(losses = ifelse((sum(away_wins) - sum(draw) + 1) < 0,0,
                                (sum(away_wins) - sum(draw) + 1)))
    
    Away_Ds <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(draws = sum(draw))
    
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
    Matches <- EPL_record_dummies %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(matches = sum(home_wins, away_wins, draw) + (ifelse((sum(home_wins, away_wins) - sum(draw) + 1) < 0,0,
                                                                    (sum(home_wins, away_wins) - sum(draw)))) + 1)
    
    #Add Matches Played to Away_Record_cleaned_PE
    Matches_Record_cleaned_PE <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                                           HomeRecord = Record_concat$record,
                                           AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                                           Points = EPL_PE$points)
    
    #Points per match (PPM)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    Points_Per_Match <- mutate(EPL_PE, PPM = Matches_Record_cleaned_PE$Points / Matches_Record_cleaned_PE$MatchesPlayed)
    
    
    PPM_cleaned <- transmute(EPL_PE, TeamName, Record = Record_cleaned$Record, 
                             HomeRecord = Record_concat$record,
                             AwayRecord = Away_Record_concat$record, MatchesPlayed = Matches$matches,
                             Points = EPL_PE$points,
                             PPM = Points_Per_Match$PPM)
    
    #Point Percentage = points / 3 * number of games played (PtPct)
    #use mutate instead of group by and summarize in order to avoid value aggregating down the column 
    PtPct <- mutate(PPM_cleaned, PtPct = (EPL_PE$points / (3*Matches$matches)))
    
    #Goal Scored (GS) Sum FTHG and FTAG grouped by team name
    Goal_scored_home <- EPL_date_trim %>% 
      group_by(TeamName = HomeTeam) %>% 
      summarize(Goals_home = sum(FTHG))
    
    Goal_scored_away <- EPL_date_trim %>% 
      group_by(TeamName = AwayTeam) %>% 
      summarize(Goals_Away = sum(FTAG))
    
    Goal_scored <- mutate(PtPct, GS = Goal_scored_home$Goals_home + Goal_scored_away$Goals_Away)
    
    #Goals Scored per match (GSM)
    Goals_per_match <- mutate(Goal_scored, GSM = GS / Matches$matches)
    
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
    Goals_allowed_pm <- mutate(Goals_allowed, GAM = Goals_allowed$GA / Matches$matches)
    
    #Arrange output 
    Final_output <- arrange(Goals_allowed_pm, desc(Goals_allowed_pm$PPM), desc(Wins$wins), desc(Goals_allowed_pm$GSM), Goals_allowed_pm$GAM)
    
  }
  return (Final_output)
}


