library("tidyverse")

#Read the EPL files and combine them.
epl13 <- read.csv('EPL_13-14.csv')
epl14 <- read.csv('EPL_14-15.csv')
epl15 <- read.csv('EPL_15-16.csv')
epl_table_og <- rbind(epl13,epl14,epl15)

#Select table with Season, Team, GF, GA, GD
epl_table_df <- select(epl_table_og, Season, Team, GF, GA, GD)
#Create a new variable for Goal Diff
epl_table_newGD <- mutate(epl_table_df, new_GD = GF - GA)
#Compare old GD with new GD
epl_diff_GD <- mutate(epl_table_newGD, diff_GD = new_GD - GD)
#There is a discrepency with the last few goal differential fields
#in the GD field from the provdided data set. new_GD minus ?10 is non numeric. 

#Create a new df of only the rows that have problematic GD
problem_GD <- mutate(epl_table_newGD, prob_GD = new_GD != GD)
problem_GD_only <- filter(problem_GD, prob_GD == TRUE)



#Part 2 
#Q1
titanic_df <- read_csv(url("https://raw.githubusercontent.com/plotly/datasets/master/titanic.csv"))
write_csv(titanic_df, "titanic_df.csv")

add_survival_status <- mutate(titanic_df, survival_status = ifelse(Survived == 0, "Did not survive", "Survived"))
                                                            

survival_stats <- group_by(add_survival_status, survival_status)
summarize(survival_stats, avg_age = mean(Age, na.rm = TRUE), avg_fare = mean(Fare, na.rm = TRUE))

#Q2
#percentage of pasengers who survived broken out by gender and the port they came from
lived_df <- add_survival_status %>%
     group_by(survival_status, Sex, Embarked) %>%
     summarize(survival_percentage = n() / nrow(add_survival_status)) %>%
     filter(survival_status == "Survived")
