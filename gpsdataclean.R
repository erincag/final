# use this code to read in data needed for website, and clean file before continuing with data analysis

#read in full excel with player data
#only need first 18 columns
library(dplyr)
player.data <- readxl::read_excel("Workout_Routine_Dirty.xlsx")
player.data <- player.data[,1:18]


#find average for wellness metrics by player
player.mean <- player.data |>
  group_by(Name) |>
  summarize(
    avg.duration = round(mean(Sleep_Duration, na.rm = TRUE)),
    avg.score = round(mean(Sleep_Score, na.rm = TRUE)),
    avg.quality = round(mean(Sleep_Quality, na.rm = TRUE), digits = 2),
    avg.soreness = round(mean(Soreness, na.rm = TRUE), digits = 2),
    avg.stress = round(mean(Stress, na.rm = TRUE), digits = 2)
  )

#find average RPE for each player
training.avg <- player.data |>
  group_by(Date) |>
  summarize(
    avg.RPE = round(mean(RPE, na.rm = TRUE), digits = 2)
  )

#fill in NA data for each player based on their average data
new <- player.data |>
  left_join(player.mean, by = "Name") |>
  mutate(
    Sleep_Duration = ifelse(is.na(Sleep_Duration), avg.duration, Sleep_Duration),
    Sleep_Score = ifelse(is.na(Sleep_Score), avg.score, Sleep_Score),
    Sleep_Quality = ifelse(is.na(Sleep_Quality), avg.quality, Sleep_Quality),
    Soreness = ifelse(is.na(Soreness), avg.soreness, Soreness),
    Stress = ifelse(is.na(Stress), avg.stress, Stress)
  )

#fill in missing RPE data by the average for the date 
cleaned <- new |>
  left_join(training.avg, by = "Date") |>
  mutate(
    RPE = ifelse(is.na(RPE), avg.RPE, RPE)
  )

#remove the goalkeepers from the data and shot by date 
full.perf.data <- cleaned[cleaned$Position != "Goalkeeper",1:18]
full.perf.data$Date = as.Date(full.perf.data$Date, format = "%m/%d/%y")