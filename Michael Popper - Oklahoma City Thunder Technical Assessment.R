shot_data <- read_csv("shots_data.csv") #import data into dataframe. Already loaded tidyverse package through the console.
shot_data <- shot_data %>%
  mutate(distance = sqrt(x^2+y^2)) %>% #create distance variable
  mutate(shot_type = ifelse(y <= 7.8 & abs(x) > 22, "C3", ifelse(distance > 23.75, "NC3", "2PT"))) #create shot_type variable using parameters set out in the brief
shot_data <- shot_data %>%
  mutate(shot_value = ifelse(parse_number(shot_type) == 3, 3, 2))
nrow(shot_data)

#shot distribution calculation
shot_data_summary <- shot_data %>% #create shot count by each team
  group_by(team) %>%
  summarise(team_shot_count = n())
shot_data_summary <- as.data.frame(shot_data_summary) #convert summarized tibble into a df so I can perform arithmetic with it
shot_data_summary_by_type <- shot_data %>% #create shot count by each team and shot type
  group_by(team, shot_type) %>%
  summarise(team_shot_count_by_type = n())
shot_data_summary_by_type <- shot_data_summary_by_type %>%
  left_join(shot_data_summary, by = "team") %>% #left join to have the shot count by team populate every row in the shot_data_summary_by_type df, will then perform calculation in mutate function
  mutate(Shot_Distribution = team_shot_count_by_type/team_shot_count)

#eFG calculation
shot_data_eFG <- shot_data %>%
  group_by(team, shot_type) %>%
  summarise(eFG_individual = ifelse(shot_value == 3, 1.5*sum(fgmade)/n(), sum(fgmade)/n())) %>% #group_by calculated eFG for each row, so need to take the average to condense to the same format as the shot_distribution df
  summarise(eFG = mean(eFG_individual))
#Combination and cleanup
shot_data_final <- shot_data_summary_by_type %>%
  left_join(shot_data_eFG, by = c("team" = "team", "shot_type" = "shot_type")) %>% #join eFG df to shot_distribution df
  select(-team_shot_count)
shot_data_final
