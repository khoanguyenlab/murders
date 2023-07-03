library(tidyverse)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max=3)
read_csv(url, col_names = F)

library(tidyverse)
library(dslabs)

class(co2)
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
class(co2_wide)
head(co2_wide)

co2_tidy <- co2_wide %>% pivot_longer(1:12,names_to = "month", values_to = "co2") %>%
  mutate(month = as.numeric(month))
co2_tidy %>% ggplot(aes(month, co2, color = year)) + 
  geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat %>% pivot_wider(names_from = gender, values_from = admitted)
tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp %>% unite(column_name, gender, key, sep="_") %>%
  pivot_wider(names_from=column_name, values_from = value)

# Install and load the Lahman library
# This library contains a variety of datasets related to US professional baseball
# The Batting data frame contains the offensive statistics for all baseball players over several seasons
library(Lahman)
top <- Batting %>% # top 10 home run (HR) hitters in 2016
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
People %>% as_tibble() # People data frame has demographic information for all players
str(top)
str(People)
t1 <- top %>% select(playerID, HR)
t2 <- People %>% select(playerID,nameFirst,nameLast) 
setequal(t1$playerID, t2$playerID)
top_names <- left_join(t1,t2,by="playerID")
top_names <- Salaries %>% filter(yearID == "2016") %>% select(yearID,playerID, salary) %>%
  right_join(top_names, by = "playerID")
top_names
#Inspect the AwardsPlayers table. Filter awards to include only the year 2016.
# How many players from the top 10 home run hitters won at least one award in 2016?
ta <- AwardsPlayers %>% filter(yearID == "2016" & !is.na(awardID)) 
  semi_join(top_names,ta)
#How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?
at0 <- top_names %>% select(playerID)
at <- ta %>% select(playerID) %>% setdiff(at0)
str(at)
