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
