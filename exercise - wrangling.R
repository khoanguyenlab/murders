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


# Webscraping -  Major League Baseball payrolls
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
html_table(nodes[1:4])
html_table(nodes[19:21])

tab_1 <- html_table(nodes[10])[[1]]
tab_1 <- tab_1 %>% slice(-1) 
colnames(tab_1) <- c("No.","Team", "Payroll", "Average")
tab_1 <- tab_1 %>% select(Team, Payroll, Average)
tab_2 <- html_table(nodes[19])[[1]]
tab_2 <- tab_2 %>% slice(-1) 
colnames(tab_2) <- c("Team", "Payroll", "Average")
tab_2 <- tab_2 %>% select(Team, Payroll, Average)
tab_f <- full_join(tab_1, tab_2, by = "Team")
nrow(tab_f)

# Webscraping - Brexit referendum
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- html_nodes(h, "table")
tab
html_table(tab, fill=T)

#Case Study: Extracting a Table from a PDF
library("pdftools")
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
raw_data_research_funding_rates <- txt[2]
data("raw_data_research_funding_rates")
raw_data_research_funding_rates %>% head
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

#Assessment Part 2: String Processing Part 3
#Import raw Brexit referendum polling data from Wikipedia
library(rvest)
library(tidyverse)
library(stringr)
polls

polls <- polls %>% 
  filter(str_detect(Remain,"%"))
colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
as.numeric(polls$remain)/100
as.numeric(str_remove(polls$remain, "%"))/100
parse_number(polls$remain)/100
as.numeric(str_remove(polls$remain, "%"))
polls

#Option 1
temp <- str_extract_all(polls$dates,"\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

#option 2
"[0-9]+\\s[a-zA-Z]+"

#Option 3
"\\d{1,2}\\s[a-zA-Z]+"

#Option 4
"\\d+\\s[a-zA-Z]{3,5}"
