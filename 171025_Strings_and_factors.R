library(tidyverse)
library(janitor)
library(haven)
library(rvest)

library(stringr)
library(forcats)

# PULSE data
pulse_data = read_sas("../data/public_pulse_data.sas7bdat") %>%
  clean_names() %>%
  gather(key = visit, value = bdi, bdiscore_bl:bdiscore_12m) %>%
  mutate(visit = str_replace(visit, "bdiscore_", ""),
         visit = str_replace(visit, "bl", "00m"),
         visit = fct_relevel(visit, str_c(c("00", "01", "06", "12"), "m"))) %>%
  arrange(id, visit)

print(pulse_data, n = 12)


# NSDUH
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_xml = read_html(url)

table_marj = (drug_use_xml %>% html_nodes(css = "table"))[[1]] %>%
  html_table() %>%
  .[-1,] %>%
  as_tibble()

data_marj = 
  table_marj %>%
  select(-contains("P Value")) %>%
  gather(key = key, value = percent, -State) %>%
  separate(key, into = c("age", "year"), sep = "\\(") %>%
  mutate(year = str_sub(year, 1, -2),
         percent = str_replace(percent, "[a-z]", ""),
         percent = as.numeric(percent)) %>%
  filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))

data_marj %>%
  filter(age == "12-17") %>% 
  mutate(State = fct_reorder(State, percent)) %>%  # treat State as a factor are reorder according to percent.
  ggplot(aes(x = State, y = percent, color = year)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# bm1 example
ins1 = c(1.53, 1.61, 3.75, 2.89, 3.26)
ins2 = c(3.15, 3.96, 3.59, 1.89, 1.45, 1.56)
ins3 = c(3.89, 3.68, 5.70, 5.62, 5.79, 5.33)
ins4 = c(8.18, 5.64, 7.36, 5.33, 8.82, 5.26, 7.10)
ins5 = c(5.86, 5.46, 5.69, 6.49, 7.81, 9.03, 7.49, 8.98)

bmi_data = data_frame(
  insulin = c(ins1, ins2, ins3, ins4, ins5),
  ind = c(rep(1, length(ins1)),
          rep(2, length(ins2)),
          rep(3, length(ins3)),
          rep(4, length(ins4)),
          rep(5, length(ins5)))
)

bmi_data = bmi_data %>% 
  mutate(ind = factor(ind), 
         ind = fct_recode(ind,
                          "level_1" = "1",
                          "level_2" = "2",
                          "level_3" = "3",
                          "level_4" = "4",
                          "level_5" = "5")
  )

bmi_data %>% 
  ggplot(aes(x = ind, y = insulin)) + geom_boxplot()


## New York City restaurants
library(tidyverse)
library(httr)
library(jsonlite)

get_all_inspections = function(url) {
  
  all_inspections = vector("list", length = 0)
  
  loop_index = 1
  chunk_size = 50000
  DO_NEXT = TRUE
  
  while (DO_NEXT) {
    message("Getting data, page ", loop_index)
    
    all_inspections[[loop_index]] = 
      GET(url,
          query = list(`$order` = "zipcode",
                       `$limit` = chunk_size,
                       `$offset` = as.integer((loop_index - 1) * chunk_size)
          )
      ) %>%
      content("text") %>%
      fromJSON() %>%
      as_tibble()
    
    DO_NEXT = dim(all_inspections[[loop_index]])[1] == chunk_size
    loop_index = loop_index + 1
  }
  
  all_inspections
  
}

url = "https://data.cityofnewyork.us/resource/9w7m-hzhe.json"

nyc_inspections = get_all_inspections(url) %>%
  bind_rows() 


nyc_inspections %>% 
  group_by(boro, grade) %>% 
  summarize(n = n()) %>% 
  spread(key = grade, value = n)

nyc_inspections =
  nyc_inspections %>%
  filter(grade %in% c("A", "B", "C"), boro != "Missing") %>% 
  mutate(boro = str_to_title(boro))

nyc_inspections %>% 
  filter(str_detect(dba, regex("pizza", ignore_case = TRUE))) %>% 
  group_by(boro, grade) %>% 
  summarize(n = n()) %>% 
  spread(key = grade, value = n)

# make bar plot
nyc_inspections %>% 
  filter(str_detect(dba, regex("pizza", ignore_case = TRUE))) %>%
  ggplot(aes(x = boro, fill = grade)) + geom_bar()

# Reorder the bar plot
nyc_inspections %>% 
  filter(str_detect(dba, regex("pizza", ignore_case = TRUE))) %>%
  mutate(boro = fct_infreq(boro)) %>% #reorder according to the frequency
  ggplot(aes(x = boro, fill = grade)) + geom_bar()

# rename (not work)
nyc_inspections %>% 
  filter(str_detect(dba, regex("pizza", ignore_case = TRUE))) %>%
  mutate(boro = fct_infreq(boro),
         boro = replace(boro, which(boro == "Brooklyn"), "Hipsterville") %>%
  ggplot(aes(x = boro, fill = grade)) + geom_bar()

# rename (a feasible way for the factor)
nyc_inspections %>% 
  filter(str_detect(dba, regex("pizza", ignore_case = TRUE))) %>%
  mutate(boro = fct_infreq(boro),
         boro = fct_recode(boro, "Hipsterville" = "Brooklyn")) %>%
  ggplot(aes(x = boro, fill = grade)) + geom_bar()
