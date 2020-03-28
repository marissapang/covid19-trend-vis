setwd("~/Dropbox/django_projects/covid19-Rcode")

require(data.table)
require(dplyr)
require(tidyr)

# read global-level data at country level
confirmed = fread("data/time_series_covid19_confirmed_global.csv", stringsAsFactors = F)
confirmed = confirmed %>% select(-Lat, -Long)
confirmed = gather(confirmed, Date, Num_Confirmed, 3:ncol(confirmed), factor_key=TRUE)
colnames(confirmed) = c("State", "Country", "Date", "Num_Confirmed")
confirmed = confirmed %>% mutate(Date = as.Date(Date, format = "%m/%d/%y"))

# remap hong kong
confirmed = confirmed %>% mutate(Country = ifelse(State == "Hong Kong", "Hong Kong", Country))

# read us data at county and state level
us_confirmed = fread("data/us-counties.csv", stringsAsFactors = F) %>% select(-fips)
colnames(us_confirmed) = c("Date", "County", "State", "Num_Confirmed", "Num_Deaths")
us_confirmed = us_confirmed %>% 
  group_by(Date, State) %>%
  summarise(Num_Confirmed = sum(Num_Confirmed)) %>%
  ungroup() %>%
  mutate(Date = as.Date(Date))

county_cases_total = us_confirmed %>% group_by(Date) %>% summarise(Num_Confirmed_By_County = sum(Num_Confirmed)) %>% ungroup()

# calculate discrepancies
us_confirmed_adjustment = confirmed %>% filter(Country == "US")
us_confirmed_adjustment = us_confirmed_adjustment %>% left_join(county_cases_total, "Date")

us_confirmed_adjustment = us_confirmed_adjustment %>%
  mutate(Adj_Num_Confirmed = pmax(Num_Confirmed, Num_Confirmed_By_County, na.rm=T),
         Unclassified = Adj_Num_Confirmed - Num_Confirmed_By_County,
         Unclassified = ifelse(is.na(Num_Confirmed_By_County), Num_Confirmed, Unclassified))


# replace US data
confirmed = confirmed %>% filter(Country != 'US') 
confirmed = rbind(confirmed, us_confirmed %>% mutate(Country = "United States") %>% select(State, Country, Date, Num_Confirmed))
confirmed = rbind(confirmed, us_confirmed_adjustment %>% 
                    mutate(Country = "United States", State = "Unassigned") %>%
                    select(State, Country, Date, Num_Confirmed = Unclassified))

x = confirmed %>% filter(Country == "United States")
y = x %>% group_by(Date) %>% summarise(Num_Confirmed = sum(Num_Confirmed))
  
# #### check state data for US ####
# confirmed = confirmed %>% 
#   left_join(us_states, by=c("Country_Or_Region", "Province_Or_State")) %>%
#   mutate(Province_Or_State = ifelse(Country_Or_Region == "US", State, Province_Or_State)) %>%
#   select(-State, -County) %>%
#   mutate(Num_Confirmed = ifelse(is.na(Num_Confirmed), 0, Num_Confirmed))
# 
# head(confirmed)
# fwrite(confirmed, "../covid19/trends/data/confirmed_cases.csv")

# #### OLD remap US states ####
# state_abbr = fread("data/state_abbreviations.csv", sep=',',stringsAsFactors = F)
# 
# us_states = confirmed %>% 
#   filter(Country_Or_Region == "US") %>% 
#   select(Country_Or_Region, Province_Or_State) %>% 
#   distinct() %>%
#   mutate(Province_Or_State_2 = Province_Or_State)
# 
# us_states = separate(us_states, Province_Or_State_2, into = c("County", "State"), sep=', ')
# 
# us_states = us_states %>% left_join(state_abbr, by=c("County"="State_Name"))
# us_states = us_states %>% mutate(State = ifelse(is.na(State), State_Abbr, State)) %>% select(-State_Abbr)
# 
# # manually add in "state" name for specila regions
# us_states = us_states %>% mutate(Special_State = case_when(
#   County %in% c("Diamond Princess", "Grand Princess") ~ "Cruise Ships",
#   County %in% c("United States Virgin Islands", "Virgin Islands")~"Virgin Islands",
#   County == "Puerto Rico"~"PR",
#   County == "Guam"~"GU",
#   County == "District of Columbia"~"Washington DC",
# ))
# 
# us_states = us_states %>% mutate(State = ifelse(is.na(State), Special_State, State)) %>% select(-Special_State)
# 
# x = us_states %>% filter(is.na(State))
# View(x)
# # check for non-mapped US states and issue warning on exclusions
# x = confirmed %>% filter(Country_Or_Region == "US", is.na(Province_Or_State))
# print(paste("Number of unmapped entires in the US is ", nrow(x), ", with ", sum(x$Num_Confirmed), " cases unassigned.", sep=''))

# #### EDA ####
# us_confirmed = confirmed %>% filter(Country_Or_Region == "US")
# x = us_confirmed %>% group_by(State) %>% summarise(Value = sum(Num_Confirmed, na.rm=T))
# 
# global_confirmed = confirmed %>% 
#   group_by(Date) %>%
#   summarise(Num_Confirmed = sum(Num_Confirmed))
# 
# us_confirmed = confirmed %>% 
#   filter(Country_Or_Region == "US") %>%
#   group_by(Date) %>%
#   summarise(Num_Confirmed = sum(Num_Confirmed, na.rm=T))
# 
# 
# plot(global_confirmed$Num_Confirmed)
# plot(us_confirmed$Num_Confirmed)
