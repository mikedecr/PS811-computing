# ----------------------------------------------------
#   PS 811: statistical computing
#   "Lecture" 5: Real world data nightmares
#   reading, cleaning, joining data
# ----------------------------------------------------



# ---- packages -----------------------

library("here")
library("tidyverse")
# readxl





# ---- reading data -----------------------

# raw nominate data
nom_raw <- 
  read_csv(here("data", "HSall_members.csv")) %>%
  print()

# gary jacobson House data
# (statedistrict-year level)
gj_raw <- 
  readxl::read_excel(
    here("data", "HR4616.xls"),
    col_types = c(
      "year" = "numeric",
      "stcd" = "numeric",
      "inc" = "numeric",
      "pwin" = "numeric",
      "dv" = "numeric",
      "dvp" = "numeric",
      "fr" = "numeric",
      "po1" = "numeric",
      "po2" = "numeric",
      "redist" = "numeric",
      "dexp" = "numeric",
      "rexp" = "numeric",
      "dpres" = "numeric",
      "switchb" = "numeric",
      "switcha" = "numeric"
    ),
    guess_max = 16000
  ) %>%
  print()

# haven::read_dta
# fixed-width *fwf read_fwf
# {rio} rio::import()





# ---- cleaning -----------------------

state.name
state.abb

states_tab <- 
  tibble(
    state_name = state.name,
    state_abbrev = state.abb,
    state_number = rank(state_name)
  ) %>%
  arrange(state_abbrev) %>% 
  mutate(
    n_obs = n(),
    row_n = row_number()
  ) %>%
  print()



# every stcd is either 3 or 4 chars long
count(gj_raw, stcd) %>% 
  print(n = nrow(.))

count(gj_raw, nchar(stcd))


str_sub("abcdefg", 3L, 7L)

# parse_number() ≠ as.numeric()
c("a", "b", "c") %>%
  as.factor() %>%
  as.numeric()




# recode GJ
gj <- gj_raw %>%
  mutate(
    state_number = 
      case_when(
        nchar(stcd) == 3 ~ str_sub(stcd, 1L, 1L),
        nchar(stcd) == 4 ~ str_sub(stcd, 1L, 2L)
      ) %>%
      parse_number(),
    district_code = 
      str_sub(stcd, -2L, -1L) %>%
      parse_number()
  ) %>%
  select(
    election_year = year,
    statedist = stcd,
    state_number, 
    district_code, 
    incumbency = inc,
    party_win = pwin,
    dem_share_house = dv,
    dem_share_pres = dpres
  ) %>%
  left_join(x = ., y = states_tab) %>%
  print()

# recode Nominate data (VoteView)
nom <- nom_raw %>%
  filter(chamber == "House") %>%
  mutate(
    congress_year = 1787 + (congress * 2),
    election_year = congress_year - 1
  ) %>%
  select(
    congress_year, election_year,
    party_code, 
    state_icpsr, state_abbrev, district_code,
    bioname,
    nominate_dim1
  ) %>%
  print()







# ---- merge (join) -----------------------

merged <- 
  inner_join(
    nom, gj,
    by = c("election_year", "state_abbrev", "district_code")
  ) %>%
  print()

merged %>%
  group_by(election_year) %>%
  summarize(
    nstate = n_distinct(state_abbrev)
  ) %>%
  arrange(desc(nstate))



# common goal: "skeleton dataframe" (crossing()!)




# ---- trim and write data -----------------------

trimmed <- merged %>%
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican"
    ),
    republican = as.numeric(party == "Republican")
  ) %>%
  mutate_at(
    .vars = vars(dem_share_house, dem_share_pres),
    .funs = function(x) 100 - x
  ) %>%
  rename_at(
    .vars = vars(starts_with("dem_share")),
    .funs = ~ str_replace(., "dem_share", "rep_share")
  ) %>%
  select(
    election_year, state_abbrev, district_code,
    bioname,
    nominate_dim1,
    starts_with("rep_share"),
    party,
    republican
  ) %>%
  print()


ggplot(trimmed) +
  aes(x = rep_share_pres, y = nominate_dim1) +
  geom_point()



# csv, rds, rdata...
write_rds(
  trimmed,
  here("data", "house-elections.rds")
)


# ---- other helpful tools -----------------------

trimmed %>%
  group_by(state_abbrev) %>%
  summarize(
    state_mean = mean(nominate_dim1, na.rm = TRUE)
  ) %>%
  ggplot() +
  aes(x = state_mean, y = fct_reorder(as.factor(state_abbrev), "WI", "MO")) +
  geom_point()

# have we talked about factors? stringsAsFactors
# check syllabus links
# lubridate for dates
# tibble(), tribble()
# n(), row_number()

