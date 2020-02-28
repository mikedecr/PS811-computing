# ----------------------------------------------------
#   prep data for regression lesson
#   cleaning the Jacobson House data and merging with ideal points
# ----------------------------------------------------

library("here")
library("magrittr")
library("tidyverse")
library("broom")

# ---- read data -----------------------

# gary jacobson House elections data
# (statedistrict-year level)
gj_raw <- 
  readxl::read_excel(
    here("data", "HR4616.xls"),
    guess_max = 16000
  ) %>%
  print()

# NOMINATE data
nom_raw <- 
  read_csv(here("data", "HSall_members.csv")) %>% 
  print()


# ---- clean data for merge -----------------------

# every statedist is 3 or 4 chars
gj %>% count(nchar(statedist))
gj %>% filter(str_detect(as.character(statedist), "00"))

states_tab <- 
  tibble(
    state_name = state.name,
    state_abbrev = state.abb,
    state_num_alpha = rank(state_name)
  ) %>%
  print()

# keep select variables
# state is the first 1 or 2 chars from stcd
# district is always last 2 chars
gj <- gj_raw %>%
  mutate(
    state_num_alpha = 
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
    state_num_alpha, 
    district_code,
    incumbency = inc,
    party_win = pwin,
    dem_share_house = dv,
    dem_share_pres = dpres
  ) %>%
  left_join(states_tab) %>%
  print()


nom <- nom_raw %>%
  filter(chamber == "House") %>%
  filter(party_code %in% c(100, 200)) %>%
  mutate(
    congress_year = 1787 + (congress * 2),
    election_year = congress_year - 1
  ) %>%
  filter(election_year %in% c(gj$election_year)) %>%
  select(
    congress_year, election_year, 
    party_code,
    state_icpsr,
    district_code,
    state_abbrev,
    bioname,
    nominate_dim1
  ) %>%
  print()


merged <- 
  inner_join(
    nom, gj
    # ,
    # by = c("election_year" = "election_year", "state_icpsr" = "state_fips")
  ) %>%
  print()


merged %>% 
  group_by(election_year) %>%
  summarize(
    nstate = n_distinct(state_abbrev)
  ) %>%
  arrange(desc(nstate))


trimmed <- merged %>%
  mutate(
    party = case_when(
      party_code == 100 ~ "Democrat",
      party_code == 200 ~ "Republican"
    ),
    republican = as.numeric(party == "Republican")
  ) %>%
  mutate_at(
    vars(contains("dem_share")),
    .funs = ~ 100 - .
  ) %>%
  rename_at(
    vars(contains("dem_share")),
    .funs = ~ str_replace(., "dem_share", "rep_share")
  ) %>%
  select(
    election_year, 
    state_abbrev,
    district_code,
    bioname, 
    nominate_dim1,
    contains("rep_share"),
    party,
    republican
  ) %>%
  filter(
    is.na(nominate_dim1) == FALSE &
    (is.na(rep_share_pres) == FALSE | is.na(rep_share_house) == FALSE)
  ) %>%
  arrange(desc(election_year)) %>%
  print()


ggplot(trimmed) +
  aes(x = rep_share_pres, y = nominate_dim1, color = party) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = party), color = "black") +
  facet_wrap(~ election_year)

write_csv(trimmed, here("data", "house-elections.csv"))
