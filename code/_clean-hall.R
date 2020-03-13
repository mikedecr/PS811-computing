# ----------------------------------------------------
#   data cleaning
#   andrew hall primaries (APSR) data
#   for reshaping lecture
# ----------------------------------------------------

# ---- jacobson election data -----------------------

db_raw <- 
  here("data", "broockman_female-candidates.dta") %>%
  haven::read_dta() %>%
  print()


hall_raw <- 
  here("data", "hall_house-primaries.dta") %>%
  haven::read_dta() %>%
  print()


hall <- hall_raw %>%
  transmute(
    state_abb = state, 
    district_number = dist,
    election_year = year,
    republican = as.numeric(dem == 0),
    primary_abs_distance = absdist,
    extremist_margin = rv,
    extremist_win = treat,
    gen_vote_share = dv,
    gen_win = dv_win,
    primary_winner_HS = winner_score,
    primary_loser_HS = case_when(
      republican == 1 & extremist_win == 1 ~ 
        (primary_winner_HS - primary_abs_distance),
      republican == 1 & extremist_win == 0 ~ 
        (primary_winner_HS + primary_abs_distance),
      republican == 0 & extremist_win == 1 ~ 
        (primary_winner_HS + primary_abs_distance),
      republican == 0 & extremist_win == 0 ~ 
        (primary_winner_HS - primary_abs_distance),
    ),
    extremist_HS = case_when(
      extremist_win == 1 ~ primary_winner_HS,
      extremist_win == 0 ~ primary_loser_HS
    ),
    moderate_HS = case_when(
      extremist_win == 1 ~ primary_loser_HS,
      extremist_win == 0 ~ primary_winner_HS
    ),
    primary_winner_share = prim_share,
    extremist_primary_vote = vote_P0,
    moderate_primary_vote = vote_P1,
    primary_winner_vote = case_when(
      extremist_win == 1 ~ extremist_primary_vote,
      extremist_win == 0 ~ moderate_primary_vote
    ),
    primary_loser_vote = case_when(
      extremist_win == 1 ~ moderate_primary_vote,
      extremist_win == 0 ~ extremist_primary_vote
    )
  ) %>%
  print()


ggplot(hall) +
  aes(primary_loser_HS, primary_abs_distance, color = as.factor(republican)) +
  facet_wrap(~ extremist_win) +
  geom_point()


# how we know we have the primary ideology variables correct
# if Republican, extremists are RIGHT of moderates
ggplot(hall) +
  aes(y = primary_abs_distance, x = primary_winner_HS - primary_loser_HS,
      color = as.factor(extremist_win)
  ) +
  geom_point() +
  facet_grid(str_glue("extremist_win = {extremist_win}") ~ str_glue("republican = {republican}"))


# if we've reverse engineered the HS scores correctly...
ggplot(hall) +
  aes(y = primary_abs_distance, x = abs(extremist_HS - moderate_HS)) +
  geom_point()



# how we know we have the primary vote variables correct
hall %>%
  ggplot() +
  aes(
    y = extremist_margin, 
    x = (extremist_primary_vote - moderate_primary_vote) / 
        (extremist_primary_vote + moderate_primary_vote), 
    color = as.factor(extremist_win)
  ) +
  labs(x = "Extremist Advantage, Top 2") +
  geom_point()


# ---- reshaping long? -----------------------

# we can re-calculate "extremist win" and "extremist margin"
select(hall, starts_with("moderate"))
select(hall, starts_with("extremist"))

# drop redundant things
# calculate top-two vote shares?
hall_long <- hall %>%
  select(
    -primary_abs_distance, 
    -extremist_margin, -extremist_win,
    -primary_winner_share, 
    -starts_with("extremist"), -starts_with("moderate")
  ) %>%
  nest(winners = contains("primary_winner")) %>%
  nest(losers = contains("primary_loser")) %>%
  gather(key = winner, value = data, winners, losers) %>%
  mutate(
    cleaned = map(
      .x = data,
      .f = ~ .x %>%
        rename_at(
          vars(contains("primary_winner")),
          ~ str_replace(., "primary_winner_", "")
        ) %>%
        rename_at(
          vars(contains("primary_loser")),
          ~ str_replace(., "primary_loser_", "")
        )
    )
  ) %>%
  unnest(cleaned) %>%
  rename(
    donor_score = HS,
    vote_share = vote
  ) %>%
  select(-data, -winner) %>%
  arrange(state_abb, district_number, election_year, republican) %>%
  mutate(cand_id = as.character(str_glue("cand_{row_number()}"))) %>%
  group_by(state_abb, district_number, election_year, republican) %>%
  mutate(
    contestant_id = as.character(str_glue("contestant_{row_number()}"))
  ) %>%
  select(cand_id, state_abb, district_number, election_year, republican, contestant_id, everything()) %>%
  ungroup() %>%
  print()


hall_long %>%
  select(
    -c(gen_vote_share, gen_win), 
    # everything(), 
    # gen_vote_share, gen_win
  ) %>%
  rename(
    ideology_score = donor_score,
    primary_vote = vote_share
  ) %>%
  write_csv(here("data", "house-primaries-top-two.csv"))

# tasks: 
# make abs_distance, keep if big distance
# make primary margin, keep if small margin
# calculate margin within district
# calculate candidate distance within district
# make leads of DV?

hall_long %>%
  select(-cand_id) %>%
  pivot_wider(
    names_from = contestant_id,
    values_from = c(donor_score, vote_share)
  ) %>%
  mutate()
  
  