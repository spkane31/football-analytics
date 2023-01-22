# garbage time
library(tidyverse)
library(gt)
library(dplyr)

df <- nflreadr::load_pbp() %>%
  filter(rush == 1 | pass == 1, !is.na(epa)) %>%
  mutate(
    epa = qb_epa,
    garbage = ifelse(
       wp > .95 | wp < .05,
      1,
      0
    )
  ) %>%
  group_by(id, garbage) %>%
  summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    tot_epa = sum(epa),
    avg_epa = mean(epa),
    n = n()
  ) %>%
  arrange(id) %>%
  tidyr::pivot_wider(id_cols = c(id, name, team), names_from = garbage, values_from = c(tot_epa, avg_epa, n)) %>%
  filter(n_0 > 25, n_1 > 5, tot_epa_1 > 5, tot_epa_0 > 5) %>%
  dplyr::mutate(pct = tot_epa_1 / (tot_epa_0 + tot_epa_1), tot = tot_epa_0 + tot_epa_1) %>%
  dplyr::arrange(-tot_epa_1) %>%
  ungroup() %>%
  left_join(nflreadr::load_players() %>% select(gsis_id, headshot), by = c("id" = "gsis_id")) %>%
  dplyr::mutate(rank = 1 : n(), pct = pct * 100) %>%
  select(rank, headshot, name, team, tot, garbage_epa = tot_epa_1, pct) %>%
  head(10)

df

t <- df %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(headshot)),
    fn = function(x) web_image(url = paste0(x))
  ) %>%
  text_transform(
    locations = cells_body(c(team)),
    fn = function(x) web_image(url = paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
  ) %>%
  cols_label(
    rank = "", "headshot" = "", name = "Player", team = "", tot = "Total EPA", garbage_epa = "Garbage EPA", pct = "Percent"
  ) %>%
  gtExtras::gt_color_rows(columns = garbage_epa, palette = "ggsci::blue_material" ) %>%
  gt::tab_header(title = paste("2022 Garbage Time Kings"), subtitle = "Garbage time: win probability > 95% or < 95%") %>%
  gt::tab_source_note(gt::md(glue::glue('{lubridate::today()} | @SeanKane942'))) %>%
  tab_options(
    table.font.size = px(20L),
    data_row.padding = px(1)
  ) %>%
  gt::tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(1)
    )
  ) %>%
  gtExtras::gt_theme_nytimes() %>%
  gtExtras::fmt_symbol_first(pct, symbol = "%", decimals = 1) %>%
  gtExtras::fmt_symbol_first(c(tot, garbage_epa), decimals = 1)


t %>%
  gtsave("/Users/seankane/github.com/football-analytics/img/garbage-amt.png", expand = 20)
