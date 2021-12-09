census
================
Congyang Xie
12/6/2021

US census data

Data layout description:
<https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2020/sc-est2020-alldata6.pdf>

``` r
library(tidyverse)
```

``` r
census_df <- 
  read_csv("data/SC-EST2020-ALLDATA5.csv") %>% 
  janitor::clean_names() %>% 
  select(-sumlev, -region, -division, -state)  %>% 
  rename(state = name,
         gender = sex) %>%
  filter(origin != 0,
         gender != 0) %>% 
  mutate(
    origin = as.integer(origin),
    race = as.integer(race),
    new_race = 
      case_when( 
        origin == 2 ~ "Hispanic/Latino",
        origin == 1 & race == 1 ~ "European-American/White",
        origin == 1 & race == 2 ~ "African-American/Black",
        origin == 1 & race == 3 ~ "Native American/Alaskan",
        origin == 1 & race %in% c(4, 5) ~ "Asian/Pacific Islander"
      
    )
  ) %>% 
  select(-race, -origin) %>%
  rename(race = new_race) %>% 
  mutate(
    age = as.numeric(round(replace_na(age, mean(age, na.rm = TRUE)))),
    age_bin = AMR::age_groups(age, c(15, 25, 35, 55, 85))) %>% 
  select(-age, - census2010pop, -estimatesbase2010) %>% 
  pivot_longer(
    popestimate2010:popestimate2020,
    names_to = "year",
    names_prefix ="popestimate",
    values_to = "population"
  ) %>% 
  group_by(state, race, gender, age_bin, year) %>%
  summarize(population = sum(population)) %>% 
  mutate(
    gender = recode(gender, `1` = "Male", `2` = "Female"),
    state = state.abb[match(state, state.name)]
  ) %>% 
  drop_na()
```

    ## Rows: 197370 Columns: 22

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): SUMLEV, STATE, NAME
    ## dbl (19): REGION, DIVISION, SEX, ORIGIN, RACE, AGE, CENSUS2010POP, ESTIMATES...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## `summarise()` has grouped output by 'state', 'race', 'gender', 'age_bin'. You can override using the `.groups` argument.
