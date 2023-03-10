# Load packages ----------------------------------------------------------------

packages <- 
  c(
    "fabricatr",
    "tidyverse",
    "here",
    "dataReporter",
    "haven"
  )

pacman::p_load(
  packages,
  character.only = TRUE,
  dep = TRUE
)

# Set seet for reproducible randomization --------------------------------------

set.seed(1)

# Create child-level data ------------------------------------------------------

children <- 
  fabricate(
    village_id = add_level(
      N = 10,
      n_mothers = runif(N, min = 10, max = 20) %>% round(0)
    ),
    mother_index = add_level(
      N = n_mothers,
      mother_age = runif(N, min = 17, max = 50) %>% round(0),
      mother_yob = 2022 - mother_age,
      mother_educ = rnorm(N, mean = 9, sd = 3.5) %>% round(0) %>% abs,
      mother_income = (rpois(N, lambda = 2) * 100) + ((runif(N, min = 0, max = 9)  %>% round(0)) * 10),
      n_children = rpois(N, lambda = 2),
      flag_m_year = rbinom(N, 1, prob = .05),
      flag_m_age = rbinom(N, 1, prob = .1),
      flag_n_age = runif(N, min = -5, max = 5) %>% round(0),
      flag_m_children = rbinom(N, 1, prob = .05),
      flag_n_children = runif(N, min = -1, max = 2) %>% round(0),
      flag_i_high = rbinom(N, 1, prob = .05),
      flag_i_zero = ifelse(!flag_i_high, rbinom(N, 1, prob = .2), 0),
      flag_i_neg = ifelse(!flag_i_high & !flag_i_zero, rbinom(N, 1, prob = .1), 0)
    ),
    child_index = add_level(
      N = n_children,
      age = runif(N, min = 0, max = 13) %>% round(0),
      sex = runif(N, min = 1, max = 6) %>% round(0) %>% 
        factor(
          levels = 1:6,
          labels = c("Male", "Female", "M", "F", "male", "female")
        ) %>%
        as.character,
      in_school = ifelse(
        age >= 6, 
        rbinom(N, 1, prob = .9), 
        rbinom(N, 1, prob = .2)
      ),
      grade = ifelse(in_school, age - 5, NA) %>% abs(),
      flag_c_year = rbinom(N, 1, prob = .05),
      flag_c_grade = rbinom(N, 1, prob = .02),
      flag_n_grade = rpois(N, lambda = 3),
      flag_c_grade_dk = rbinom(N, 1, prob = .05),
      yob = ifelse(flag_c_year, 9999, 2022 - age)
    )
  ) %>%
  mutate(
    mother_yob = ifelse(flag_m_year, -99, mother_yob),
    mother_age = ifelse(flag_m_age, mother_age + flag_n_age, mother_age),
    n_children = ifelse(flag_m_children, n_children + flag_n_children, n_children),
    grade = ifelse(flag_c_grade, grade + flag_n_grade, grade),
    grade = ifelse(flag_c_grade_dk, -99, grade),
    grade = na_if(grade, 0),
    mother_income = 
      case_when(
        flag_i_high == 1 ~ mother_income * 20,
        flag_i_zero == 1 ~ 0,
        flag_i_neg == 1 ~ -88,
        TRUE ~ mother_income
      )
  ) %>%
  select(
    -n_mothers,
    -starts_with("flag")
  ) 


  
# Create mother-level tidy data ------------------------------------------------

mother_tidy <-
  children %>%
  select(-c(child_index:yob)) %>%
  unique %>%
  group_by(village_id) %>%
  transmute(
    village_id,
    mother_id = row_number() %>% 
      as.character() %>%
      str_pad(2, "0", side  = "left") %>%
      paste0(village_id, .),
    yob = mother_yob,
    age = mother_age,
    educ = mother_educ,
    mother_income,
    n_children,
    mother_index
  ) %>%
  ungroup

mother_tidy %>%
  select(-mother_index) %>%
  write_csv(
    here(
      "data",
      "tidy",
      "mother.csv"
    )
  )

mother_tidy %>%
  select(-mother_index) %>%
  write_rds(
    here(
      "data",
      "tidy",
      "mother.rds"
    )
  )

mother_tidy %>%
  select(-mother_index) %>%
  write_dta(
    here(
      "data",
      "tidy",
      "mother.dta"
    )
  )

# Create child-level tidy data -------------------------------------------------

child_tidy <-
  children %>%
  left_join(
    mother_tidy %>% 
      select(mother_index, mother_id),
    by = "mother_index"
  ) %>%
  group_by(village_id, mother_id) %>%
  transmute(
    village_id,
    mother_id,
    child_no = row_number() %>% 
      as.character(),
    child_id = child_no %>%
      str_pad(2, "0", side  = "left") %>%
      paste0(mother_id, .),
    yob,
    age,
    sex,
    grade,
    in_school
  ) %>%
  ungroup

child_tidy %>%
  select(-child_no) %>%
  write_csv(
    here(
      "data",
      "tidy",
      "child.csv"
    )
  )

child_tidy %>%
  select(-child_no) %>%
  write_rds(
    here(
      "data",
      "tidy",
      "child.rds"
    )
  )

child_tidy %>%
  select(-child_no) %>%
  write_dta(
    here(
      "data",
      "tidy",
      "child.dta"
    )
  )

# Create mother-level wide data ------------------------------------------------


mother_raw <-
  child_tidy %>%
  select(-c(child_id, village_id)) %>%
  pivot_wider(
    values_from = c("age", "sex", "in_school", "yob", "grade"),
    names_from = "child_no"
  ) %>%
  left_join(
    mother_tidy,
    .,
    by = "mother_id"
  )

dups <- 
  mother_raw %>%
  slice_sample(n = 3)

missing_dup <- 
  dups %>%
  slice(1) %>%
  select(village_id:n_children)

all_dups <-
  dups %>%
  slice(-1)

mother_w_dups <-
  mother_raw %>%
  mutate(
    across(
      mother_id,
      ~ ifelse(
        . == "1003",
        "0103",
        .
      )
    )
  ) %>%
  bind_rows(missing_dup) %>%
  bind_rows(all_dups) %>%
  arrange(mother_id)

mother_w_dups %>%
  select(-mother_index) %>%
  write_csv(
    here(
      "data",
      "raw",
      "mother_survey.csv"
    )
  )

mother_w_dups %>%
  select(-mother_index) %>%
  write_dta(
    here(
      "data",
      "raw",
      "mother_survey.dta"
    )
  )



# Create example of long data --------------------------------------------------

mother_long <-
  mother_tidy %>%
  pivot_longer(
    cols = c("village_id", "age", "educ", "n_children"),
    values_transform = ~ as.character(.)
  )

mother_long %>%
  select(-mother_index) %>%
  write_csv(
    here(
      "data",
      "raw",
      "mother_long.csv"
    )
  )

# Create child-level analysis data ---------------------------------------------

child_final <-
  mother_tidy %>%
  transmute(
    mother_id,
    mother_age = age,
    mother_educ = educ,
    n_siblings = n_children - 1
  ) %>%
  left_join(
    child_tidy,
    .,
    by = "mother_id"
  ) %>%
  select(-child_no)

child_final %>%
  write_csv(
    here(
      "data",
      "analysis",
      "child.csv"
    )
  )

child_final %>%
  write_rds(
    here(
      "data",
      "analysis",
      "child.rds"
    )
  )

child_final %>%
  write_dta(
    here(
      "data",
      "analysis",
      "child.dta"
    )
  )
