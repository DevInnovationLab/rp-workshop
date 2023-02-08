# Load packages ----------------------------------------------------------------

packages <- 
  c(
    "fabricatr",
    "tidyverse",
    "here"
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
      mother_age = runif(N, min = 20, max = 50) %>% round(0),
      mother_educ = rnorm(N, mean = 9, sd = 3.5) %>% round(0),
      n_children = rpois(N, lambda = 2)
    ),
    child_index = add_level(
      N = n_children,
      age = runif(N, min = 2, max = 15) %>% round(0),
      sex = rbinom(N, 1, prob = .5) %>% 
        factor(
          levels = 0:1,
          labels = c("Male", "Female")
        ),
      in_school = ifelse(
        age >= 6, 
        rbinom(N, 1, prob = .9), 
        rbinom(N, 1, prob = .4)
      )
    )
  ) %>%
  select(-n_mothers)

# Create mother-level tidy data ------------------------------------------------

mother_tidy <-
  children %>%
  select(-c(child_index:in_school)) %>%
  unique %>%
  group_by(village_id) %>%
  transmute(
    village_id,
    mother_id = row_number() %>% 
      as.character() %>%
      str_pad(2, "0", side  = "left") %>%
      paste0(village_id, .),
    age = mother_age,
    educ = mother_educ,
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
    age,
    sex,
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

# Create mother-level wide data ------------------------------------------------

mother_level <-
  child_tidy %>%
  select(-child_id) %>%
  pivot_wider(
    values_from = c("age", "sex", "in_school"),
    names_from = "child_no"
  ) %>%
  left_join(
    mother_tidy,
    .,
    by = "mother_id"
  )

mother_level %>%
  select(-mother_index) %>%
  write_csv(
    here(
      "data",
      "raw",
      "mother_survey.csv"
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
