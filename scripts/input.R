# setup -------------------------------------------------------------------
# library(data.table)
library(tidyverse)
library(readxl)
# library(lubridate)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
# data.raw <- tibble(id=gl(2, 10), group = gl(2, 10), outcome = rnorm(20))
data.raw <- read_excel("dataset/Tabela de dados analíticos.xlsx")

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  separate(`PEM T0/PIM T0`, c("PEM T0", "PIM T0"), convert = TRUE) %>%
  separate(`PEM T4/PIM T4`, c("PEM T4", "PIM T4"), convert = TRUE) %>%
  separate(`PEM T24/PIM T24`, c("PEM T24", "PIM T24"), convert = TRUE) %>%
  janitor::clean_names() %>%
  mutate(id = str_replace(id, " ", "")) %>%
  distinct()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    grupo = factor(grupo), grupo = relevel(grupo, "Controle"),
    intensa_t1 = dor_t1 > 3,
    intensa_t4 = dor_t4 > 3,
    intensa_t24 = dor_t24 > 3,
    pem = pem_t4 - pem_t0,
    pim = pim_t4 - pim_t0,
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    grupo = "Grupo",
    # outcome = "Study outcome",
    intensa_t1 = "Dor moderada ou intensa (1h)",
    intensa_t4 = "Dor moderada ou intensa (4h)",
    intensa_t24 = "Dor moderada ou intensa (24h)",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    # id,
    # grupo,
    -nome,
    -prontuario,
  )

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(nrow(analytical)) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
