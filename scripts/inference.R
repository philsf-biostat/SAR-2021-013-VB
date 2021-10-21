# setup -------------------------------------------------------------------
# library(tableone)
# library(gt)
# library(gtsummary)
# library(infer)

# tables ------------------------------------------------------------------

# baseline characterísticas
tab_desc <- analytical %>%
  # select
  select(grupo, idade, sexo, peso, asa, opioides, pem_t0, pim_t0) %>%
  tbl_summary(
    by = grupo
  ) %>%
  # include study N
  add_overall() %>%
  # pretty format categorical variables
  bold_labels() %>%
  # bring home the bacon!
  add_p(
    # use Fisher test (defaults to chi-square)
    test = all_categorical() ~ "fisher.test",
    # use 3 digits in pvalue
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  # modify_caption(caption = "**Tabela 1** Características demográficas") %>%
  modify_header(label ~ "**Características dos participantes**") %>%
  # bold significant p values
  bold_p()

# Template Cohen's D table (obs: does NOT compute p)
tab_inf <- analytical %>%
  mutate(grupo = relevel(grupo, "BQL")) %>%
  # select
  select(
    grupo,
    intensa_t1,
    intensa_t4,
    pem,
    pim,
    idade,
    sexo,
    peso,
    ) %>%
  tbl_summary(
    by = grupo,
    type = pim ~ "continuous",
    include = c(intensa_t1, intensa_t4, pem, pim),
  ) %>%
  add_difference(
    test = all_dichotomous() ~ "prop.test",
    # adj.vars = c(sexo, idade, peso),
  ) %>%
  bold_p() %>%
  modify_header(label ~ "**Desfechos**") %>%
  # modify_header(estimate ~ '**d**') %>%
  bold_labels()
