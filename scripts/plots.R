# setup -------------------------------------------------------------------
# library(ggplot2)
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

# Theme setting (less is more)
theme_set(
  theme_classic()
)
theme_update(
  legend.position = "top"
)

# plots -------------------------------------------------------------------

gg <- ggplot(analytical, aes(outcome, fill = group)) +
  geom_density( alpha = .8) +
  # scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal) +
  labs()

data.raw %>%
  select(id, grupo, dor_t1, dor_t4, dor_t24) %>%
  pivot_longer(-c(id, grupo), names_to = "t", values_to = "dor") %>%
  mutate(t = parse_number(t)) %>%
  ggplot(aes(t, dor, group = id, color = grupo)) +
  geom_line() +
  # scale_color_brewer(palette = ff.pal) +
  ylim(c(0,10)) +
  xlim(c(0,24)) +
  theme(legend.title = element_blank()) +
  labs(x = "T (horas)", y = "Dor (0-10)")

data.raw %>%
  select(id, grupo, starts_with(c("pem", "pim"))) %>%
  pivot_longer(-c(id, grupo),) %>%
  separate(name, into = c("var", "t")) %>%
  mutate(t = parse_number(t)) %>%
  ggplot(aes(t, value, group = id, color = grupo)) +
  geom_line() +
  xlim(c(0,24)) +
  theme(legend.title = element_blank()) +
  facet_wrap(~ var, 2, scales = "free_y")
