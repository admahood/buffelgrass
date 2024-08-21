# plot jsdms
library(gghmsc)
library(tidyverse)
load("data/hmsc/hmsc_probit_no_peci_group.Rda")


lut_gensp<- m1$Y |>
  colnames() |>
  str_replace_all("_", " ") |>
  str_to_sentence() |>
  str_replace_all("Moss fern", 'Mosses & Ferns')
names(lut_gensp)  <- m1$Y |>
  colnames() 

gghmsc::gghmsc_beta(m1)

# omega ====================
gghmsc::gghmsc_omega(m1, hc.method = 'single', lut_gensp = lut_gensp) +
  geom_segment(aes(x=5.5, xend = 22.5, y=7.5, yend = 7.5)) +
  geom_segment(aes(x=6.5, xend = 22.5, y=6.5, yend = 6.5))+
  geom_segment(aes(x=6.5, xend = 6.5, y=0.5, yend = 6.5)) +
  geom_segment(aes(x=5.5, xend = 5.5, y=0.5, yend = 7.5))
ggsave(filename = "figs/omega.png", height = 8, width = 8, bg='white')
