# Models où sIgA est le mediateur ----
## Models génériques main analysis avec fonction (pour prédicteur numérique) ----
bdd_final_Y1 <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat) %>%
  na.omit()

bdd_final_Y2 <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat) %>%
  na.omit()

model_crude_Y1 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_12 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

model_crude_Y2 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_24 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

# Crude model, Bayley 1Y
table_fit_crude_GM_Y1_bayley_Y1 <- mediation(model = model_crude_Y1, data = bdd_final_Y1)

# Crude model, Bayley 2Y
table_fit_crude_GM_Y1_bayley_Y2 <- mediation(model = model_crude_Y2, data = bdd_final_Y2)

table_article_GM_Y1  <- list(
  table_article_GM_Y1_bayley_Y1 =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1, name_predictor = "GM at 12 months") %>%
    rename("Est Y1" = est, "CI Y1" = CI, "p-value Y1" = pvalue),

  table_article_GM_Y1_bayley_Y2 =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2, name_predictor = "GM at 12 months") %>%
    rename("Est Y2" = est, "CI Y2" = CI, "p-value Y2" = pvalue))%>%
  reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))

## Models génériques main analysis sans fonction (pour prédicteur categoriel) ----
bdd_final_Y1 <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat) %>%
  na.omit()

bdd_final_Y2 <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat) %>%
  na.omit()

model_crude_Y1 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_12 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

model_crude_Y2 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_24 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

# Crude model, Bayley 1Y
set.seed(1000)
fit_crude_GM_Y1_bayley_Y1 <-
  sem(model_crude_Y1,
      data = bdd_final_Y1,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")

table_fit_crude_GM_Y1_bayley_Y1 <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y1,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

# Crude model, Bayley 2Y
set.seed(1000)
fit_crude_GM_Y1_bayley_Y2 <-
  sem(model_crude_Y2,
      data = bdd_final_Y2,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y2 <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y2,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

table_article_GM_Y1  <- list(
  table_article_GM_Y1_bayley_Y1 =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1, name_predictor = "GM at 12 months") %>%
    rename("Est Y1" = est, "CI Y1" = CI, "p-value Y1" = pvalue),

  table_article_GM_Y1_bayley_Y2 =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2, name_predictor = "GM at 12 months") %>%
    rename("Est Y2" = est, "CI Y2" = CI, "p-value Y2" = pvalue))%>%
  reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))

## Modele gérérique analysis stratifiées avec fonction (pour prédicteur numérique) ----
bdd_final_Y1_exclusive <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Exclusive")

bdd_final_Y2_exclusive <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Exclusive")

bdd_final_Y1_partial <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Partial")

bdd_final_Y2_partial <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Partial")

bdd_final_Y1_none <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "None")

bdd_final_Y2_none <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "None")


model_crude_Y1 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_12 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

model_crude_Y2 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_24 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'


table_fit_crude_GM_Y1_bayley_Y1_exclusive <- mediation(model = model_crude_Y1, data = bdd_final_Y1_exclusive)
table_fit_crude_GM_Y1_bayley_Y2_exclusive <- mediation(model = model_crude_Y2, data = bdd_final_Y2_exclusive)
table_fit_crude_GM_Y1_bayley_Y1_partial <- mediation(model = model_crude_Y1, data = bdd_final_Y1_partial)
table_fit_crude_GM_Y1_bayley_Y2_partial <- mediation(model = model_crude_Y2, data = bdd_final_Y2_partial)
table_fit_crude_GM_Y1_bayley_Y1_none <- mediation(model = model_crude_Y1, data = bdd_final_Y1_none)
table_fit_crude_GM_Y1_bayley_Y2_none <- mediation(model = model_crude_Y2, data = bdd_final_Y2_none)

table_fit_crude_GM_Y1_bayley_Y1_strat  <- list(
  table_fit_crude_GM_Y1_bayley_Y1_exclusive =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_exclusive, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 exclusive" = est, "CI Y1 exclusive" = CI, "p-value Y1 exclusive" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_exclusive =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_exclusive, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 exclusive" = est, "CI Y2 exclusive" = CI, "p-value Y2 exclusive" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y1_partial =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_partial, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 partial" = est, "CI Y1 partial" = CI, "p-value Y1 partial" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_partial =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_partial, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 partial" = est, "CI Y2 partial" = CI, "p-value Y2 partial" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y1_none =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_none, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 none" = est, "CI Y1 none" = CI, "p-value Y1 none" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_none =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_none, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 none" = est, "CI Y2 none" = CI, "p-value Y2 none" = pvalue)) %>%
  reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))



## Modele gérérique analysis stratifiées sans fonction (pour prédicteur catégoriel) ----
bdd_final_Y1_exclusive <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Exclusive")

bdd_final_Y2_exclusive <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Exclusive")

bdd_final_Y1_partial <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Partial")

bdd_final_Y2_partial <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "Partial")

bdd_final_Y1_none <- bdd %>%
  select(ident,
         BAYLEYQ4_3_12,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "None")

bdd_final_Y2_none <- bdd %>%
  select(ident,
         BAYLEYQ4_3_24,
         sIgA_3m_cat_CutOff5,
         predictor,
         Sex, GA, Birthmode_2cat, BFStatus3m) %>%
  na.omit() %>%
  filter(BFStatus3m == "None")


model_crude_Y1 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_12 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'

model_crude_Y2 <- '# simple mediation
sIgA_3m_cat_CutOff5 ~ a*predictor
BAYLEYQ4_3_24 ~ b*sIgA_3m_cat_CutOff5 + c*predictor
# indirect effect
ab := a*b
# total effect
total := c + (a*b)'


# Crude model, Bayley 1Y
set.seed(1000)                         # exlusive
fit_crude_GM_Y1_bayley_Y1_exclusive <-
  sem(model_crude_Y1,
      data = bdd_final_Y1_exclusive,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y1_exclusive <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y1_exclusive,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

set.seed(1000)                            # partial
fit_crude_GM_Y1_bayley_Y1_partial <-
  sem(model_crude_Y1,
      data = bdd_final_Y1_partial,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y1_partial <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y1_partial,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

set.seed(1000)                          # none
fit_crude_GM_Y1_bayley_Y1_none <-
  sem(model_crude_Y1,
      data = bdd_final_Y1_none,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y1_none <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y1_none,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

# Crude model, Bayley 2Y
set.seed(1000)                                  # exlusive
fit_crude_GM_Y1_bayley_Y2_exclusive <-
  sem(model_crude_Y2,
      data = bdd_final_Y2_exclusive,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y2_exclusive <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y2_exclusive,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

set.seed(1000)                                  # partial
fit_crude_GM_Y1_bayley_Y2_partial <-
  sem(model_crude_Y2,
      data = bdd_final_Y2_partial,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y2_partial <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y2_partial,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

set.seed(1000)                              # none
fit_crude_GM_Y1_bayley_Y2_none <-
  sem(model_crude_Y2,
      data = bdd_final_Y2_none,
      ordered = c("predictor", "sIgA_3m_cat_CutOff5"),
      se = "bootstrap",
      bootstrap = 50,
      estimator = "DWLS")
table_fit_crude_GM_Y1_bayley_Y2_none <-
  parameterEstimates(
    fit_crude_GM_Y1_bayley_Y2_none,
    ci = TRUE,
    level = 0.95,
    boot.ci.type = "perc")

table_fit_crude_GM_Y1_bayley_Y1_strat  <- list(
  table_fit_crude_GM_Y1_bayley_Y1_exclusive =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_exclusive, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 exclusive" = est, "CI Y1 exclusive" = CI, "p-value Y1 exclusive" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_exclusive =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_exclusive, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 exclusive" = est, "CI Y2 exclusive" = CI, "p-value Y2 exclusive" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y1_partial =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_partial, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 partial" = est, "CI Y1 partial" = CI, "p-value Y1 partial" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_partial =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_partial, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 partial" = est, "CI Y2 partial" = CI, "p-value Y2 partial" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y1_none =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_none, name_predictor = "GM at 12 months") %>%
    rename("Est Y1 none" = est, "CI Y1 none" = CI, "p-value Y1 none" = pvalue),

  table_fit_crude_GM_Y1_bayley_Y2_none =
    nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_none, name_predictor = "GM at 12 months") %>%
    rename("Est Y2 none" = est, "CI Y2 none" = CI, "p-value Y2 none" = pvalue)) %>%
  reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))


# # Models où sIgA est le prédicteur ----
# ## Models génériques main analysis avec fonction (pour médiateur numérique) ----
# bdd_final_Y1 <- bdd %>%
#   select(ident, 
#          BAYLEYQ4_3_12, 
#          sIgA_3m_cat_CutOff5, 
#          mediator, 
#          Sex, GA, Birthmode_2cat) %>%
#   na.omit()
# 
# bdd_final_Y2 <- bdd %>%
#   select(ident, 
#          BAYLEYQ4_3_24, 
#          sIgA_3m_cat_CutOff5, 
#          mediator, 
#          Sex, GA, Birthmode_2cat) %>%
#   na.omit()
# 
# model_crude_Y1 <- '# simple mediation
# mediator  ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_12 ~ b*mediator + c*sIgA_3m_cat_CutOff5 
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# model_crude_Y2 <- '# simple mediation
# mediator  ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_24 ~ b*mediator + c*sIgA_3m_cat_CutOff5 
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# # Crude model, Bayley 1Y
# table_fit_crude_GM_Y1_bayley_Y1 <- mediation(model = model_crude_Y1, data = bdd_final_Y1)
# 
# # Crude model, Bayley 2Y
# table_fit_crude_GM_Y1_bayley_Y2 <- mediation(model = model_crude_Y2, data = bdd_final_Y2)
# 
# table_article_GM_Y1  <- list(
#   table_article_GM_Y1_bayley_Y1 = 
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1" = est, "CI Y1" = CI, "p-value Y1" = pvalue),
#   
#   table_article_GM_Y1_bayley_Y2 =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2" = est, "CI Y2" = CI, "p-value Y2" = pvalue))%>%
#   reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))
# 
# ## Models génériques main analysis sans fonction (pour médiateur categoriel) ----
# bdd_final_Y1 <- bdd %>%
#   select(ident, 
#          BAYLEYQ4_3_12, 
#          sIgA_3m_cat_CutOff5, 
#          mediator, 
#          Sex, GA, Birthmode_2cat) %>%
#   na.omit()
# 
# bdd_final_Y2 <- bdd %>%
#   select(ident, 
#          BAYLEYQ4_3_24, 
#          sIgA_3m_cat_CutOff5, 
#          mediator, 
#          Sex, GA, Birthmode_2cat) %>%
#   na.omit()
# 
# model_crude_Y1 <- '# simple mediation
# mediator  ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_12 ~ b*mediator + c*sIgA_3m_cat_CutOff5 
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# model_crude_Y2 <- '# simple mediation
# mediator  ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_24 ~ b*mediator + c*sIgA_3m_cat_CutOff5 
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# # Crude model, Bayley 1Y
# set.seed(1000)
# fit_crude_GM_Y1_bayley_Y1 <- 
#   sem(model_crude_Y1, 
#       data = bdd_final_Y1,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap", 
#       bootstrap = 50,
#       estimator = "DWLS")
# 
# table_fit_crude_GM_Y1_bayley_Y1 <- 
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y1, 
#     ci = TRUE, 
#     level = 0.95, 
#     boot.ci.type = "perc")
# 
# # Crude model, Bayley 2Y
# set.seed(1000)
# fit_crude_GM_Y1_bayley_Y2 <- 
#   sem(model_crude_Y2, 
#       data = bdd_final_Y2,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap", 
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y2 <- 
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y2, 
#     ci = TRUE, 
#     level = 0.95, 
#     boot.ci.type = "perc")
# 
# table_article_GM_Y1  <- list(
#   table_article_GM_Y1_bayley_Y1 = 
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1" = est, "CI Y1" = CI, "p-value Y1" = pvalue),
#   
#   table_article_GM_Y1_bayley_Y2 =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2" = est, "CI Y2" = CI, "p-value Y2" = pvalue))%>%
#   reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))
# 
# ## Modele gérérique analysis stratifiées avec fonction (pour médiateur numérique) ----
# bdd_final_Y1_exclusive <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Exclusive")
# 
# bdd_final_Y2_exclusive <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Exclusive")
# 
# bdd_final_Y1_partial <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Partial")
# 
# bdd_final_Y2_partial <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Partial")
# 
# bdd_final_Y1_none <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "None")
# 
# bdd_final_Y2_none <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "None")
# 
# 
# model_crude_Y1 <- '# simple mediation
# mediator ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_12 ~ b*mediator + c*sIgA_3m_cat_CutOff5
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# model_crude_Y2 <- '# simple mediation
# mediator ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_24 ~ b*mediator + c*sIgA_3m_cat_CutOff5
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# 
# table_fit_crude_GM_Y1_bayley_Y1_exclusive <- mediation(model = model_crude_Y1, data = bdd_final_Y1_exclusive)
# table_fit_crude_GM_Y1_bayley_Y2_exclusive <- mediation(model = model_crude_Y2, data = bdd_final_Y2_exclusive)
# table_fit_crude_GM_Y1_bayley_Y1_partial <- mediation(model = model_crude_Y1, data = bdd_final_Y1_partial)
# table_fit_crude_GM_Y1_bayley_Y2_partial <- mediation(model = model_crude_Y2, data = bdd_final_Y2_partial)
# table_fit_crude_GM_Y1_bayley_Y1_none <- mediation(model = model_crude_Y1, data = bdd_final_Y1_none)
# table_fit_crude_GM_Y1_bayley_Y2_none <- mediation(model = model_crude_Y2, data = bdd_final_Y2_none)
# 
# table_fit_crude_GM_Y1_bayley_Y1_strat  <- list(
#   table_fit_crude_GM_Y1_bayley_Y1_exclusive =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_exclusive, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 exclusive" = est, "CI Y1 exclusive" = CI, "p-value Y1 exclusive" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_exclusive =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_exclusive, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 exclusive" = est, "CI Y2 exclusive" = CI, "p-value Y2 exclusive" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y1_partial =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_partial, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 partial" = est, "CI Y1 partial" = CI, "p-value Y1 partial" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_partial =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_partial, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 partial" = est, "CI Y2 partial" = CI, "p-value Y2 partial" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y1_none =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_none, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 none" = est, "CI Y1 none" = CI, "p-value Y1 none" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_none =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_none, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 none" = est, "CI Y2 none" = CI, "p-value Y2 none" = pvalue)) %>%
#   reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))
# 
# 
# 
# ## Modele gérérique analysis stratifiées sans fonction (pour médiateur catégoriel) ----
# bdd_final_Y1_exclusive <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Exclusive")
# 
# bdd_final_Y2_exclusive <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Exclusive")
# 
# bdd_final_Y1_partial <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Partial")
# 
# bdd_final_Y2_partial <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "Partial")
# 
# bdd_final_Y1_none <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_12,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "None")
# 
# bdd_final_Y2_none <- bdd %>%
#   select(ident,
#          BAYLEYQ4_3_24,
#          sIgA_3m_cat_CutOff5,
#          mediator,
#          Sex, GA, Birthmode_2cat, BFStatus3m) %>%
#   na.omit() %>%
#   filter(BFStatus3m == "None")
# 
# 
# model_crude_Y1 <- '# simple mediation
# mediator ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_12 ~ b*mediator + c*sIgA_3m_cat_CutOff5
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# model_crude_Y2 <- '# simple mediation
# mediator ~ a*sIgA_3m_cat_CutOff5
# BAYLEYQ4_3_24 ~ b*mediator + c*sIgA_3m_cat_CutOff5
# # indirect effect
# ab := a*b
# # total effect
# total := c + (a*b)'
# 
# 
# # Crude model, Bayley 1Y
# set.seed(1000)                         # exlusive
# fit_crude_GM_Y1_bayley_Y1_exclusive <-
#   sem(model_crude_Y1,
#       data = bdd_final_Y1_exclusive,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y1_exclusive <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y1_exclusive,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# set.seed(1000)                            # partial
# fit_crude_GM_Y1_bayley_Y1_partial <-
#   sem(model_crude_Y1,
#       data = bdd_final_Y1_partial,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y1_partial <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y1_partial,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# set.seed(1000)                          # none
# fit_crude_GM_Y1_bayley_Y1_none <-
#   sem(model_crude_Y1,
#       data = bdd_final_Y1_none,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y1_none <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y1_none,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# # Crude model, Bayley 2Y
# set.seed(1000)                                  # exlusive
# fit_crude_GM_Y1_bayley_Y2_exclusive <-
#   sem(model_crude_Y2,
#       data = bdd_final_Y2_exclusive,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y2_exclusive <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y2_exclusive,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# set.seed(1000)                                  # partial
# fit_crude_GM_Y1_bayley_Y2_partial <-
#   sem(model_crude_Y2,
#       data = bdd_final_Y2_partial,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y2_partial <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y2_partial,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# set.seed(1000)                              # none
# fit_crude_GM_Y1_bayley_Y2_none <-
#   sem(model_crude_Y2,
#       data = bdd_final_Y2_none,
#       ordered = c("mediator", "sIgA_3m_cat_CutOff5"),
#       se = "bootstrap",
#       bootstrap = 50,
#       estimator = "DWLS")
# table_fit_crude_GM_Y1_bayley_Y2_none <-
#   parameterEstimates(
#     fit_crude_GM_Y1_bayley_Y2_none,
#     ci = TRUE,
#     level = 0.95,
#     boot.ci.type = "perc")
# 
# table_fit_crude_GM_Y1_bayley_Y1_strat  <- list(
#   table_fit_crude_GM_Y1_bayley_Y1_exclusive =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_exclusive, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 exclusive" = est, "CI Y1 exclusive" = CI, "p-value Y1 exclusive" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_exclusive =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_exclusive, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 exclusive" = est, "CI Y2 exclusive" = CI, "p-value Y2 exclusive" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y1_partial =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_partial, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 partial" = est, "CI Y1 partial" = CI, "p-value Y1 partial" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_partial =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_partial, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 partial" = est, "CI Y2 partial" = CI, "p-value Y2 partial" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y1_none =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y1_none, name_mediator = "GM at 12 months") %>%
#     rename("Est Y1 none" = est, "CI Y1 none" = CI, "p-value Y1 none" = pvalue),
#   
#   table_fit_crude_GM_Y1_bayley_Y2_none =
#     nettoyage(fit = table_fit_crude_GM_Y1_bayley_Y2_none, name_mediator = "GM at 12 months") %>%
#     rename("Est Y2 none" = est, "CI Y2 none" = CI, "p-value Y2 none" = pvalue)) %>%
#   reduce(~left_join(.x, .y, by = c("Predictor", "Mediator", "Path")))