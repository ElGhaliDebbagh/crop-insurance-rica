# ============================================================
# RICA — Script A : Choc calamité (SBVCLIM > 0)
# ============================================================
# Variable de choc : choc_calamite = 1 si SBVCLIM > 0
# (aide fonds calamités agricoles reçue)
#
# Avantage : variable exogène — ne suppose pas d'être assuré
# Un non-assuré sinistré apparaît dans SBVCLIM mais pas dans INDAS
# Pas de problème d'endogénéité contrairement à INDAS
#
# Structure :
#   1. Construction du panel individuel avec IDNUM
#   2. Construction des variables
#   3. Comparaison brute
#   4. Estimation du propensity score
#   5. Qualité du score
#   6. Modèles 1 et 2 (PS seul, PS + effets fixes IDNUM)
#   7. Trois régressions A, B, C avec lags
#   8. Tableaux récapitulatifs
#
# Prérequis : panel_script1.R lancé (rica_data et table_regions en mémoire)
# ============================================================

library(tidyverse)
library(fixest)
library(knitr)
library(kableExtra)
library(marginaleffects)

# ============================================================
# 1. CONSTRUCTION DU PANEL INDIVIDUEL AVEC IDNUM
# ============================================================

cat("=== Construction du panel individuel avec IDNUM ===\n")

panel_idnum_raw <- purrr::map_dfr(rica_data, function(d) {
  vars_ok <- intersect(
    c("IDNUM", "OTEXE", "CDEXE", "NREG", "REGIO",
      "ZALTI", "ZDEFA", "ZENVI",
      "EXTR2", "ASSRE", "INDAS", "SBVCLIM",
      "TAUDET_NR", "AUTOF",
      "SAUIR", "EAUIR", "annee"),
    names(d)
  )
  d %>% select(all_of(vars_ok))
}) %>%
  filter(annee >= 2002) %>%
  mutate(
    NREG  = as.character(NREG),
    REGIO = as.character(REGIO)
  )

# Harmonisation région REGIO → NREG pour 2002-2014
panel_idnum_raw <- panel_idnum_raw %>%
  left_join(table_regions, by = c("REGIO" = "REGIO_old")) %>%
  mutate(
    NREG = case_when(
      annee <= 2014 & !is.na(NREG_new) ~ NREG_new,
      TRUE ~ NREG
    )
  ) %>%
  select(-any_of(c("REGIO", "NREG_new")))

cat("NA sur NREG après harmonisation :", sum(is.na(panel_idnum_raw$NREG)), "\n")
cat("Observations :", nrow(panel_idnum_raw), "\n")
cat("IDNUM distincts :", n_distinct(panel_idnum_raw$IDNUM), "\n\n")

# ============================================================
# 2. CONSTRUCTION DES VARIABLES
# ============================================================

cat("=== Construction des variables ===\n")

panel_A <- panel_idnum_raw %>%
  filter(!NREG %in% c("95", "96", "98")) %>%
  filter(ZALTI %in% c(1, 2, 3)) %>%
  filter(!is.na(ZDEFA)) %>%
  mutate(
    # Variable dépendante
    assure = as.integer(ASSRE > 0),
    
    # Variable de choc — SBVCLIM uniquement
    # Avantage : exogène, ne suppose pas d'être assuré
    # Limite : rate les sinistres non reconnus administrativement
    choc = as.integer(SBVCLIM > 0),
    
    # Variables financières
    ratio_dette = TAUDET_NR / 100,
    autof_ihs   = log(AUTOF + sqrt(AUTOF^2 + 1))
  )

# Lead individuel (t+1)
panel_A <- panel_A %>%
  arrange(IDNUM, annee) %>%
  group_by(IDNUM) %>%
  mutate(
    assure_t1 = lead(assure, 1),
    n_annees  = n()
  ) %>%
  ungroup() %>%
  filter(!is.na(assure_t1)) %>%
  filter(n_annees >= 2)

cat("Panel final :", nrow(panel_A), "observations\n")
cat("IDNUM distincts :", n_distinct(panel_A$IDNUM), "\n\n")

# Distribution du choc
cat("Distribution du choc (SBVCLIM > 0) :\n")
panel_A %>%
  count(choc) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Statut d'assurance
cat("\nStatut d'assurance par exploitation :\n")
panel_A %>%
  group_by(IDNUM) %>%
  summarise(n_annees = n(), n_assure = sum(assure)) %>%
  mutate(statut = case_when(
    n_assure == 0        ~ "Jamais assurée",
    n_assure == n_annees ~ "Toujours assurée",
    TRUE                 ~ "Change de statut"
  )) %>%
  count(statut) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ============================================================
# 3. COMPARAISON BRUTE
# ============================================================

cat("\n=== Comparaison brute (sans contrôle) ===\n")

panel_A %>%
  group_by(choc) %>%
  summarise(
    n              = n(),
    taux_assure_t  = round(weighted.mean(assure,    EXTR2, na.rm=TRUE), 3),
    taux_assure_t1 = round(weighted.mean(assure_t1, EXTR2, na.rm=TRUE), 3)
  ) %>%
  mutate(choc = ifelse(choc == 1,
                       "Sinistré calamité (SBVCLIM>0)",
                       "Non sinistré (SBVCLIM=0)")) %>%
  print()

# ============================================================
# 4. ESTIMATION DU PROPENSITY SCORE
# ============================================================
# On estime P(SBVCLIM > 0 | caractéristiques)
# Variables : proxies du risque climatique structurel

cat("\n=== Estimation du propensity score ===\n")

ps_model_A <- glm(
  choc ~ factor(OTEXE) + factor(CDEXE) +
    factor(NREG)  + factor(ZALTI) +
    factor(ZDEFA) + factor(ZENVI) +
    SAUIR + EAUIR + ratio_dette +
    factor(annee),
  data   = panel_A,
  family = binomial(link = "probit"),
  weights = EXTR2
)

panel_A <- panel_A %>%
  mutate(ps = predict(ps_model_A, type = "response"))

cat("Score estimé pour", nrow(panel_A), "observations.\n\n")

# ============================================================
# 5. QUALITÉ DU SCORE
# ============================================================

cat("=== Qualité du propensity score ===\n")

panel_A %>%
  group_by(choc) %>%
  summarise(
    n         = n(),
    score_moy = round(mean(ps), 3),
    score_min = round(min(ps),  3),
    score_max = round(max(ps),  3),
    score_sd  = round(sd(ps),   3)
  ) %>%
  print()

null_model_A <- glm(choc ~ 1, data = panel_A,
                    family = binomial("probit"), weights = EXTR2)
pseudo_r2_A <- 1 - (logLik(ps_model_A) / logLik(null_model_A))
cat("\nPseudo-R² de McFadden :", round(pseudo_r2_A, 3), "\n\n")

cat("SD du score :", round(sd(panel_A$ps), 4), "\n")
cat("Moyenne du score :", round(mean(panel_A$ps), 4), "\n\n")

# ============================================================
# 6. MODÈLES 1 ET 2 — EFFET DU CHOC SUR ASSURE_T1
# ============================================================

cat("\n=== Modèle 1 — contrôle par le score ===\n")

modele_1_A <- glm(
  assure_t1 ~ choc + ps +
    factor(OTEXE) + factor(CDEXE) +
    factor(NREG)  + factor(ZALTI) +
    factor(annee),
  data   = panel_A,
  family = binomial("probit"),
  weights = EXTR2
)

coef_1_A <- coef(summary(modele_1_A))["choc", ]
cat("Coefficient choc :", round(coef_1_A["Estimate"],  4), "\n")
cat("p-value          :", round(coef_1_A["Pr(>|z|)"],  4), "\n\n")

cat("=== Modèle 2 — score + effets fixes IDNUM ===\n")

modele_2_A <- feglm(
  assure_t1 ~ choc + ps | IDNUM + annee,
  data     = panel_A,
  family   = binomial("probit"),
  weights  = ~EXTR2,
  glm.iter = 100
)

print(summary(modele_2_A))

# ============================================================
# 7. TROIS RÉGRESSIONS A, B, C AVEC LAGS
# ============================================================

cat("\n=== Construction des lags ===\n")

panel_lags_A <- panel_A %>%
  arrange(IDNUM, annee) %>%
  group_by(IDNUM) %>%
  mutate(
    choc_lag1 = lag(choc, 1),
    choc_lag2 = lag(choc, 2),
    choc_lag3 = lag(choc, 3)
  ) %>%
  ungroup() %>%
  filter(!is.na(choc_lag1))

cat("Panel avec lags :", nrow(panel_lags_A), "observations\n\n")

# Modèle A — PS seul
cat("--- Modèle A : PS seul ---\n")
modele_A_ps <- feglm(
  assure_t1 ~ ps | IDNUM + annee,
  data = panel_lags_A, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_A_ps))
ame_A_ps <- avg_slopes(modele_A_ps, variables = "ps", vcov = FALSE)
cat("AME ps :", round(ame_A_ps$estimate, 4), "\n\n")

# Modèle B — Vécu seul (lag1 à lag3)
cat("--- Modèle B : Vécu seul (lag1 à lag3) ---\n")
modele_B_A <- feglm(
  assure_t1 ~ choc_lag1 + choc_lag2 + choc_lag3 | IDNUM + annee,
  data = panel_lags_A, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_B_A))
ame_B_A <- avg_slopes(modele_B_A,
                      variables = c("choc_lag1", "choc_lag2", "choc_lag3"),
                      vcov = FALSE)
cat("AME Modèle B :\n")
print(ame_B_A %>% select(term, estimate))

# Modèle C — PS + Vécu
cat("\n--- Modèle C : PS + Vécu ---\n")
modele_C_A <- feglm(
  assure_t1 ~ ps + choc_lag1 + choc_lag2 + choc_lag3 | IDNUM + annee,
  data = panel_lags_A, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_C_A))
ame_C_A <- avg_slopes(modele_C_A,
                      variables = c("ps", "choc_lag1",
                                    "choc_lag2", "choc_lag3"),
                      vcov = FALSE)
cat("AME Modèle C :\n")
print(ame_C_A %>% select(term, estimate))

# ============================================================
# 8. TABLEAUX RÉCAPITULATIFS
# ============================================================

cat("\n=== TABLEAU RÉCAPITULATIF — Modèles 1 et 2 ===\n")

coef_brut_A <- coef(summary(glm(
  assure_t1 ~ choc, data = panel_A,
  family = binomial("probit"), weights = EXTR2
)))["choc", ]

resultats_A <- tibble(
  Modèle = c(
    "Brut (sans contrôle)",
    "Modèle 1 — contrôle par le score",
    "Modèle 2 — score + effets fixes IDNUM et année"
  ),
  Coefficient = round(c(
    coef_brut_A["Estimate"],
    coef_1_A["Estimate"],
    coef(modele_2_A)["choc"]
  ), 4),
  `p-value` = round(c(
    coef_brut_A["Pr(>|z|)"],
    coef_1_A["Pr(>|z|)"],
    pvalue(modele_2_A)["choc"]
  ), 4)
) %>%
  mutate(Significatif = case_when(
    `p-value` < 0.001 ~ "***",
    `p-value` < 0.01  ~ "**",
    `p-value` < 0.05  ~ "*",
    TRUE              ~ "n.s."
  ))

print(resultats_A)

resultats_A %>%
  kable(caption = paste(
    "Choc calamité (SBVCLIM > 0) — effet sur la souscription en t+1.",
    "| Pseudo-R² PS :", round(pseudo_r2_A, 3)
  )) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_choc_calamite.png")

cat("Tableau sauvegardé : tab_choc_calamite.png\n")

# Régressions séparées par lag — SBVCLIM
cat("\n=== RÉGRESSIONS SÉPARÉES PAR LAG (SBVCLIM) ===\n")

panel_lags_A <- panel_A %>%
  arrange(IDNUM, annee) %>%
  group_by(IDNUM) %>%
  mutate(
    choc_lag1 = lag(choc, 1),
    choc_lag2 = lag(choc, 2),
    choc_lag3 = lag(choc, 3),
    choc_lag4 = lag(choc, 4),
    choc_lag5 = lag(choc, 5),
    choc_lag6 = lag(choc, 6),
    choc_lag7 = lag(choc, 7),
    choc_lag8 = lag(choc, 8)
  ) %>%
  ungroup() %>%
  filter(!is.na(choc_lag1))

resultats_sep_A <- purrr::map_dfr(1:8, function(k) {
  var_lag <- paste0("choc_lag", k)
  data_k  <- panel_lags_A %>% filter(!is.na(.data[[var_lag]]))
  formule <- as.formula(paste0("assure_t1 ~ ps + ", var_lag, " | IDNUM + annee"))
  modele_k <- tryCatch(
    feglm(formule, data = data_k, family = binomial("probit"),
          weights = ~EXTR2, glm.iter = 500),
    error = function(e) NULL
  )
  if (is.null(modele_k)) return(NULL)
  ame_k <- tryCatch(
    avg_slopes(modele_k, variables = var_lag, vcov = FALSE),
    error = function(e) NULL
  )
  tibble(
    lag      = k,
    n_obs    = nobs(modele_k),
    coef_lag = round(coef(modele_k)[var_lag], 4),
    pval_lag = round(pvalue(modele_k)[var_lag], 4),
    ame_lag  = if (!is.null(ame_k)) round(ame_k$estimate, 4) else NA
  )
})

print(resultats_sep_A)