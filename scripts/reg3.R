# ============================================================
# RICA — Script 5 : Analyse de l'effet de mémoire
# ============================================================
# Objectif : tester l'effet des chocs passés sur la souscription
# en distinguant risque objectif (PS) et vécu (chocs passés)
#
# Deux approches :
#   APPROCHE 1 — Lags simultanés (tous ensemble, échantillon commun)
#     Avantage : chaque lag contrôlé pour les autres
#     Inconvénient : perte d'observations (exige historique long)
#
#   APPROCHE 2 — Régressions séparées par lag (échantillon propre)
#     Avantage : maximise les observations disponibles
#     Inconvénient : ne contrôle pas les lags entre eux
#
# Prérequis : panel_script1.R + Propensity_score.R lancés (panel_ps en mémoire)
# ============================================================

library(tidyverse)
library(fixest)
library(knitr)
library(kableExtra)
library(marginaleffects)

# ============================================================
# 1. CONSTRUCTION DES LAGS (jusqu'à lag 10)
# ============================================================

cat("=== Construction des lags ===\n")

panel_lags <- panel_ps %>%
  arrange(IDNUM, annee) %>%
  group_by(IDNUM) %>%
  mutate(
    choc_lag1  = lag(choc, 1),
    choc_lag2  = lag(choc, 2),
    choc_lag3  = lag(choc, 3),
    choc_lag4  = lag(choc, 4),
    choc_lag5  = lag(choc, 5),
    choc_lag6  = lag(choc, 6),
    choc_lag7  = lag(choc, 7),
    choc_lag8  = lag(choc, 8),
    choc_lag9  = lag(choc, 9),
    choc_lag10 = lag(choc, 10),
    
    # Cumul de chocs sur 3 et 5 ans
    n_chocs_3ans = choc_lag1 + choc_lag2 + choc_lag3,
    n_chocs_5ans = choc_lag1 + choc_lag2 + choc_lag3 +
      choc_lag4 + choc_lag5
  ) %>%
  ungroup() %>%
  filter(!is.na(choc_lag1))

cat("Panel avec lags :", nrow(panel_lags), "observations\n")
cat("IDNUM distincts  :", n_distinct(panel_lags$IDNUM), "\n\n")

# Attrition par lag
cat("Observations disponibles par lag maximum :\n")
tibble(
  lag_max = 1:10,
  n_obs   = sapply(1:10, function(k) {
    sum(!is.na(panel_lags[[paste0("choc_lag", k)]]))
  })
) %>% print()

# ============================================================
# 2. APPROCHE 1 — LAGS SIMULTANÉS (lag1 à lag10)
# ============================================================
# Tous les lags dans la même régression
# Chaque coefficient contrôlé pour les autres
# Échantillon : exploitations avec 10+ ans d'historique

cat("\n=== APPROCHE 1 : Lags simultanés ===\n")

# Modèle A1 — PS seul
cat("\n--- Modèle A1 : PS seul ---\n")
modele_A1 <- feglm(
  assure_t1 ~ ps | IDNUM + annee,
  data = panel_lags, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_A1))
ame_A1 <- avg_slopes(modele_A1, variables = "ps", vcov = FALSE)
cat("AME ps :", round(ame_A1$estimate, 4), "\n")

# Modèle B1 — Vécu seul (lag1 à lag10)
cat("\n--- Modèle B1 : Vécu seul (lag1 à lag10) ---\n")
modele_B1 <- feglm(
  assure_t1 ~ choc_lag1 + choc_lag2 + choc_lag3 +
    choc_lag4 + choc_lag5 + choc_lag6 +
    choc_lag7 + choc_lag8 + choc_lag9 +
    choc_lag10 | IDNUM + annee,
  data = panel_lags, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_B1))
vars_lags <- paste0("choc_lag", 1:10)
ame_B1 <- avg_slopes(modele_B1, variables = vars_lags, vcov = FALSE)
cat("AME Modèle B1 :\n")
print(ame_B1 %>% select(term, estimate))

# Modèle C1 — PS + Vécu (lag1 à lag10)
cat("\n--- Modèle C1 : PS + Vécu (lag1 à lag10) ---\n")
modele_C1 <- feglm(
  assure_t1 ~ ps + choc_lag1 + choc_lag2 + choc_lag3 +
    choc_lag4 + choc_lag5 + choc_lag6 +
    choc_lag7 + choc_lag8 + choc_lag9 +
    choc_lag10 | IDNUM + annee,
  data = panel_lags, family = binomial("probit"),
  weights = ~EXTR2, glm.iter = 500
)
print(summary(modele_C1))
vars_C1 <- c("ps", paste0("choc_lag", 1:10))
ame_C1 <- avg_slopes(modele_C1, variables = vars_C1, vcov = FALSE)
cat("AME Modèle C1 :\n")
print(ame_C1 %>% select(term, estimate))

# ============================================================
# 3. APPROCHE 2 — RÉGRESSIONS SÉPARÉES PAR LAG
# ============================================================
# Une régression par lag, chacune sur son propre échantillon
# Maximise les observations disponibles
# Ne contrôle pas les lags entre eux

cat("\n=== APPROCHE 2 : Régressions séparées par lag ===\n")

resultats_sep <- purrr::map_dfr(1:10, function(k) {
  
  var_lag <- paste0("choc_lag", k)
  cat("  Lag", k, "...\n")
  
  # Échantillon propre à ce lag
  data_k <- panel_lags %>%
    filter(!is.na(.data[[var_lag]]))
  
  # Régression : PS + lag_k seul
  formule <- as.formula(
    paste0("assure_t1 ~ ps + ", var_lag, " | IDNUM + annee")
  )
  
  modele_k <- tryCatch(
    feglm(formule, data = data_k,
          family = binomial("probit"),
          weights = ~EXTR2, glm.iter = 500),
    error = function(e) NULL
  )
  
  if (is.null(modele_k)) return(NULL)
  
  # AME du lag
  ame_k <- tryCatch(
    avg_slopes(modele_k, variables = var_lag, vcov = FALSE),
    error = function(e) NULL
  )
  
  tibble(
    lag      = k,
    n_obs    = nobs(modele_k),
    coef_lag = round(coef(modele_k)[var_lag], 4),
    pval_lag = round(pvalue(modele_k)[var_lag], 4),
    ame_lag  = if (!is.null(ame_k)) round(ame_k$estimate, 4) else NA,
    coef_ps  = round(coef(modele_k)["ps"], 4)
  )
})

cat("\nRésultats Approche 2 :\n")
print(resultats_sep)

# ============================================================
# 4. MODÈLES CUMUL DE CHOCS
# ============================================================

cat("\n=== Cumul de chocs ===\n")

# Cumul sur 3 ans
modele_cumul3 <- feglm(
  assure_t1 ~ ps + n_chocs_3ans | IDNUM + annee,
  data = panel_lags %>% filter(!is.na(n_chocs_3ans)),
  family = binomial("probit"), weights = ~EXTR2, glm.iter = 500
)
cat("Cumul 3 ans :\n")
print(summary(modele_cumul3))
ame_c3 <- avg_slopes(modele_cumul3,
                     variables = c("ps", "n_chocs_3ans"),
                     vcov = FALSE)
cat("AME :\n") ; print(ame_c3 %>% select(term, estimate))

# Cumul sur 5 ans
modele_cumul5 <- feglm(
  assure_t1 ~ ps + n_chocs_5ans | IDNUM + annee,
  data = panel_lags %>% filter(!is.na(n_chocs_5ans)),
  family = binomial("probit"), weights = ~EXTR2, glm.iter = 500
)
cat("\nCumul 5 ans :\n")
print(summary(modele_cumul5))
ame_c5 <- avg_slopes(modele_cumul5,
                     variables = c("ps", "n_chocs_5ans"),
                     vcov = FALSE)
cat("AME :\n") ; print(ame_c5 %>% select(term, estimate))

# ============================================================
# 5. TABLEAU COMPARATIF DES DEUX APPROCHES
# ============================================================

cat("\n=== TABLEAU COMPARATIF DES DEUX APPROCHES ===\n")

# Coefficients Approche 1
coef_B1 <- coef(modele_B1)
coef_C1 <- coef(modele_C1)

comparatif <- tibble(
  Lag = 1:10,
  
  # Approche 1 — lags simultanés
  `A1 : Coef (simultané)` = round(coef_B1[paste0("choc_lag", 1:10)], 4),
  
  # Approche 2 — régressions séparées
  `A2 : Coef (séparé)` = resultats_sep$coef_lag,
  `A2 : N obs`         = resultats_sep$n_obs,
  `A2 : AME (pp)`      = resultats_sep$ame_lag
)

print(comparatif)

comparatif %>%
  kable(
    caption = paste(
      "Comparaison des deux approches — effet des chocs passés",
      "sur la souscription en t+1.",
      "Approche 1 : tous les lags simultanés (échantillon commun).",
      "Approche 2 : une régression par lag (échantillon propre)."
    )
  ) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_comparatif_approches.png")

cat("\nTableau sauvegardé : tab_comparatif_approches.png\n")