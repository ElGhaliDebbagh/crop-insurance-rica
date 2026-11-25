# ============================================================
# RICA — Script 2 : Propensity Score
# ============================================================
# Objectif : estimer l'effet causal d'un choc climatique en t
# sur la décision de souscrire une assurance-récolte en t+1
#
# Structure :
#   1. Construction du panel individuel avec IDNUM
#   2. Construction des variables
#   3. Comparaison brute (sans contrôle)
#   4. Estimation du propensity score
#   5. Qualité du score
#   6. Régression finale (Modèle 1 et Modèle 2)
#   7. Tableau récapitulatif
#
# Prérequis : panel_script1.R lancé (rica_data en mémoire)
# ============================================================

library(tidyverse)
library(fixest)
library(knitr)
library(kableExtra)

# ============================================================
# 1. CONSTRUCTION DU PANEL INDIVIDUEL AVEC IDNUM
# ============================================================
# IDNUM confirmé comme identifiant stable par Agreste
# (communication personnelle, Agreste-Info)

cat("=== Construction du panel individuel avec IDNUM ===\n")

panel_idnum_raw <- purrr::map_dfr(rica_data, function(d) {
  vars_ok <- intersect(
    c("IDNUM", "OTEXE", "CDEXE", "NREG", "REGIO",  # REGIO pour harmonisation
      "ZALTI", "ZDEFA", "ZENVI",                     # zones géographiques
      "EXTR2", "ASSRE", "INDAS", "SBVCLIM",          # assurance et sinistres
      "TAUDET_NR", "AUTOF",                          # variables financières
      "SAUIR", "EAUIR",                              # irrigation
      "annee"),
    names(d)
  )
  d %>% select(all_of(vars_ok))
}) %>%
  filter(annee >= 2002) %>%
  mutate(
    NREG  = as.character(NREG),
    REGIO = as.character(REGIO)
  )

# Harmonisation région REGIO → NREG pour les années 2000-2014
# Nécessaire car NREG n'existe qu'à partir de 2015
# Sans cette étape : 86 816 NA sur NREG pour les années 2002-2014
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

panel_ps <- panel_idnum_raw %>%
  
  # Exclusion DOM (Guadeloupe, Martinique, Réunion)
  # Agriculture structurellement différente + pas de données météo
  filter(!NREG %in% c("95", "96", "98")) %>%
  
  # Exclusion ZALTI = 9 (code transitoire 2016-2017, non documenté)
  filter(ZALTI %in% c(1, 2, 3)) %>%
  
  # Exclusion des 2 NA sur ZDEFA
  filter(!is.na(ZDEFA)) %>%
  
  mutate(
    # Variable dépendante : souscription assurance-récolte
    assure = as.integer(ASSRE > 0),
    
    # Variable de choc climatique
    # Combine indemnité assurance (INDAS) ET aide calamités (SBVCLIM)
    # pour capturer tous les sinistres reconnus, qu'on soit assuré ou non
    choc = as.integer(INDAS > 0 | SBVCLIM > 0),
    
    # Ratio d'endettement (TAUDET_NR est en %)
    ratio_dette = TAUDET_NR / 100,
    
    # Transformation IHS de l'autofinancement
    # AUTOF peut être négatif — log classique inapplicable
    # IHS définie sur tout R, se comporte comme log pour grandes valeurs
    # Référence : Bellemare & Wichman (2020)
    autof_ihs = log(AUTOF + sqrt(AUTOF^2 + 1))
  )

# Construction du lead individuel (t+1)
# Possible grâce à IDNUM — vrai lead par exploitation
panel_ps <- panel_ps %>%
  arrange(IDNUM, annee) %>%
  group_by(IDNUM) %>%
  mutate(
    assure_t1 = lead(assure, 1),  # assurance l'année suivante
    n_annees  = n()
  ) %>%
  ungroup() %>%
  filter(!is.na(ZDEFA)) %>%      # 2 NA résiduels sur ZDEFA
  filter(!is.na(assure_t1)) %>%  # exclut la dernière année (pas de t+1)
  filter(n_annees >= 2)          # exclut les IDNUM présents 1 seule année

cat("Panel PS final :", nrow(panel_ps), "observations\n")
cat("IDNUM distincts :", n_distinct(panel_ps$IDNUM), "\n\n")

# Distribution du choc
cat("Distribution du choc :\n")
panel_ps %>%
  count(choc) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Statistiques sur les trois groupes d'exploitations
cat("\nStatut d'assurance par exploitation :\n")
panel_ps %>%
  group_by(IDNUM) %>%
  summarise(
    n_annees  = n(),
    n_assure  = sum(assure)
  ) %>%
  mutate(statut = case_when(
    n_assure == 0        ~ "Jamais assurée",
    n_assure == n_annees ~ "Toujours assurée",
    TRUE                 ~ "Change de statut"
  )) %>%
  count(statut) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ============================================================
# 3. COMPARAISON BRUTE (sans contrôle)
# ============================================================

cat("\n=== Comparaison brute (sans contrôle) ===\n")

panel_ps %>%
  group_by(choc) %>%
  summarise(
    n              = n(),
    taux_assure_t  = round(weighted.mean(assure,    EXTR2, na.rm=TRUE), 3),
    taux_assure_t1 = round(weighted.mean(assure_t1, EXTR2, na.rm=TRUE), 3)
  ) %>%
  mutate(choc = ifelse(choc == 1, "Sinistré (choc=1)", "Non sinistré (choc=0)")) %>%
  print()

# ============================================================
# 4. ESTIMATION DU PROPENSITY SCORE
# ============================================================
# On estime P(choc = 1 | caractéristiques) via un probit
#
# Variables choisies — proxies du risque climatique :
#   OTEXE  : type de production (céréaliers plus exposés)
#   CDEXE  : taille économique
#   NREG   : région (exposition climatique structurelle)
#   ZALTI  : zone altimétrique (exposition gel en montagne)
#   ZDEFA  : zone défavorisée (zones à risque plus élevé)
#   ZENVI  : zone à contraintes environnementales
#   SAUIR  : surface irriguée (exposition sécheresse)
#   EAUIR  : charges d'eau d'irrigation (intensité risque hydrique)
#   ratio_dette : situation financière (suggestion Villeneuve)
#   factor(annee) : effets fixes année (chocs climatiques nationaux)
#
# Variables exclues du PS :
#   autof_ihs : potentiellement endogène au choc
#   (une exploitation sinistrée peut avoir un autofinancement réduit)

cat("\n=== Estimation du propensity score ===\n")

ps_model <- glm(
  choc ~ factor(OTEXE) +
    factor(CDEXE) +
    factor(NREG)  +
    factor(ZALTI) +
    factor(ZDEFA) +
    factor(ZENVI) +
    SAUIR +
    EAUIR +
    ratio_dette +
    factor(annee),
  data   = panel_ps,
  family = binomial(link = "probit"),
  weights = EXTR2
)

# Score prédit pour chaque observation
panel_ps <- panel_ps %>%
  mutate(ps = predict(ps_model, type = "response"))

cat("Score estimé pour", nrow(panel_ps), "observations.\n\n")

# ============================================================
# 5. QUALITÉ DU PROPENSITY SCORE
# ============================================================

cat("=== Qualité du propensity score ===\n")

# Distribution du score par groupe
cat("Distribution du score par groupe :\n")
panel_ps %>%
  group_by(choc) %>%
  summarise(
    n         = n(),
    score_moy = round(mean(ps), 3),
    score_min = round(min(ps),  3),
    score_max = round(max(ps),  3),
    score_sd  = round(sd(ps),   3)
  ) %>%
  print()

# Pseudo-R² de McFadden
null_model <- glm(
  choc ~ 1,
  data   = panel_ps,
  family = binomial(link = "probit"),
  weights = EXTR2
)

pseudo_r2 <- 1 - (logLik(ps_model) / logLik(null_model))
cat("\nPseudo-R² de McFadden :", round(pseudo_r2, 3), "\n")
cat("  < 0.10 : score peu précis\n")
cat("  0.10-0.20 : acceptable\n")
cat("  > 0.20 : bon\n\n")

# ============================================================
# 6. RÉGRESSION FINALE
# ============================================================
# On estime l'effet du choc en t sur assure_{t+1}
# en contrôlant par le propensity score
#
# Modèle 1 : ps + contrôles observables + effets fixes année
#   → contrôle les différences observables entre exploitations
#
# Modèle 2 : ps + effets fixes IDNUM + effets fixes année
#   → contrôle aussi les différences non observables stables
#   → compare chaque exploitation avec elle-même dans le temps
#   → identifié sur la variation within (exploitations qui changent)

cat("\n=== Régression finale ===\n")

# Modèle 1 — contrôle par le score + variables observables
modele_ps <- glm(
  assure_t1 ~ choc + ps +
    factor(OTEXE) + factor(CDEXE) +
    factor(NREG)  + factor(ZALTI) +
    factor(annee),
  data   = panel_ps,
  family = binomial(link = "probit"),
  weights = EXTR2
)

coef_ps <- coef(summary(modele_ps))["choc", ]
cat("Modèle 1 — contrôle par le score :\n")
cat(" Coefficient choc :", round(coef_ps["Estimate"],  4), "\n")
cat(" p-value          :", round(coef_ps["Pr(>|z|)"],  4), "\n\n")

# Modèle 2 — score + effets fixes IDNUM (via feglm)
# feglm utilisé car glm() plante en mémoire avec ~20 000 dummies IDNUM
modele_ps_fe <- feglm(
  assure_t1 ~ choc + ps | IDNUM + annee,
  data     = panel_ps,
  family   = binomial("probit"),
  weights  = ~EXTR2,
  glm.iter = 100
)

coef_fe <- coef(modele_ps_fe)["choc"]
pval_fe <- pvalue(modele_ps_fe)["choc"]
cat("Modèle 2 — score + effets fixes IDNUM :\n")
cat(" Coefficient choc :", round(coef_fe, 4), "\n")
cat(" p-value          :", round(pval_fe, 4), "\n\n")
cat("NOTE feglm :\n")
print(summary(modele_ps_fe))

# ============================================================
# 7. TABLEAU RÉCAPITULATIF
# ============================================================

cat("\n=== TABLEAU RÉCAPITULATIF ===\n")

coef_brut <- coef(summary(glm(
  assure_t1 ~ choc, data = panel_ps,
  family = binomial("probit"), weights = EXTR2
)))["choc", ]

resultats <- tibble(
  Modèle = c(
    "Brut (sans contrôle)",
    "Modèle 1 — contrôle par le score",
    "Modèle 2 — score + effets fixes IDNUM et année"
  ),
  Coefficient = round(c(
    coef_brut["Estimate"],
    coef_ps["Estimate"],
    coef_fe
  ), 4),
  `p-value` = round(c(
    coef_brut["Pr(>|z|)"],
    coef_ps["Pr(>|z|)"],
    pval_fe
  ), 4)
) %>%
  mutate(
    Significatif = case_when(
      `p-value` < 0.001 ~ "***",
      `p-value` < 0.01  ~ "**",
      `p-value` < 0.05  ~ "*",
      TRUE              ~ "n.s."
    )
  )

print(resultats)

resultats %>%
  kable(caption = paste(
    "Effet du choc climatique en t sur la souscription en t+1.",
    "| Pseudo-R² PS :", round(pseudo_r2, 3)
  )) %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_propensity_score.png")

cat("\nTableau sauvegardé : tab_propensity_score.png\n")
cat("\nInterprétation :\n")
cat(" Coefficient > 0 *** → rational learning\n")
cat(" Coefficient < 0 *** → gambler's fallacy\n")
cat(" Non significatif    → pas d'effet détectable\n")