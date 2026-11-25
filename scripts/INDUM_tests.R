# ============================================================
# RICA — Script 3 : Vérification de la continuité d'IDNUM
# ============================================================
# Objectif : tester si IDNUM est un identifiant permanent
# permettant de suivre les mêmes exploitations d'une année à l'autre
#
# Structure :
#   1. Chargement des bases brutes (data)
#   2. Test 1 — Taux de chevauchement entre années consécutives
#   3. Test 2 — Stabilité des caractéristiques pour les IDNUM communs
#   4. Test 3 — Présence sur plusieurs années (distribution)
#   5. Test 4 — Gaps dans les trajectoires
#   6. Test 5 — Comparaison variables financières pour les IDNUM communs
#   7. Conclusion et tableau récapitulatif
# ============================================================

library(tidyverse)
library(knitr)
library(kableExtra)

# Prérequis : avoir lancé panel_script1.R qui charge la liste data
# Si data n'est pas en mémoire, relancer panel_script1.R d'abord
if (!exists("data")) {
  stop("La liste 'data' n'est pas en mémoire. Lance panel_script1.R d'abord.")
}

# ============================================================
# 1. EXTRACTION DES IDNUM PAR ANNÉE
# ============================================================
# On extrait IDNUM + variables clés pour chaque année
# Ces variables serviront à vérifier que le même IDNUM
# correspond bien à la même exploitation d'une année à l'autre

cat("=== Extraction des IDNUM par année ===\n")

# Variables qu'on garde pour la vérification
vars_verif <- c("IDNUM", "OTEXE", "CDEXE", "NREG", "ASSRE",
                "TAUDET_NR", "AUTOF", "RESCO", "annee")

# Extraction pour toutes les années >= 2002

# Renomme data pour éviter le conflit avec la fonction R de base
rica_data <- data

idnum_all <- purrr::map_dfr(rica_data, function(d) {
  vars_ok <- intersect(vars_verif, names(d))
  d %>% select(all_of(vars_ok))
}) %>%
  filter(annee >= 2002) %>%
  mutate(NREG = as.character(NREG))

cat("Total observations :", nrow(idnum_all), "\n")
cat("Années couvertes   :", n_distinct(idnum_all$annee), "\n")
cat("IDNUM distincts    :", n_distinct(idnum_all$IDNUM), "\n\n")

# ============================================================
# 2. TEST 1 — TAUX DE CHEVAUCHEMENT ENTRE ANNÉES CONSÉCUTIVES
# ============================================================
# Pour chaque paire d'années consécutives, on calcule :
# - le nombre d'IDNUM communs
# - le % de l'année t-1 qu'on retrouve en t
# - le % de l'année t qui était déjà en t-1

cat("=== TEST 1 : Chevauchement entre années consécutives ===\n")

annees <- sort(unique(idnum_all$annee))

chevauchement <- map_dfr(2:length(annees), function(i) {
  a1 <- annees[i-1]
  a2 <- annees[i]
  
  id1 <- idnum_all %>% filter(annee == a1) %>% pull(IDNUM)
  id2 <- idnum_all %>% filter(annee == a2) %>% pull(IDNUM)
  
  n_communs <- length(intersect(id1, id2))
  
  tibble(
    annee_t1     = a1,
    annee_t2     = a2,
    n_t1         = length(id1),
    n_t2         = length(id2),
    n_communs    = n_communs,
    pct_t1       = round(100 * n_communs / length(id1), 1),
    pct_t2       = round(100 * n_communs / length(id2), 1)
  )
})

print(chevauchement, n = 30)

# Moyenne du taux de chevauchement
cat("\nTaux de chevauchement moyen :", 
    round(mean(chevauchement$pct_t1), 1), "%\n\n")

# Sauvegarde tableau
chevauchement %>%
  kable(
    col.names = c("Année t-1", "Année t", "N(t-1)", "N(t)",
                  "IDNUM communs", "% de t-1 retrouvés en t",
                  "% de t présents en t-1"),
    caption = "Taux de chevauchement des IDNUM entre années consécutives"
  ) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_idnum_chevauchement.png")

cat("Tableau 1 sauvegardé.\n\n")

# ============================================================
# 3. TEST 2 — STABILITÉ DES CARACTÉRISTIQUES
# ============================================================
# Pour les IDNUM présents dans deux années consécutives,
# on vérifie si OTEXE, CDEXE et NREG sont stables
# Un vrai identifiant permanent devrait avoir ces variables stables
# à ~95%+ (changements légitimes : croissance → changement CDEXE,
# diversification → changement OTEXE)

cat("=== TEST 2 : Stabilité des caractéristiques ===\n")

stabilite <- map_dfr(2:length(annees), function(i) {
  a1 <- annees[i-1]
  a2 <- annees[i]
  
  d1 <- idnum_all %>%
    filter(annee == a1) %>%
    select(IDNUM, OTEXE, CDEXE, NREG) %>%
    rename(OTEXE_t1 = OTEXE, CDEXE_t1 = CDEXE, NREG_t1 = NREG)
  
  d2 <- idnum_all %>%
    filter(annee == a2) %>%
    select(IDNUM, OTEXE, CDEXE, NREG) %>%
    rename(OTEXE_t2 = OTEXE, CDEXE_t2 = CDEXE, NREG_t2 = NREG)
  
  communs <- inner_join(d1, d2, by = "IDNUM")
  
  if (nrow(communs) == 0) return(NULL)
  
  tibble(
    annee_t1         = a1,
    annee_t2         = a2,
    n_communs        = nrow(communs),
    pct_meme_OTEXE   = round(100 * mean(communs$OTEXE_t1 == communs$OTEXE_t2), 1),
    pct_meme_CDEXE   = round(100 * mean(communs$CDEXE_t1 == communs$CDEXE_t2), 1),
    pct_meme_NREG    = round(100 * mean(communs$NREG_t1  == communs$NREG_t2),  1)
  )
})

print(stabilite, n = 30)

cat("\nMoyennes sur toute la période :\n")
cat(" OTEXE stable :", round(mean(stabilite$pct_meme_OTEXE), 1), "%\n")
cat(" CDEXE stable :", round(mean(stabilite$pct_meme_CDEXE), 1), "%\n")
cat(" NREG stable  :", round(mean(stabilite$pct_meme_NREG),  1), "%\n\n")

# Sauvegarde
stabilite %>%
  kable(
    col.names = c("Année t-1", "Année t", "N communs",
                  "% OTEXE stable", "% CDEXE stable", "% NREG stable"),
    caption = "Stabilité des caractéristiques pour les IDNUM communs"
  ) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_idnum_stabilite.png")

cat("Tableau 2 sauvegardé.\n\n")

# ============================================================
# 4. TEST 3 — DISTRIBUTION DU NOMBRE D'ANNÉES PAR IDNUM
# ============================================================
# Combien d'années chaque IDNUM est-il présent ?
# Un vrai panel individuel devrait avoir beaucoup d'IDNUM
# présents sur de longues séquences

cat("=== TEST 3 : Distribution du nombre d'années par IDNUM ===\n")

presence <- idnum_all %>%
  group_by(IDNUM) %>%
  summarise(
    n_annees   = n_distinct(annee),
    annee_min  = min(annee),
    annee_max  = max(annee),
    .groups    = "drop"
  )

distribution <- presence %>%
  mutate(categorie = case_when(
    n_annees == 1  ~ "1 an",
    n_annees <= 3  ~ "2-3 ans",
    n_annees <= 5  ~ "4-5 ans",
    n_annees <= 10 ~ "6-10 ans",
    n_annees <= 15 ~ "11-15 ans",
    n_annees <= 20 ~ "16-20 ans",
    TRUE           ~ "21-23 ans"
  )) %>%
  count(categorie) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(categorie)

print(distribution)

cat("\nIDNUM présents 10+ ans :", 
    sum(presence$n_annees >= 10), "\n")
cat("IDNUM présents 20+ ans :", 
    sum(presence$n_annees >= 20), "\n\n")

distribution %>%
  kable(
    col.names = c("Durée de présence", "Nb IDNUM", "% du total"),
    caption   = "Distribution des IDNUM selon leur durée de présence dans le panel"
  ) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_idnum_distribution.png")

cat("Tableau 3 sauvegardé.\n\n")

# ============================================================
# 5. TEST 4 — GAPS DANS LES TRAJECTOIRES
# ============================================================
# Un IDNUM avec des gaps (disparaît une année puis réapparaît)
# est suspect — soit c'est une coïncidence de numérotation,
# soit une sortie-rentrée réelle dans le panel de sondage

cat("=== TEST 4 : Gaps dans les trajectoires ===\n")

gaps <- idnum_all %>%
  group_by(IDNUM) %>%
  arrange(annee) %>%
  summarise(
    n_annees  = n_distinct(annee),
    annee_min = min(annee),
    annee_max = max(annee),
    n_attendu = annee_max - annee_min + 1,
    a_gap     = as.integer(n_annees < n_attendu),
    .groups   = "drop"
  ) %>%
  filter(n_annees >= 2)  # on ne regarde que les IDNUM présents 2+ ans

cat("IDNUM présents 2+ ans :", nrow(gaps), "\n")
cat("Avec gaps             :", sum(gaps$a_gap), 
    "(", round(100 * mean(gaps$a_gap), 1), "%)\n")
cat("Sans gaps             :", sum(gaps$a_gap == 0),
    "(", round(100 * mean(gaps$a_gap == 0), 1), "%)\n\n")

# Exemple de quelques IDNUM avec gaps
cat("Exemples d'IDNUM avec gaps :\n")
gaps %>%
  filter(a_gap == 1) %>%
  arrange(IDNUM) %>%
  head(10) %>%
  print()

# ============================================================
# 6. TEST 5 — STABILITÉ DES VARIABLES FINANCIÈRES
# ============================================================
# Pour les IDNUM présents 2 années consécutives, on vérifie si
# les variables financières sont cohérentes dans le temps.
# Un vrai individu devrait avoir des variables financières
# qui évoluent progressivement, pas de façon erratique.
# On calcule la corrélation de RESCO entre t et t+1 pour
# les IDNUM communs — une corrélation élevée renforce l'hypothèse
# de continuité individuelle.

cat("=== TEST 5 : Corrélation des variables financières ===\n")

corr_financiere <- map_dfr(2:length(annees), function(i) {
  a1 <- annees[i-1]
  a2 <- annees[i]
  
  d1 <- idnum_all %>%
    filter(annee == a1) %>%
    select(IDNUM, RESCO, TAUDET_NR, AUTOF) %>%
    rename_with(~paste0(., "_t1"), -IDNUM)
  
  d2 <- idnum_all %>%
    filter(annee == a2) %>%
    select(IDNUM, RESCO, TAUDET_NR, AUTOF) %>%
    rename_with(~paste0(., "_t2"), -IDNUM)
  
  communs <- inner_join(d1, d2, by = "IDNUM") %>%
    filter(!is.na(RESCO_t1), !is.na(RESCO_t2),
           !is.na(TAUDET_NR_t1), !is.na(TAUDET_NR_t2))
  
  if (nrow(communs) < 10) return(NULL)
  
  tibble(
    annee_t1       = a1,
    annee_t2       = a2,
    n              = nrow(communs),
    corr_RESCO     = round(cor(communs$RESCO_t1,     communs$RESCO_t2),     3),
    corr_TAUDET    = round(cor(communs$TAUDET_NR_t1, communs$TAUDET_NR_t2), 3),
    corr_AUTOF     = round(cor(communs$AUTOF_t1,     communs$AUTOF_t2),     3)
  )
})

print(corr_financiere, n = 30)

cat("\nCorrélations moyennes :\n")
cat(" RESCO     :", round(mean(corr_financiere$corr_RESCO,  na.rm=TRUE), 3), "\n")
cat(" TAUDET_NR :", round(mean(corr_financiere$corr_TAUDET, na.rm=TRUE), 3), "\n")
cat(" AUTOF     :", round(mean(corr_financiere$corr_AUTOF,  na.rm=TRUE), 3), "\n\n")

corr_financiere %>%
  kable(
    col.names = c("Année t-1", "Année t", "N",
                  "Corr. RESCO", "Corr. TAUDET_NR", "Corr. AUTOF"),
    caption   = "Corrélation des variables financières pour les IDNUM communs"
  ) %>%
  kable_styling(full_width = FALSE, font_size = 11,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tab_idnum_correlations.png")

cat("Tableau 5 sauvegardé.\n\n")

# ============================================================
# 7. CONCLUSION
# ============================================================

cat("=== CONCLUSION ===\n\n")

cat("Récapitulatif des tests :\n")
cat(" Test 1 — Chevauchement moyen      :", 
    round(mean(chevauchement$pct_t1), 1), "% des IDNUM retrouvés d'une année à l'autre\n")
cat(" Test 2 — Stabilité OTEXE          :",
    round(mean(stabilite$pct_meme_OTEXE), 1), "%\n")
cat(" Test 2 — Stabilité CDEXE          :",
    round(mean(stabilite$pct_meme_CDEXE), 1), "%\n")
cat(" Test 2 — Stabilité NREG           :",
    round(mean(stabilite$pct_meme_NREG), 1), "%\n")
cat(" Test 4 — IDNUM sans gaps          :",
    round(100 * mean(gaps$a_gap == 0), 1), "%\n")
cat(" Test 5 — Corrélation RESCO moy.   :",
    round(mean(corr_financiere$corr_RESCO, na.rm=TRUE), 3), "\n")
cat("\n")
cat("Si le chevauchement est > 85%, la stabilité > 90%,\n")
cat("les gaps < 5% et les corrélations financières > 0.7,\n")
cat("IDNUM peut être utilisé comme identifiant individuel.\n")