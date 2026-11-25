# ============================================================
# RICA Panel — Script complet de construction
# ============================================================
# Objectif : empiler les bases RICA 2000-2024 en un panel cohérent
# pour l'analyse des déterminants de l'assurance-récolte
#
# Structure du script :
#   0. Configuration et packages
#   1. Liste des fichiers et vérification
#   2. Chargement de chaque année dans data (liste R)
#   3. Vérification des anomalies identifiées
#   4. Table de correspondance des régions
#   5. Fonction de chargement harmonisé
#   6. Empilement du panel
#   7. Construction des variables dérivées
#   8. Variables de sinistre passé (lag)
#   9. Nettoyage : exclusion 2000-2001 + dummies de rupture
#  10. Vérifications finales
#  11. Sauvegarde
# ============================================================

library(tidyverse)
library(survey)   

# ============================================================
# 0. CONFIGURATION
# ============================================================

data_path <- "data/"

# Variables conservées dans le panel final
# On ne charge pas les ~985 variables de chaque année —
# seulement celles utiles à l'analyse pour alléger la mémoire
vars_panel <- c(
  # Identifiants et structure
  "annee",       # année de l'exercice (construite par le script)
  "CDEXE",       # Classe de dimension économique (taille)
  "OTEXE",       # Orientation technico-économique (type de production)
  "EXTR2",       # Poids d'extrapolation — obligatoire pour toute stat pondérée
  "NREG",        # Région (nouveaux codes post-2016, harmonisé sur toute la période)
  "ZALTI",       # Zone altimétrique (plaine=1, collines=2, montagne=3)
  "ZDEFA",       # Zone défavorisée
  
  # Variable dépendante
  "ASSRE",       # Charges d'assurance-récolte (€) — 0 = non assuré
  
  # Variables assurance complémentaires
  "INDAS",       # Indemnités d'assurance reçues (€) — sinistre indemnisé
  "SBVCLIM",     # Aide calamités agricoles (€) — proxy sinistre non assuré
  "SBVASS",      # Aide à l'assurance-récolte (€) — uniquement à partir de 2024
  # (réforme Descrozaille — loi n°2022-298 du 2 mars 2022)
  
  # Variables financières
  "TAUDET_NR",   # Taux d'endettement (%) — libellé officiel RICA
  "AUTOF",       # Autofinancement (€) — peut être négatif
  "ANLMT",       # Annuité des emprunts long et moyen terme (€)
  "RESCO",       # Résultat courant avant impôt / RCAI (€)
  "RESEX",       # Résultat d'exploitation (€)
  "TRESO",       # Trésorerie nette (€)
  
  # Taille économique continue
  "PBUCE",        # Production Brute Standard en euros
  "IDNUM"
)

# ============================================================
# 1. LISTE DES FICHIERS ET VÉRIFICATION
# ============================================================
# Noms exacts des fichiers — attention à la casse qui varie selon les années
# (Rica France vs rica France vs RICA)

fichiers <- tribble(
  ~annee, ~fichier,
  2000,  "Rica France 2000.txt",
  2001,  "Rica France 2001.txt",
  2002,  "Rica France 2002.txt",
  2003,  "Rica France 2003.txt",
  2004,  "Rica France 2004.txt",
  2005,  "Rica France 2005.txt",
  2006,  "Rica France 2006.txt",
  2007,  "Rica France 2007.txt",
  2008,  "Rica France 2008.txt",
  2009,  "Rica France 2009.txt",
  2010,  "Rica France 2010.txt",
  2011,  "Rica France 2011.txt",
  2012,  "rica France 2012.txt",
  2013,  "rica2013.txt",
  2014,  "rica2014.txt",
  2015,  "RICA2015.txt",
  2016,  "RICA2016.txt",
  2017,  "RICA2017.txt",
  2018,  "RICA2018.txt",
  2019,  "Rica_France_micro_Donnees_ex2019.csv",
  2020,  "Rica_France_micro_Donnees_ex2020.csv",
  2021,  "Rica_France_micro_Donnees_ex2021.csv",
  2022,  "Rica_France_micro_Donnees_ex2022_corrige.csv",
  2023,  "Rica_France_micro_Donnees_ex2023.csv",
  2024,  "Rica_France_micro_Donnees_ex2024.csv"
) %>%
  mutate(chemin = file.path(data_path, fichier))

# Vérification que tous les fichiers existent
manquants <- fichiers %>% filter(!file.exists(chemin))
if (nrow(manquants) > 0) {
  cat("ATTENTION — fichiers manquants :\n")
  print(manquants$fichier)
} else {
  cat("Tous les fichiers sont présents.\n")
}

# ============================================================
# 2. CHARGEMENT DE CHAQUE ANNÉE DANS UNE LISTE (data)
# ============================================================
# On charge ici les bases COMPLÈTES (toutes les variables) de chaque
# année dans une liste R appelée "data". Cela permet d'explorer
# chaque année individuellement, de vérifier les anomalies identifiées,
# et de comparer les structures entre années.
#
# Accès : data[["2000"]] pour la base 2000, data[["2021"]] pour 2021, etc.
#
# Note : ce chargement est plus lourd en mémoire que le panel
# (on charge toutes les variables). Sur une machine avec moins de
# 16 Go de RAM, cette section peut être lente.

cat("\n--- Chargement des bases complètes dans data ---\n")

data <- list()

for (i in seq_len(nrow(fichiers))) {
  annee_i  <- fichiers$annee[i]
  chemin_i <- fichiers$chemin[i]
  
  cat(" ", annee_i, "...\n")
  
  data[[as.character(annee_i)]] <- read_delim(
    chemin_i,
    delim    = ";",
    locale   = locale(decimal_mark = ".", encoding = "latin1"),
    show_col_types = FALSE
  ) %>%
    rename_with(toupper) %>%   # uniformise la casse (regio → REGIO, etc.)
    mutate(annee = annee_i)
}

cat("Bases chargées dans data. Accès : data[[\"2000\"]], data[[\"2021\"]], etc.\n")

# ============================================================
# 3. VÉRIFICATIONS DES ANOMALIES IDENTIFIÉES
# ============================================================

cat("\n=== VÉRIFICATION DES ANOMALIES ===\n")

# --- Anomalie 1 : ASSRE = 0 pour toutes les exploitations en 2000-2001 ---
# Explication : avant 2002, ASSRE ne captait pas encore les contrats
# d'assurance récolte dans le questionnaire RICA (rupture de définition).
# Ces deux années sont exclues du panel final.
cat("\nASSRE en 2000-2001 :\n")
for (a in c("2000", "2001")) {
  n_nonzero <- sum(data[[a]]$ASSRE > 0, na.rm = TRUE)
  cat(" ", a, "— observations ASSRE > 0 :", n_nonzero, "/", nrow(data[[a]]), "\n")
}

# --- Anomalie 2 : 2012-2013 ont 999 variables (vs 995 pour les autres) ---
# Explication : légère extension du questionnaire sur ces deux années,
# sans impact sur nos variables d'intérêt.
cat("\nNombre de variables par année :\n")
sapply(data, ncol) %>%
  enframe(name = "annee", value = "n_vars") %>%
  mutate(annee = as.integer(annee)) %>%
  arrange(annee) %>%
  print(n = 30)

# --- Anomalie 3 : 2021 a 1041 variables (vs ~985 pour les autres) ---
# Explication : ajout d'un module apicole (56 variables AAL = abeilles)
# + renommage de SBVDPM/SBVDPS en SBVDPUM/SBVDPUS (2 variables)
cat("\nVariables ajoutées en 2021 vs 2020 :\n")
extra_2021 <- setdiff(names(data[["2021"]]), names(data[["2020"]]))
cat(" ", length(extra_2021), "variables ajoutées :\n")
cat(" ", extra_2021, "\n")

cat("\nVariables supprimées en 2021 vs 2020 :\n")
missing_2021 <- setdiff(names(data[["2020"]]), names(data[["2021"]]))
cat(" ", missing_2021, "\n")

# --- Anomalie 4 : variable région REGIO vs NREG ---
# Avant 2015 : REGIO (anciens codes, 22 régions)
# À partir de 2015 : NREG (nouveaux codes, 13 régions post-réforme territoriale)
# En 2015-2024 : les deux coexistent
cat("\nPrésence des variables région par année :\n")
tibble(
  annee = as.integer(names(data)),
  REGIO = sapply(data, function(d) "REGIO" %in% names(d)),
  NREG  = sapply(data, function(d) "NREG"  %in% names(d))
) %>%
  arrange(annee) %>%
  print(n = 30)

# --- Anomalie 5 : taux d'assurance brut par année ---
# Baisse visible entre 2015 et 2022 malgré la hausse des subventions.
# Deux explications possibles :
# (a) hausse des primes qui décourage les petites exploitations
# (b) effet de la réforme territoriale sur la pondération EXTR2
cat("\nTaux d'assurance brut (non pondéré) et prime moyenne par année :\n")
sapply(data, function(d) {
  c(
    pct_assures = mean(d$ASSRE > 0, na.rm = TRUE) * 100,
    prime_moy   = mean(d$ASSRE[d$ASSRE > 0], na.rm = TRUE)
  )
}) %>%
  t() %>%
  as_tibble(rownames = "annee") %>%
  mutate(annee = as.integer(annee)) %>%
  arrange(annee) %>%
  print(n = 30)

# ============================================================
# 4. TABLE DE CORRESPONDANCE DES RÉGIONS
# ============================================================
# Avant 2015 : REGIO avec anciens codes (22 régions)
# À partir de 2015 : NREG avec nouveaux codes (13 régions)
# On convertit REGIO → NREG pour harmoniser sur toute la période.
#
# Limite : la fusion de plusieurs anciennes régions en une nouvelle
# (ex : Grand Est = Champagne-Ardenne + Alsace + Lorraine) implique
# une perte de granularité pour les années 2000-2015.

table_regions <- tribble(
  ~REGIO_old, ~NREG_new,
  "11",       "11",   # Île-de-France (inchangé)
  "21",       "44",   # Champagne-Ardenne → Grand Est
  "22",       "44",   # Alsace → Grand Est
  "23",       "44",   # Lorraine → Grand Est
  "24",       "27",   # Franche-Comté → Bourgogne-Franche-Comté
  "25",       "27",   # Bourgogne → Bourgogne-Franche-Comté
  "26",       "28",   # Haute-Normandie → Normandie
  "31",       "32",   # Nord-Pas-de-Calais → Hauts-de-France
  "32",       "32",   # Picardie → Hauts-de-France
  "41",       "28",   # Basse-Normandie → Normandie
  "42",       "53",   # Bretagne (inchangé)
  "43",       "52",   # Pays de la Loire (inchangé)
  "52",       "24",   # Centre → Centre-Val de Loire
  "53",       "75",   # Poitou-Charentes → Nouvelle-Aquitaine
  "54",       "75",   # Aquitaine → Nouvelle-Aquitaine
  "72",       "76",   # Midi-Pyrénées → Occitanie
  "73",       "84",   # Rhône-Alpes → Auvergne-Rhône-Alpes
  "74",       "84",   # Auvergne → Auvergne-Rhône-Alpes
  "82",       "76",   # Languedoc-Roussillon → Occitanie
  "83",       "75",   # Limousin → Nouvelle-Aquitaine
  "91",       "93",   # PACA (inchangé)
  "93",       "94",   # Corse (inchangé)
  "94",       "95",   # Guadeloupe
  "95",       "96",   # Martinique
  "98",       "98"    # Réunion (inchangé)
)

# ============================================================
# 5. FONCTION DE CHARGEMENT HARMONISÉ
# ============================================================
# Charge un fichier RICA, uniformise la casse, harmonise la région,
# renomme les variables PAC modifiées en 2021, et sélectionne
# uniquement les variables utiles pour le panel.

charger_rica <- function(chemin, annee) {
  
  df <- read_delim(
    chemin,
    delim    = ";",
    locale   = locale(decimal_mark = ".", encoding = "latin1"),
    show_col_types = FALSE
  ) %>%
    rename_with(toupper) %>%       # uniformise la casse
    mutate(annee = annee)
  
  # Harmonisation région
  if (annee <= 2014) {
    # 2000-2014 : REGIO (anciens codes) → NREG (nouveaux codes)
    df <- df %>%
      mutate(REGIO = as.character(REGIO)) %>%
      left_join(table_regions, by = c("REGIO" = "REGIO_old")) %>%
      rename(NREG = NREG_new) %>%
      select(-any_of("REGIO"))
  } else {
    # 2015+ : NREG existe déjà, on supprime REGIO
    df <- df %>%
      mutate(NREG = as.character(NREG)) %>%
      select(-any_of("REGIO"))
  }
  
  # Renommage SBVDPM/SBVDPS → SBVDPUM/SBVDPUS (changement en 2021)
  if ("SBVDPM" %in% names(df)) df <- df %>% rename(SBVDPUM = SBVDPM)
  if ("SBVDPS" %in% names(df)) df <- df %>% rename(SBVDPUS = SBVDPS)
  
  # Sélection des variables utiles
  vars_presentes <- intersect(vars_panel, names(df))
  df %>% select(all_of(vars_presentes))
}

# ============================================================
# 6. EMPILEMENT DU PANEL
# ============================================================
# pmap_dfr() applique charger_rica() à chaque fichier et empile
# les résultats. Les colonnes absentes dans certaines années
# (ex : SBVASS avant 2024) sont remplies automatiquement avec NA.

cat("\n--- Construction du panel harmonisé ---\n")

panel <- fichiers %>%
  pmap_dfr(function(annee, fichier, chemin) {
    cat(" ", annee, "...\n")
    charger_rica(chemin, annee)
  })

cat("\nPanel brut chargé :", nrow(panel), "observations,",
    ncol(panel), "variables\n")

# ============================================================
# 7. CONSTRUCTION DES VARIABLES DÉRIVÉES
# ============================================================

panel <- panel %>%
  mutate(
    
    # Variable dépendante : souscription assurance-récolte
    # 1 si l'exploitation a payé une prime (ASSRE > 0), 0 sinon
    assure = as.integer(ASSRE > 0),
    
    # Sinistre indemnisé — uniquement pour les assurés
    # 1 si indemnité reçue dans l'année (INDAS > 0)
    sinistre = as.integer(INDAS > 0),
    
    # Proxy sinistre climatique — pour tous (assurés et non-assurés)
    # 1 si aide calamités agricoles reçue (SBVCLIM > 0)
    # Attention : mesuré la même année que la décision d'assurance
    # Limite : pas un vrai sinistre "passé" sur coupe transversale
    calamite = as.integer(SBVCLIM > 0),
    
    # Taux d'endettement en proportion (TAUDET_NR est en %)
    ratio_dette = TAUDET_NR / 100,
    
    # Transformation IHS de l'autofinancement
    # AUTOF peut être négatif — log(x+1) serait indéfini
    # IHS = log(x + sqrt(x²+1)) définie sur R entier
    # Se comporte comme log pour les grandes valeurs positives
    # Référence : Bellemare & Wichman (2020), Oxford Bulletin of Econ. & Stat.
    autof_ihs = log(AUTOF + sqrt(AUTOF^2 + 1)),
    
    # SBVASS : remplacer NA par 0 pour les années avant 2024
    # La variable n'existe que dans le RICA 2024 (exercice 2023)
    # Avant 2022 : pas de subvention à l'assurance au sens Descrozaille
    SBVASS = replace_na(SBVASS, 0),
    
    # Prime nette de subvention = coût réel pour l'agriculteur
    prime_nette = ASSRE - SBVASS
  )

# ============================================================
# 8. VARIABLES DE SINISTRE PASSÉ (LAG)
# ============================================================
# C'est LA variable qu'on ne pouvait pas construire sur coupe.
# calamite_lag = aide calamités reçue l'année précédente (t-1).
# Un choc climatique en t-1 devrait augmenter la probabilité
# de s'assurer en t (effet d'apprentissage / perception du risque).
#
# LIMITE IMPORTANTE : sans identifiant individuel dans les données
# publiques, on ne peut pas faire un vrai lag par exploitation.
# On approxime en construisant le lag au niveau du groupe
# (NREG × OTEXE × CDEXE). C'est une limite à mentionner
# explicitement dans le mémoire.

panel <- panel %>%
  arrange(NREG, OTEXE, CDEXE, annee) %>%
  group_by(NREG, OTEXE, CDEXE) %>%
  mutate(
    # Sinistre climatique l'année précédente
    calamite_lag = lag(calamite, 1),
    # Était assuré l'année précédente
    assure_lag   = lag(assure, 1)
  ) %>%
  ungroup()

# ============================================================
# 9. NETTOYAGE FINAL ET DUMMIES DE RUPTURE
# ============================================================

# Exclusion de 2000-2001
# Raison : ASSRE = 0 pour toutes les exploitations — rupture de définition
# du questionnaire RICA, pas une vraie absence d'assurance.
# Le panel commence donc en 2002.
panel <- panel %>%
  filter(annee >= 2002) %>%
  mutate(
    
    # Dummy réforme territoriale (2016)
    # À partir de 2016, les régions RICA sont les 13 nouvelles régions.
    # Peut affecter la pondération EXTR2 et la comparabilité régionale.
    post_2016 = as.integer(annee >= 2016),
    
    # Dummy réforme Descrozaille (2023)
    # La loi n°2022-298 du 2 mars 2022 est entrée en vigueur pour
    # l'exercice 2023 (RICA 2024) : hausse subvention à 70%,
    # simplification administrative.
    post_reforme = as.integer(annee >= 2023)
  )

cat("\nPanel final après exclusion 2000-2001 :",
    nrow(panel), "observations\n")

# ============================================================
# 10. VÉRIFICATIONS FINALES
# ============================================================

cat("\n=== VÉRIFICATIONS PANEL FINAL ===\n")

# Observations par année
cat("\nObservations par année :\n")
panel %>% count(annee) %>% print(n = 30)

# Taux d'assurance pondéré par année
# C'est le résultat central — on doit voir :
# - ~40% stable entre 2002 et 2014
# - légère baisse 2015-2022 (hausse des primes + rupture territoriale)
# - remontée 2023-2024 (réforme Descrozaille)
cat("\nTaux d'assurance pondéré (EXTR2) par année :\n")
panel %>%
  group_by(annee) %>%
  summarise(
    taux_pondere  = weighted.mean(assure, EXTR2, na.rm = TRUE),
    taux_brut     = mean(assure, na.rm = TRUE),
    prime_moy     = mean(ASSRE[ASSRE > 0], na.rm = TRUE),
    n_obs         = n()
  ) %>%
  print(n = 30)

# Taux d'assurance par OTEXE (type de production)
cat("\nTaux d'assurance pondéré par type de production (OTEXE) :\n")
panel %>%
  group_by(OTEXE) %>%
  summarise(taux = weighted.mean(assure, EXTR2, na.rm = TRUE)) %>%
  arrange(desc(taux)) %>%
  print(n = 20)

# Valeurs manquantes
cat("\nNA par variable :\n")
panel %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA") %>%
  filter(n_NA > 0) %>%
  arrange(desc(n_NA)) %>%
  print()

cat("\nPANEL FINAL :", nrow(panel), "observations |",
    ncol(panel), "variables |",
    n_distinct(panel$annee), "années (2002-2024)\n")

library(knitr)
library(kableExtra)

taux_otexe_panel <- panel %>%
  group_by(OTEXE) %>%
  summarise(taux = weighted.mean(assure, EXTR2, na.rm = TRUE)) %>%
  arrange(desc(taux)) %>%
  mutate(
    OTEXE_label = c(
      "15" = "Céréales/oléagineux",
      "16" = "Autres grandes cultures",
      "83" = "Grandes cultures + herbivores",
      "84" = "Cultures + élevages mixtes",
      "61" = "Polyculture",
      "37" = "Viticulture qualité",
      "38" = "Autre viticulture",
      "74" = "Polyélevage granivores",
      "39" = "Fruits",
      "47" = "Bovins mixtes",
      "73" = "Polyélevage herbivores",
      "45" = "Bovins lait",
      "46" = "Bovins viande",
      "50" = "Granivores",
      "48" = "Ovins/caprins",
      "29" = "Horticulture",
      "28" = "Maraîchage"
    )[as.character(OTEXE)],
    taux_pct = scales::percent(taux, accuracy = 0.1)
  ) %>%
  select(OTEXE, OTEXE_label, taux_pct)

# Sauvegarde en PNG via kableExtra
taux_otexe_panel %>%
  kable(
    col.names = c("Code OTEXE", "Type de production", "Taux d'assurance"),
    caption   = "Taux d'assurance-récolte pondéré par type de production (2002-2024)",
    align     = c("c", "l", "r")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width        = FALSE,
    font_size         = 12
  ) %>%
  save_kable("results/tableau_taux_otexe.png")

cat("Tableau sauvegardé : tableau_taux_otexe.png\n")

# ============================================================
# 11. SAUVEGARDE
# ============================================================
# Format .rds : natif R, rapide à recharger, conserve les types
# Pour recharger : panel <- readRDS("data/panel_rica_2002_2024.rds")

saveRDS(panel, "data/panel_rica_2002_2024.rds")
cat("Panel sauvegardé : panel_rica_2002_2024.rds\n")

# La liste data (bases complètes par année) reste en mémoire
# pour exploration. Pour la sauvegarder aussi :
# saveRDS(data, "data/data_brut_toutes_annees.rds")
# Attention : fichier lourd (~2-3 Go)
# Alias pour éviter le conflit avec la fonction R de base data()
rica_data <- data
cat("rica_data disponible en mémoire.\n")
