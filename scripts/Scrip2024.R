 # RICA 2024 : Statistiques descriptives 
library(tidyverse)
library(survey)
library(knitr)

# --- Chargement ---
rica <- read_delim(
  "data/Rica_France_micro_Donnees_ex2024.csv",
  delim = ";",
  locale = locale(decimal_mark = ".", grouping_mark = ","),
  show_col_types = FALSE
)

# Construction des variables 

# Objectif du bloc construire les variables et les indicateurs our ensuite répondre aux questions : qui s'assure ? pourquoi ? quels effets ? 

# Première variable : ASSRE : Charges d'assurance-récolte (en euros) 
# Elle représente le montant de la prime d'assurance-récolte payée; 0 représente les exploitation non assurées et >0 pour les assurées. 
# On en dérive deux variables : assure (binaire pour assuré ou non) et log_ASSRE : log du montant de la prime parmi les assurés. 

# Deuxième variable : INDAS : Produits des indemnités d'assurance (en euros)
# Montant des indemnités reçues de l'assureur dans l'année;  Une valeur > 0 signifie que l'exploitation a subi un sinistre et a été indemnisée.
# On crée une indicatrice du sinistre  mais du coup que pour les assurés. 

# Troisième variable : SBVCLIM : Aide calamités agricoles (en euros)
# Représente une indémnisation / aide publique versée aux exploitations victimes d'un aléa climatique reconnu comme calamité agricole (régime public, distinct de l'assurance privée)
# Une valeur >0 peut indiquer un sinistre chez un non-assuré. On crée une indicatrice binaire "calamite" 

# Quatrième variable : TAUDET_NR : Taux d'endettement (en %) 
# Pour étudier le lien endettement assurance : on crée ratio_dette en la divisant par 100 pour l'avoir entre 0 et 1 pour les régressions. 

rica <- rica %>%
  mutate(
    assure      = as.integer(ASSRE > 0),
    log_ASSRE   = ifelse(ASSRE > 0, log(ASSRE), NA),
    ratio_dette = TAUDET_NR / 100,
    sinistre    = as.integer(INDAS > 0),
    calamite    = as.integer(SBVCLIM > 0)
  )

# Cinquième variable : Pondération : Variable : EXTR2 : Poids d'extrapolation
# le poinds  que représente chaque exploitation par rapport à l'agriculture française: la fonction syvdesign() fait que ce poids est directement incluts dans le calcul de moyennes proportions et régressions. 

design <- svydesign(ids = ~1, weights = ~EXTR2, data = rica)


# Taux d'assurance global 
# Combien d'exploitations françaises sont assurées en 2023 ? comment varie ce taux avec le type de production ? 
# Villeneuve et al : 13% sur  2000-2020. 

# Sixième variable : OTEXE : Orientation Technico-Économique 17 postes (typologie 2007)
# 15	Exploitations spécialisées en céréaliculture et en culture de plantes oléagineuses et protéagineuses
# 16	Exploitations spécialisées en autres grandes cultures
# 28	Exploitations spécialisées en culture de légumes et champignons
# 29	Exploitations spécialisées en culture de fleurs et horticulture diverse
# 37	Exploitations spécialisées en viticulture de qualité
# 38	Exploitations spécialisées en autre viticulture
# 39	Exploitations spécialisées en cultures fruitières et autres cultures permanentes
# 45	Exploitations bovines spécialisées — orientation lait
# 46	Exploitations bovines spécialisées — orientation élevage et viande
# 47	Exploitations bovines — lait, élevage et viande combinés
# 48	Exploitations avec ovins, caprins et autres herbivores
# 50	Exploitations granivores
# 61	Exploitations de polyculture
# 73	Exploitations de polyélevage à orientation herbivores
# 74	Exploitations de polyélevage à orientation granivores
# 83	Exploitations mixtes grandes cultures et herbivores
# 84	Exploitations de cultures diverses et élevages mixtes

# Taux global pondéré
svymean(~assure, design)

# manuellement (même résultat)
taux_manuel <- sum(rica$EXTR2 * rica$assure) / sum(rica$EXTR2)
taux_manuel

# Par type de production
taux_otex <- svyby(~assure, ~OTEXE, design, svymean) %>%
  arrange(desc(assure))

kable(taux_otex, digits = 2,
      col.names = c("OTEXE", "Taux assurance", "Erreur std"),
      caption = "Taux d'assurance récolte par type de production (pondéré EXTR2)")


# Taux d'assurance par taille économique 
# Villeneuve et al les grandes exploitations s'assurent d'avantage mais bénéficient moins de l'assurance. 
# Septième variable CDEXE : Code Classe de dimension économique (typologie 2007)
# Taille économique mesurée par la Production Brute Standard (PBS) :
#   5 : PBS 15 000 – 25 000 €  (très petites exploitations)
#   6 : PBS 25 000 – 50 000 €
#   7 : PBS 50 000 – 100 000 €
#   8 : PBS 100 000 – 250 000 €
#   9 : PBS 250 000 – 500 000 €
#   10 : PBS ≥ 500 000 €       (très grandes exploitations)
#  RICA public n'inclut que les classes 5 à 10 (PBS ≥ 15k€),
# les micro-exploitations en dessous du seuil ne sont pas couvertes.
taux_cdex <- svyby(~assure, ~CDEXE, design, svymean) %>%
  arrange(CDEXE)

kable(taux_cdex, digits = 2,
      col.names = c("CDEXE", "Taux assurance", "Erreur std"),
      caption = "Taux d'assurance par classe de taille économique (pondéré EXTR2)")


# Comparaison entre assurés et non assurés

# Huitième variable : RESEX : Résultat d'exploitation (euros)
# mesure de la performance économique de l'activité agricole 

# Neuvième variable : RESCO : Résultat Courant Avant Impôt / RCAI (euros)
# RESEX + résultat financier (charges d'intérêts incluses), c'est l'indicateur de revenu de référence dans la littérature RICA car il reflète la situation financière complète de l'exploitation.

# Dixième variable : ANLMT : Annuité des emprunts long et moyen terme (euros) 
# Montant annuel du remboursement des emprunts LMT (capital + intérêts).

# Onzième variable : AUTOF : Autofinancement (euros) 
# Capacité de l'exploitation à financer ses investissements sur ses propres ressources.

compar <- svyby(
  ~RESEX + RESCO + TAUDET_NR + ANLMT + AUTOF,
  ~assure,
  design,
  svymean,
  na.rm = TRUE
)

kable(t(compar), digits = 0,
      col.names = c("Non-assurés", "Assurés"),
      caption = "Comparaison assurés / non-assurés — moyennes pondérées")


# BLOC 6 — Distribution prime, indemnités et coût net
# ============================================================

# Parmi les assurés uniquement, on regarde la distribution de la prime,
# de la subvention reçue, et du ratio indemnité/prime pour ceux
# qui ont subi un sinistre. C'est une première mesure de la
# "rentabilité" brute de l'assurance au niveau individuel.

# --- SBVASS : Aide à l'assurance-récolte (euros) ---
# Subvention PAC couvrant une partie de la prime d'assurance-récolte.
# Depuis la réforme de 2022, cette subvention est passée de 65% à 70%
# de la prime pour les contrats multirisques climatiques.
# prime_nette = ASSRE - SBVASS est le coût réel supporté par l'agriculteur.
# C'est ce coût net — et non la prime brute — qui est pertinent
# pour analyser la décision d'assurance.

# --- ratio_couv : indemnité / prime ---
# Mesure brute du rendement de l'assurance pour les sinistres.
# Un ratio > 1 signifie que l'indemnité dépasse la prime payée.
# Notre analyse préliminaire trouvait une médiane de 0.95 en non-pondéré
# — à comparer avec le résultat pondéré ici.

assures <- rica %>% filter(assure == 1) %>%
  mutate(
    prime_nette = ASSRE - SBVASS,
    ratio_couv  = ifelse(INDAS > 0, INDAS / ASSRE, NA)
  )

design_assures <- svydesign(ids = ~1, weights = ~EXTR2, data = assures)

svyquantile(~ASSRE + prime_nette, design_assures,
            quantiles = c(0.25, 0.5, 0.75),
            na.rm = TRUE)

sinistres <- assures %>% filter(INDAS > 0)
design_sin <- svydesign(ids = ~1, weights = ~EXTR2, data = sinistres)
svymean(~ratio_couv, design_sin, na.rm = TRUE)

# ============================================================
# BLOC 7 — Probit : déterminants de la souscription
# ============================================================

# Variable dépendante : assure (1 = assuré, 0 = non assuré)
#
# Régresseurs retenus d'après Di Falco et al. (2014) et
# Villeneuve et al. (2024) :
#
# TAILLE & STRUCTURE
#   CDEXE    : taille économique (ordinal 5-10) — effet positif attendu
#   PBUCE    : PBS en euros — mesure continue de la taille
#
# TYPE DE PRODUCTION
#   OTEXE    : dummies par orientation — céréaliers + que éleveurs
#
# VARIABLES FINANCIÈRES
#   TAUDET_NR : taux d'endettement — signe ambigu (+ si banque exige
#               assurance, - si contrainte liquidité)
#   AUTOF     : autofinancement — proxy solidité financière
#   TRESO     : trésorerie nette — contrainte de liquidité court terme
#
# SINISTRE PASSÉ (proxy)
#   calamite  : indicatrice SBVCLIM > 0 — a reçu aide calamités
#               = proxy d'un choc climatique passé
# infos sur cette variable (https://mesdemarches.agriculture.gouv.fr/demarches/exploitation-agricole/demander-une-aide-pac/article/demander-une-indemnisationros )
# ZONE GÉOGRAPHIQUE
#   NREG      : région (dummies) — capte exposition climatique régionale
#   ZALTI     : zone altimétrique (plaine/collines/montagne)
#   ZDEFA     : zone défavorisée

probit_1 <- glm(
  assure ~ factor(CDEXE) + factor(OTEXE) +
    ratio_dette + log(AUTOF + 1) + 
    calamite +
    factor(NREG) + factor(ZALTI),
  data   = rica,
  family = binomial(link = "probit"),
  weights = EXTR2
)

summary(probit_1)


# Variables climatiques : données météorologiques téléchargées à partir de odre.opendatasoft.com
# Variables climatiques annuelle par région : température moyenne, température min et température max
# à partir de t min on crée nb_jours_gel : jours ou t min <2023 risque de gel des cultures 
# et amplitude_therm   : TMax - TMin moyenne (stress thermique)


library(lubridate)

# --- Chargement ---
meteo <- read_delim(
  "data/temperature-quotidienne-regionale.csv",
  delim = ";",
  locale = locale(decimal_mark = ".", encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  rename(
    code_region = `Code INSEE région`,
    date        = Date,
    tmin        = `TMin (°C)`,
    tmax        = `TMax (°C)`,
    tmoy        = `TMoy (°C)`
  ) %>%
  mutate(date = as_date(substr(date, 1, 10)))

# --- Filtrer sur 2023 uniquement ---
# Le RICA 2024 couvre l'exercice comptable 2023
meteo_2023 <- meteo %>%
  filter(year(date) == 2023)

# --- Agrégation annuelle par région ---
clim_2023 <- meteo_2023 %>%
  group_by(code_region) %>%
  summarise(
    temp_moy      = mean(tmoy, na.rm = TRUE),
    temp_min_moy  = mean(tmin, na.rm = TRUE),
    nb_jours_gel  = sum(tmin < 0, na.rm = TRUE),
    amplitude_moy = mean(tmax - tmin, na.rm = TRUE)
  ) %>%
  mutate(code_region = as.numeric(code_region))

clim_2023

# les données météorologiques excluent 3 régions qui sont les DOM 95 Guadeloupe 96 Martinique et 98 Réunion 
# On décide de les exclure pour cette partie 

# Merge RICA + données climatiques 2023
# On exclut les DOM (NREG 95, 96, 98)
rica_clim <- rica %>%
  filter(!NREG %in% c(95, 96, 98)) %>%
  left_join(clim_2023, by = c("NREG" = "code_region"))

# Vérification
rica_clim %>%
  summarise(
    n_total       = n(),
    na_temp_moy   = sum(is.na(temp_moy)),
    na_gel        = sum(is.na(nb_jours_gel))
  )

# ============================================================
# BLOC 8 — Probit enrichi avec variables climatiques
# ============================================================

# On ajoute aux déterminants classiques trois variables climatiques
# construites à partir des données Météo-France 2023 :
#
# temp_moy      : température moyenne annuelle régionale
#                 proxy de l'exposition climatique générale
#                 effet attendu ambigu : chaleur = risque sécheresse (+)
#                 mais aussi = saisons plus longues (-)
#
# nb_jours_gel  : nombre de jours où TMin < 0°C en 2023
#                 proxy direct du risque de gel des cultures
#                 effet attendu positif : plus de gel = plus d'assurance
#
# amplitude_moy : écart moyen TMax - TMin quotidien
#                 proxy de la variabilité thermique journalière
#                 stress thermique sur les cultures
#                 effet attendu positif

# Note : on retire factor(NREG) du modèle car les variables
# climatiques captent désormais l'hétérogénéité régionale.
# Garder les deux serait redondant et créerait de la multicolinéarité.

rica_clim <- rica_clim %>%
  mutate(autof_ihs = log(AUTOF + sqrt(AUTOF^2 + 1)))

# Vérification
sum(is.na(rica_clim$autof_ihs))

probit_3 <- glm(
  assure ~ factor(CDEXE) + factor(OTEXE) +
    ratio_dette + autof_ihs +
    calamite +
    temp_moy + nb_jours_gel + amplitude_moy +
    factor(ZALTI),
  data   = rica_clim,
  family = binomial(link = "probit"),
  weights = EXTR2
)

summary(probit_3)

# Comparaison des trois modèles
cat("Probit 1 (sans IHS, avec NREG) — obs:", nobs(probit_1), "\n")
cat("Probit 2 (IHS, avec NREG)      — obs:", nobs(probit_2), "\n")
cat("Probit 3 (IHS, variables clim) — obs:", nobs(probit_3), "\n")


# les effets marginaux 
library(margins)
margins(probit_3) %>% summary()
