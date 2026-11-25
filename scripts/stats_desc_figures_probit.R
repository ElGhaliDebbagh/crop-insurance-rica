# ============================================================
# RICA Panel — Script 2 : Analyse et visualisations
# ============================================================
# Prérequis : avoir lancé panel_script1.R qui crée panel_rica_2002_2024.rds
#
# Structure :
#   1. Chargement du panel
#   2. Figure 1 — Évolution temporelle du taux d'assurance
#   3. Figure 2 — Taux par type de production dans le temps
#   4. Figure 3 — Évolution de la prime moyenne
#   5. Figure 4 — Effet de la réforme 2022 par OTEXE
#   6. Probit sur panel complet avec effets fixes année
#   7. Effet de la réforme — régression DiD simple
# ============================================================

library(tidyverse)
library(survey)
library(margins)

# ============================================================
# 1. CHARGEMENT
# ============================================================

panel <- readRDS("data/panel_rica_2002_2024.rds")

# Thème ggplot commun — style sobre pour un document académique
theme_rica <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    axis.title    = element_text(size = 10),
    legend.title  = element_text(size = 10),
    legend.position = "bottom"
  )

# ============================================================
# 2. FIGURE 1 — Évolution temporelle du taux d'assurance
# ============================================================
# Taux pondéré par EXTR2, avec intervalle de confiance bootstrap
# et annotations des ruptures structurelles

taux_annuel <- panel %>%
  group_by(annee) %>%
  summarise(
    taux     = weighted.mean(assure, EXTR2, na.rm = TRUE),
    prime    = mean(ASSRE[ASSRE > 0], na.rm = TRUE),
    n        = n()
  )

fig1 <- ggplot(taux_annuel, aes(x = annee, y = taux)) +
  
  # Zone réforme — loi n° 2022-298 du 2 mars 2022
  annotate("rect",
           xmin = 2022.5, xmax = 2024.5,
           ymin = -Inf,   ymax = Inf,
           fill = "#2563EB", alpha = 0.06) +
  
  # Zone baisse mystérieuse
  annotate("rect",
           xmin = 2014.5, xmax = 2022.5,
           ymin = -Inf,   ymax = Inf,
           fill = "#DC2626", alpha = 0.04) +
  
  # Ligne principale
  geom_line(color = "#1e3a5f", linewidth = 1.2) +
  geom_point(color = "#1e3a5f", size = 2.5, fill = "white",
             shape = 21, stroke = 1.5) +
  
  # Lignes verticales de rupture
  geom_vline(xintercept = 2015.5, linetype = "dashed",
             color = "#6b7280", linewidth = 0.7) +
  geom_vline(xintercept = 2023,   linetype = "dashed",
             color = "#2563EB",   linewidth = 0.7) +
  
  # Annotations
  annotate("text", x = 2015, y = 0.52,
           label = "Réforme\nterritoriale", hjust = 1,
           size = 3, color = "#6b7280") +
  annotate("text", x = 2023.1, y = 0.39,
           label = "Réforme 2022", hjust = 0,
           size = 3, color = "#2563EB") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.3, 0.55)) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  
  labs(
    title    = "Taux de souscription à l'assurance-récolte en France (2002–2024)",
    subtitle = "Taux pondéré par les coefficients d'extrapolation EXTR2 — RICA public",
    x        = NULL,
    y        = "Taux d'assurance pondéré",
    caption  = "Source : RICA France (Agreste, 2002–2024)."
  ) +
  theme_rica

ggsave("figures/fig1_taux_assurance.png",
       fig1, width = 10, height = 5.5, dpi = 300)
cat("Figure 1 sauvegardée.\n")

# ============================================================
# 3. FIGURE 2 — Taux par type de production dans le temps
# ============================================================
# On garde les 6 OTEXE les plus représentatifs et on trace
# leur évolution — ce qui permet de voir si la baisse 2015-2022
# est homogène ou concentrée sur certains types

# Labels OTEXE lisibles
labels_otexe <- c(
  "15" = "Céréales/oléagineux",
  "45" = "Bovins lait",
  "46" = "Bovins viande",
  "50" = "Granivores",
  "83" = "Grandes cultures + herbivores",
  "28" = "Maraîchage"
)

taux_otexe_annuel <- panel %>%
  filter(OTEXE %in% as.numeric(names(labels_otexe))) %>%
  mutate(OTEXE_label = labels_otexe[as.character(OTEXE)]) %>%
  group_by(annee, OTEXE_label) %>%
  summarise(taux = weighted.mean(assure, EXTR2, na.rm = TRUE),
            .groups = "drop")

fig2 <- ggplot(taux_otexe_annuel,
               aes(x = annee, y = taux,
                   color = OTEXE_label, group = OTEXE_label)) +
  
  geom_vline(xintercept = 2023, linetype = "dashed",
             color = "grey60", linewidth = 0.6) +
  
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 4)) +
  scale_color_manual(values = c(
    "Céréales/oléagineux"            = "#1e3a5f",
    "Bovins lait"                    = "#2563EB",
    "Bovins viande"                  = "#60a5fa",
    "Granivores"                     = "#f59e0b",
    "Grandes cultures + herbivores"  = "#10b981",
    "Maraîchage"                     = "#DC2626"
  )) +
  
  labs(
    title    = "Taux d'assurance-récolte par type de production (2002–2024)",
    subtitle = "Sélection de 6 OTEXE représentatifs — taux pondéré EXTR2",
    x        = NULL,
    y        = "Taux d'assurance pondéré",
    color    = NULL,
    caption  = "Source : RICA France (Agreste, 2002–2024)."
  ) +
  theme_rica +
  guides(color = guide_legend(nrow = 2))

ggsave("figures/fig2_taux_otexe.png",
       fig2, width = 10, height = 5.5, dpi = 300)
cat("Figure 2 sauvegardée.\n")

# ============================================================
# 4. FIGURE 3 — Évolution de la prime moyenne et prime nette
# ============================================================
# La prime double entre 2002 et 2024 — c'est l'une des explications
# de la baisse du taux entre 2015 et 2022

primes <- panel %>%
  filter(assure == 1) %>%
  group_by(annee) %>%
  summarise(
    prime_brute = mean(ASSRE, na.rm = TRUE),
    prime_nette = mean(prime_nette, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(prime_brute, prime_nette),
               names_to = "type", values_to = "prime") %>%
  mutate(type = recode(type,
                       prime_brute = "Prime brute",
                       prime_nette = "Prime nette (après subvention)"
  ))

fig3 <- ggplot(primes, aes(x = annee, y = prime,
                           color = type, group = type)) +
  
  geom_vline(xintercept = 2023, linetype = "dashed",
             color = "grey60", linewidth = 0.6) +
  
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  scale_y_continuous(labels = scales::label_number(
    suffix = " €", big.mark = " ")) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  scale_color_manual(values = c(
    "Prime brute"                    = "#1e3a5f",
    "Prime nette (après subvention)" = "#2563EB"
  )) +
  
  # Note : prime nette = prime brute avant 2024 (SBVASS = 0)
  annotate("text", x = 2023.2, y = 5000,
           label = "Réforme\n2022", hjust = 0,
           size = 3, color = "grey50") +
  
  labs(
    title    = "Évolution de la prime d'assurance-récolte moyenne (2002–2024)",
    subtitle = "Parmi les exploitations assurées uniquement — euros courants",
    x        = NULL,
    y        = "Prime moyenne (€)",
    color    = NULL,
    caption  = "Source : RICA France (Agreste, 2002–2024). \nNote : la prime nette n'est distinguée de la prime brute qu'à partir de 2024 (SBVASS disponible)."
  ) +
  theme_rica

ggsave("figures/fig3_primes.png",
       fig3, width = 10, height = 5.5, dpi = 300)
cat("Figure 3 sauvegardée.\n")

# ============================================================
# 5. FIGURE 4 — Effet de la réforme 2022 par OTEXE
# ============================================================
# Compare le taux d'assurance moyen avant (2018-2022) et après (2023-2024)
# la réforme, par type de production — qui a le plus bénéficié ?

labels_otexe_all <- c(
  "15" = "Céréales/oléagineux",
  "16" = "Autres grandes cultures",
  "28" = "Maraîchage",
  "29" = "Horticulture",
  "37" = "Viticulture qualité",
  "38" = "Autre viticulture",
  "39" = "Fruits",
  "45" = "Bovins lait",
  "46" = "Bovins viande",
  "47" = "Bovins mixtes",
  "48" = "Ovins/caprins",
  "50" = "Granivores",
  "61" = "Polyculture",
  "73" = "Polyélevage herbivores",
  "74" = "Polyélevage granivores",
  "83" = "Grandes cultures + herbivores",
  "84" = "Cultures + élevages mixtes"
)

effet_reforme <- panel %>%
  mutate(
    periode = case_when(
      annee >= 2023 ~ "Après réforme\n(2023-2024)",
      annee >= 2018 ~ "Avant réforme\n(2018-2022)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(periode)) %>%
  mutate(OTEXE_label = labels_otexe_all[as.character(OTEXE)]) %>%
  group_by(OTEXE_label, periode) %>%
  summarise(taux = weighted.mean(assure, EXTR2, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = periode, values_from = taux) %>%
  mutate(delta = `Après réforme\n(2023-2024)` - `Avant réforme\n(2018-2022)`) %>%
  arrange(desc(delta))

fig4 <- ggplot(effet_reforme,
               aes(x = delta,
                   y = reorder(OTEXE_label, delta),
                   fill = delta > 0)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey30") +
  geom_text(aes(label = sprintf("%+.1f pp", delta * 100),
                hjust = ifelse(delta > 0, -0.1, 1.1)),
            size = 3, color = "grey30") +
  
  scale_fill_manual(values = c("TRUE" = "#2563EB", "FALSE" = "#DC2626"),
                    guide = "none") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-0.05, 0.15))  +
  
  labs(
    title    = "Effet de la réforme 2022 sur le taux d'assurance par type de production",
    subtitle = "Variation du taux pondéré entre 2018-2022 et 2023-2024, par type de production",
    x        = "Variation du taux d'assurance (points de pourcentage)",
    y        = NULL,
    caption  = "Source : RICA France (Agreste, 2002–2024). \nNote : en bleu = hausse, en rouge = baisse."
  ) +
  theme_rica +
  theme(panel.grid.major.y = element_blank())

ggsave("figures/fig4_reforme.png",
       fig4, width = 10, height = 7, dpi = 300)
cat("Figure 4 sauvegardée.\n")

# ============================================================
# 6. PROBIT SUR PANEL COMPLET AVEC EFFETS FIXES ANNÉE
# ============================================================
# On reprend le probit de la coupe 2024 mais sur toutes les années.
# On ajoute factor(annee) comme effets fixes temporels pour
# contrôler les chocs communs à toutes les exploitations
# (conditions météo nationales, politique agricole, etc.)
#
# Interprétation des effets fixes année :
# Chaque coefficient annee_t mesure l'écart du taux d'assurance
# en t par rapport à l'année de référence (2002), toutes choses
# égales par ailleurs. C'est la variation "pure" du taux
# non expliquée par les caractéristiques des exploitations.

cat("\n=== PROBIT PANEL AVEC EFFETS FIXES ANNÉE ===\n")

# On exclut les DOM pour la même raison que sur la coupe
panel_metro <- panel %>%
  filter(!NREG %in% c("95", "96", "98")) %>%
  mutate(autof_ihs = log(AUTOF + sqrt(AUTOF^2 + 1)))

# Exclusion des 11 observations avec ZALTI = 9
# Ces observations apparaissent uniquement en 2016 et 2017,
# pendant la période de transition vers les nouvelles régions administratives.
# Le code 9 est absent du dictionnaire des variables RICA — il s'agit
# probablement d'un marqueur temporaire "zone non déterminée" utilisé
# par Agreste pendant la réforme territoriale.
# Les autres variables de ces 11 exploitations ont des valeurs normales,
# ce n'est donc pas une erreur de données mais un code zone non renseigné.

panel_metro <- panel_metro %>%
  filter(ZALTI %in% c(1, 2, 3))

probit_panel <- glm(
  assure ~ factor(CDEXE) + factor(OTEXE) +
    ratio_dette + autof_ihs +
    calamite_lag +           # sinistre passé (lag groupe)
    factor(NREG) +           # effets fixes région
    factor(ZALTI) +          # zone altimétrique
    factor(annee),           # effets fixes année — nouveauté vs coupe
  data   = panel_metro,
  family = binomial(link = "probit"),
  weights = EXTR2
)

summary(probit_panel)

summary(probit_panel)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  filter(!str_detect(variable, "factor\\(annee\\)|factor\\(NREG\\)|factor\\(OTEXE\\)")) %>%
  arrange(`Pr(>|z|)`) %>%
  knitr::kable(digits = 4)

# Effets marginaux
cat("\nEffets marginaux moyens :\n")
mfx <- margins(probit_panel,
               variables = c("ratio_dette", "autof_ihs", "calamite_lag"))
summary(mfx)

 # ============================================================
# 7. EFFET DE LA RÉFORME — RÉGRESSION DiD SIMPLE
# ============================================================
# Difference-in-Differences simplifié :
# On compare l'évolution du taux d'assurance entre les types de
# production très exposés (céréaliers, OTEXE 15) et peu exposés
# (éleveurs, OTEXE 45-46) avant et après la réforme 2022.
#
# L'idée : si la réforme a surtout bénéficié aux céréaliers
# (qui utilisent les contrats multirisques climatiques éligibles
# à la subvention), on devrait voir leur taux augmenter plus
# que celui des éleveurs après 2022.

cat("\n=== DiD SIMPLIFIÉ — EFFET RÉFORME 2022 ===\n")

panel_did <- panel_metro %>%
  filter(OTEXE %in% c(15, 45, 46)) %>%   # céréaliers vs bovins
  mutate(
    traite     = as.integer(OTEXE == 15), # céréaliers = groupe traité
    post       = as.integer(annee >= 2023),
    traite_post = traite * post
  )

did_probit <- glm(
  assure ~ traite + post + traite_post +
    factor(CDEXE) + ratio_dette + autof_ihs +
    factor(NREG) + factor(ZALTI),
  data   = panel_did,
  family = binomial(link = "probit"),
  weights = EXTR2
)

summary(did_probit)

cat("\nCoefficient d'intérêt (traite_post) :\n")
coef_did <- coef(summary(did_probit))["traite_post", ]
cat(" Coefficient :", round(coef_did["Estimate"], 4), "\n")
cat(" p-value     :", round(coef_did["Pr(>|z|)"], 4), "\n")

cat("\nToutes les figures sont sauvegardées dans figures/\n")
cat("fig1_taux_assurance.png\n")
cat("fig2_taux_otexe.png\n")
cat("fig3_primes.png\n")
cat("fig4_reforme.png\n")

library(kableExtra)

# --- Tableau 1 : coefficients probit ---
tab_probit <- summary(probit_panel)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  filter(!str_detect(variable, 
                     "factor\\(annee\\)|factor\\(NREG\\)|factor\\(OTEXE\\)")) %>%
  arrange(`Pr(>|z|)`) %>%
  rename(
    `p-value`    = `Pr(>|z|)`,
    `Erreur std` = `Std. Error`,
    `z`          = `z value`
  )

tab_probit %>%
  kable(digits = 4, caption = "Probit panel — coefficients (effets fixes OTEXE, NREG, année masqués)") %>%
  kable_styling(full_width = FALSE, font_size = 12,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tableau_probit_panel.png")

# --- Tableau 2 : effets marginaux ---
tab_mfx <- summary(mfx) %>%
  as.data.frame() %>%
  select(factor, AME, SE, z, p, lower, upper) %>%
  rename(
    Variable    = factor,
    `AME`       = AME,
    `Erreur std`= SE,
    `z`         = z,
    `p-value`   = p,
    `IC bas`    = lower,
    `IC haut`   = upper
  )

tab_mfx %>%
  kable(digits = 4, caption = "Effets marginaux moyens (AME)") %>%
  kable_styling(full_width = FALSE, font_size = 12,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tableau_effets_marginaux.png")

# --- Tableau 3 : nombre de groupes ---
tab_groupes <- panel %>%
  group_by(NREG, OTEXE, CDEXE) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  mutate(categorie = case_when(
    n_obs == 1     ~ "1 obs",
    n_obs <= 5     ~ "2-5 obs",
    n_obs <= 10    ~ "6-10 obs",
    n_obs <= 23    ~ "11-23 obs",
    TRUE           ~ "24+ obs (panel complet)"
  )) %>%
  count(categorie) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(categorie)

tab_groupes %>%
  kable(digits = 1,
        col.names = c("Catégorie", "Nb groupes", "% du total"),
        caption = "Distribution des groupes NREG × OTEXE × CDEXE") %>%
  kable_styling(full_width = FALSE, font_size = 12,
                bootstrap_options = c("striped", "hover")) %>%
  save_kable("results/tableau_groupes.png")

cat("3 tableaux sauvegardés.\n")