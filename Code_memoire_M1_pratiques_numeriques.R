# Quatrieme brouillon


# Extensions et données ----

# Library calls

{
  library(tidyverse)
  library(gtsummary)
  theme_gtsummary_language("fr", decimal.mark = ",", big.mark = " ")
  library(labelled)
  library(questionr)
  library(qessmasteR)
  library(tidymodels) # pour les régressions (plus précisément la fonction tidy()) -> en fait a priori pas besoin
  library(kableExtra) # pour tableaux, cf Notes de travail M1 PC
  library(FactoMineR) # pour la réalisation des analyses géométriques (ACM et CAH) (plus précisément, pour la création de l'objet qui contient les résulats de la procédure)
  library(factoextra)
  library(explor)
  library(survey)
  library(gghalves)
  library(RColorBrewer)
  library(ggrepel)
  library(nnet) # apparemment c'est le plus pratique pour les modèles de régression logistique multinomiale (plus que glm())
}

# Chargement des données

base <- readRDS("PC18_avec_labels.RDS")

# "Avec labels" signifie que les modalités de variables par ex en O, 1, sont remplacées par leur nom correspondant)
# La base "PC18_avec_labels.RDS" envoyée par Charles, s'est déjà vu appliquée le script de Plessz pour mettre les labels
# Si les labels sont pas appliquées, il faut executer base <- "base %>% mutate_if(is.labelled, to_character)" + "library(labelled))"


# Recodages ----

## Label de SEXE devient "Genre"

var_label(base$SEXE) <- "Genre"

## Tranches d'âge ----

base <- base |>
  mutate(tranches_age_jeunes = cut(AGE,
    breaks = c(0, 19, 24, 29),
    labels = c("15-19 ans", "20-24 ans", "25-29 ans")
  ))
var_label(base$tranches_age_jeunes) <- "Classe d'âge"


#Groupes âge Donnat 2009

## Recodage de base$AGE en base$groupes_age_donnat
base$groupes_age_donnat <- cut(base$AGE,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(15, 20, 25, 35, 45, 55, 65, 75, 100)
)

## Recodage de base$groupes_age_donnat en base$groupes_age_donnat_rec
base$groupes_age_donnat <- base$groupes_age_donnat %>%
  fct_recode(
    "15-19 ans" = "[15,20)",
    "20-24 ans" = "[20,25)",
    "25-34 ans" = "[25,35)",
    "35-44 ans" = "[35,45)",
    "45-54 ans" = "[45,55)",
    "55-64 ans" = "[55,65)",
    "65-74 ans" = "[65,75)",
    "75 ans et plus" = "[75,100]"
  )

## Univers de pratiques ----
  
## Recodage de base$CLASS_univprat en base$CLASS_univprat_quatre_cat
base$CLASS_univprat_quatre_cat <- base$CLASS_univprat %>%
  fct_recode(
    "Un des autres univers" = "Univers du petit écran",
    "Un des autres univers" = "Univers de la culture patrimoniale",
    "Un des autres univers" = "Univers de l'éclectisme classique"
  )

## Réordonnancement de base$CLASS_univprat_quatre_cat
base$CLASS_univprat_quatre_cat <- base$CLASS_univprat_quatre_cat %>%
  fct_relevel(
    "Univers du bain audiovisuel", "Univers du tout-numérique",
    "Univers de l'éclectisme augmenté", "Un des autres univers"
  )

## PCS ----

## Regroupement des PCS en 6 catégories PCS 2003
## (pour CSTOT les retraités semblent être assimilés à leur ancienne profession [comme chez Coulangeon Repères], et logiquement par exclusion les "" sont dans la catégorie "Autres personnes sans activité professionnelle")

base <- base |>
  mutate(
    CSTOT_reduit = fct_drop(CSTOT), # On enlève les modalités inutilisés
    CSTOT_reduit = fct_collapse(CSTOT_reduit,
      "Agriculteurs exploitants" = c("Agriculteurs sur petite exploitation", "Agriculteurs sur moyenne exploitation", "Agriculteurs sur grande exploitation"),
      "Artisans, commerçants et chefs d'entreprise" = c("20", "Artisans", "Commerçants et assimilés", "Chefs d'entreprise de 10 salariés ou plus"),
      "Cadres et professions intellectuelles supérieures" = c("30", "Professions libérales", "Cadres de la fonction publique", "Professeurs, professions scientifiques", "Professions de l'information, des arts et des spectacles", "Cadres administratifs et commerciaux d'entreprise", "Ingénieurs et cadres techniques d'entreprise"),
      "Professions Intermédiaires" = c("40", "Professeurs des écoles, instituteurs et assimilés", "Professions intermédiaires de la santé et du travail social", "Clergé, religieux", "Professions intermédiaires administratives de la fonction publique", "Professions intermédiaires administratives et commerciales des entreprises", "Techniciens", "Contremaîtres, agents de maîtrise"),
      "Employés" = c("50", "Employés civils et agents de service de la fonction publique", "Policiers et militaires", "Employés administratifs d'entreprise", "Employés de commerce", "Personnels des services directs aux particuliers"),
      "Ouvriers" = c("60", "Ouvriers qualifiés de type industriel", "Ouvriers qualifiés de type artisanal", "Chauffeurs", "Ouvriers qualifiés de la manutention, du magasinage et du transport", "Ouvriers non qualifiés de type industriel", "Ouvriers non qualifiés de type artisanal", "Ouvriers agricoles"),
      "Autres personnes sans activité professionnelle" = ""
    ),
    CSTOT_reduit = fct_relevel(CSTOT_reduit, "Autres personnes sans activité professionnelle", after = Inf)
  )

# Sur le tableau produit, ajouter la note (comme Coulangeon Repères) : "1. Les retraités sont classés dans leur ancienne catégorie d'activité."
### Note : on peut le vérifier avant le recodage, avec la ligne ci-dessous, que les modalités [71 à 86 - voir doc Liste professions niveau 3] qu'on a retiré étaient bien vides
### base |> count(CSTOT, .drop = FALSE) |> print(n = Inf)
### Pour la vérification des niveaux qu'il reste à l'issue du recodage : base |> count(CSTOT) |> print(n = Inf)

## Regroupement de la PCS du père en 6 catégories PCS 2003
## (pour CSTOT les retraités semblent être assimilés à leur ancienne profession [comme chez Coulangeon Repères], et logiquement par exclusion les "" sont dans la catégorie "Autres personnes sans activité professionnelle")
## Puis regroupement en 3 catégories


base <- base |>
  mutate(
    CSTOT_PER = fct_drop(CSTOT_PER), # On enlève les modalités inutilisés
    CSTOT_PER = fct_collapse(CSTOT_PER,
      "Agriculteurs exploitants" = c("Agriculteurs sur petite exploitation", "Agriculteurs sur moyenne exploitation", "Agriculteurs sur grande exploitation"),
      "Artisans, commerçants et chefs d'entreprise" = c("20", "Artisans", "Commerçants et assimilés", "Chefs d'entreprise de 10 salariés ou plus"),
      "Cadres et professions intellectuelles supérieures" = c("30", "Professions libérales", "Cadres de la fonction publique", "Professeurs, professions scientifiques", "Professions de l'information, des arts et des spectacles", "Cadres administratifs et commerciaux d'entreprise", "Ingénieurs et cadres techniques d'entreprise"),
      "Professions Intermédiaires" = c("40", "Professeurs des écoles, instituteurs et assimilés", "Professions intermédiaires de la santé et du travail social", "Clergé, religieux", "Professions intermédiaires administratives de la fonction publique", "Professions intermédiaires administratives et commerciales des entreprises", "Techniciens", "Contremaîtres, agents de maîtrise"),
      "Employés" = c("Employés civils et agents de service de la fonction publique", "Policiers et militaires", "Employés administratifs d'entreprise", "Employés de commerce", "Personnels des services directs aux particuliers"),
      "Ouvriers" = c("60", "Ouvriers qualifiés de type industriel", "Ouvriers qualifiés de type artisanal", "Chauffeurs", "Ouvriers qualifiés de la manutention, du magasinage et du transport", "Ouvriers non qualifiés de type industriel", "Ouvriers non qualifiés de type artisanal", "Ouvriers agricoles"),
      "Non renseigné" = "99"
    )
  )


## Idem pour la PCS de la mère
## Puis regroupement en 3 catégories

base <- base |>
  mutate(
    CSTOT_MER = fct_drop(CSTOT_MER), # On enlève les modalités inutilisés
    CSTOT_MER = fct_collapse(CSTOT_MER,
      "Agriculteurs exploitants" = c("Agriculteurs sur petite exploitation", "Agriculteurs sur moyenne exploitation"),
      "Artisans, commerçants et chefs d'entreprise" = c("20", "Artisans", "Commerçants et assimilés", "Chefs d'entreprise de 10 salariés ou plus"),
      "Cadres et professions intellectuelles supérieures" = c("30", "Professions libérales", "Cadres de la fonction publique", "Professeurs, professions scientifiques", "Professions de l'information, des arts et des spectacles", "Cadres administratifs et commerciaux d'entreprise", "Ingénieurs et cadres techniques d'entreprise"),
      "Professions Intermédiaires" = c("40", "Professeurs des écoles, instituteurs et assimilés", "Professions intermédiaires de la santé et du travail social", "Clergé, religieux", "Professions intermédiaires administratives de la fonction publique", "Professions intermédiaires administratives et commerciales des entreprises", "Techniciens", "Contremaîtres, agents de maîtrise"),
      "Employés" = c("Employés civils et agents de service de la fonction publique", "Policiers et militaires", "Employés administratifs d'entreprise", "Employés de commerce", "Personnels des services directs aux particuliers"),
      "Ouvriers" = c("60", "Ouvriers qualifiés de type industriel", "Ouvriers qualifiés de type artisanal", "Chauffeurs", "Ouvriers qualifiés de la manutention, du magasinage et du transport", "Ouvriers non qualifiés de type industriel", "Ouvriers non qualifiés de type artisanal", "Ouvriers agricoles"),
      "Non renseigné" = "99"
    )
  )



# Création de la variable PCS_menage_des_parents


base <- base |>
  mutate(
    PCS_menage_des_parents = case_when(
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Non renseigné") ~ "Non renseigné",
      # Ménage dominante cadre
      ## A
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante cadre",
      ## B
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante cadre",
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante cadre",
      # Ménage dominante intermédiaire
      ## A
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Employés") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante intermédiaire",
      ## B
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante intermédiaire",
      ## C
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Cadres et professions intellectuelles supérieures") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante intermédiaire",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Cadres et professions intellectuelles supérieures") ~ "Ménage dominante intermédiaire",
      ## D
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante intermédiaire",
      # Ménage dominante employée
      ## A
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Employés") ~ "Ménage dominante employée",
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante employée",
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante employée",
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante employée",
      # B
      (CSTOT_PER == "Professions Intermédiaires") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante employée",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Professions Intermédiaires") ~ "Ménage dominante employée",
      # C
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Employés") ~ "Ménage dominante employée",
      # Ménage dominante petit indépendant
      # A
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante petit indépendant",
      # B
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Employés") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Artisans, commerçants et chefs d'entreprise") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Artisans, commerçants et chefs d'entreprise") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Employés") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Agriculteurs exploitants") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante petit indépendant",
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Agriculteurs exploitants") ~ "Ménage dominante petit indépendant",
      # Ménage dominante ouvriere
      # A
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Employés") ~ "Ménage dominante ouvrière",
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante ouvrière",
      # B
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante ouvrière",
      # Ménage monactifs d'employé ou d'ouvrier => En fait on dispatche dans "Employés" et "Ouvriers"
      # A
      (CSTOT_PER == "Employés") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante employée",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Employés") ~ "Ménage dominante employée",
      # B
      (CSTOT_PER == "Ouvriers") & (CSTOT_MER == "Non renseigné") ~ "Ménage dominante ouvrière",
      (CSTOT_PER == "Non renseigné") & (CSTOT_MER == "Ouvriers") ~ "Ménage dominante ouvrière",
      # Ménage inactif
      .default = "Autre cas" # on cherche à ce que personne ne soit dedans
    )
  )

#base |> filter(CRITAGE == "15-29 ans") |> desc_quali(PCS_menage_des_parents)


# Choix de PCS de l'individu ou de la PCS ménage des parents
base <- base |>
  mutate(
    PCS_construite_selon_statut_etudiant_longue = case_when(
      # Etudiants ou apprentis : on prend la PCS ménage de leur parents
      (SITUA == "Etudiant, élève, en formation ou stagiaire non rémunéré" | SITUA == "Apprenti sous contrat ou stagiaire rémunéré") ~ PCS_menage_des_parents, # PCS_des_parents, # pour 5 personnes sur 177 "G_PCS_MENAGE_" correspond à leur propre PCS ménage et pas à celle de leurs parents
      # Personnes ayant fini leur études (sous-entendu : qui ne sont actuellement pas en étude) :  on prend leur PCS
      (SITUA != "Etudiant, élève, en formation ou stagiaire non rémunéré" & SITUA != "Apprenti sous contrat ou stagiaire rémunéré") ~ CSTOT_reduit,
      # Random qui suit : vit_avec_mere_ou_pere == "Non" & (SITUA != "Etudiant, élève, en formation ou stagiaire non rémunéré", SITUA != "Apprenti sous contrat ou stagiaire rémunéré")
      # Autre
      .default = "en dehors"
    )
  )

# Racourcissement de la PCS_construite_selon_statut_etudiant


## Recodage de PCS_construite_selon_statut_etudiant_longue en base$PCS_construite_selon_statut_etudiant_courte
base$PCS_construite_selon_statut_etudiant_courte <- base$PCS_construite_selon_statut_etudiant_longue %>%
  fct_recode(
    "Indépendants" = "Agriculteurs exploitants",
    "Indépendants" = "Artisans, commerçants et chefs d'entreprise",
    "Cadres et professions intellectuelles supérieures" = "Ménage dominante cadre",
    "Employés" = "Ménage dominante employée",
    "Professions intermédiaires" = "Ménage dominante intermédiaire",
    "Ouvriers" = "Ménage dominante ouvrière",
    "Indépendants" = "Ménage dominante petit indépendant",
    "Professions intermédiaires" = "Professions Intermédiaires"
  )

## Réordonnancement de base$PCS_construite_selon_statut_etudiant_courte
base$PCS_construite_selon_statut_etudiant_courte <- base$PCS_construite_selon_statut_etudiant_courte %>%
  fct_relevel(
    "Ouvriers", "Employés", "Indépendants", "Professions intermédiaires",
    "Cadres et professions intellectuelles supérieures", "Autres personnes sans activité professionnelle",
    "Non renseigné"
  )

#base |> filter(CRITAGE == "15-29 ans") |> desc_quali(PCS_construite_selon_statut_etudiant_courte)


## Recodage de base$PCS_construite_selon_statut_etudiant_courte en base$PCS_avec_exclusion
base$PCS_avec_exclusion <- base$PCS_construite_selon_statut_etudiant_courte %>%
  fct_recode(
    NULL = "Autres personnes sans activité professionnelle",
    NULL = "Non renseigné"
  )
base <- base |> 
  mutate(
    PCS_avec_exclusion = fct_recode(
      PCS_avec_exclusion,
      "Cadres & prof. int. sup." = "Cadres et professions intellectuelles supérieures"))

var_label(base$PCS_avec_exclusion) <- "PCS"

# base |> filter(CRITAGE == "15-29 ans") |> desc_quali(PCS_avec_exclusion)

## Diplôme le plus élevé des parents ----

## Regroupement diplôme père en 3 catégories (voir Gire et Granjon 2012)

base <- base |>
  mutate(
    M19_reduit = as_factor(M19), # les catégories de diplômes étaient codés en nombres
    M19_reduit = fct_na_value_to_level(M19_reduit, level = "Non renseigné"),
    M19_reduit = fct_collapse(M19_reduit,
                              "< Bac" = c("1", "2", "3", "4", "5", "6", "14", "15"),
                              "Bac" = c("7", "8", "9"),
                              "> Bac" = c("10", "11", "12", "13")
    )
  )

## Idem pour diplôme mère

base <- base |>
  mutate(
    M21_reduit = as_factor(M21), # les catégories de diplômes étaient codés en nombres
    M21_reduit = fct_na_value_to_level(M21_reduit, level = "Non renseigné"),
    M21_reduit = fct_collapse(M21_reduit,
                              "< Bac" = c("1", "2", "3", "4", "5", "6", "14", "15"),
                              "Bac" = c("7", "8", "9"),
                              "> Bac" = c("10", "11", "12", "13")
    )
  )

## Création de la variable diplôme parents, pour représenter le CAPITAL CULTUREL FAMILIAL

base <- base |> 
  mutate(diplome_parents = case_when(
    (M19_reduit == "Non renseigné") & (M21_reduit == "Non renseigné") ~ "< Bac", # En realité "Non renseigné" # Il y a seulement 2 personnes
    (M19_reduit == "Non renseigné") ~ M21_reduit,
    (M21_reduit == "Non renseigné") ~ M19_reduit,
    (M19_reduit == "> Bac") |(M21_reduit == "> Bac") ~ "> Bac",
    (M19_reduit == "BAc") |(M21_reduit == "Bac") ~ "Bac",
    .default = "< Bac"
  ),
  diplome_parents = fct_relevel(diplome_parents, "< Bac", "Bac", "> Bac")
  )
var_label(base$diplome_parents) <- "Niveau de diplôme des parents"

### Pour la vérification : base |> filter(CRITAGE == "15-29 ans") |> count(diplome_parents) |> print(n = Inf)

## Pratiques en amateur ----


# Déclarer au moins une pratique artistique au cours des 12 derniers mois

# photo, musique, danse, théâtre, dessin, peinture, écrire fiction

base <- base |>
  mutate(
    pratique_artistique_au_moins_une = if_else(
      A21_musique == "Oui" | A21_romans == "Oui" | A21_peinture == "Oui" | A21_cirque == "Oui" | A21_poterie == "Oui" | A21_theatre == "Oui" | A21_dessin == "Oui" | A21_danse == "Oui", "oui", "non", "non"
    )
  )
var_label(base$pratique_artistique_au_moins_une) <- "Déclarer au moins une pratique artistique pour les 12 derniers mois"

# Utiliser des outils et supports numériques pour des pratiques culturelles en amateurs


base <- base |>
  mutate(
    utiliser_numerique_pour_pratiques_amateur = case_when(
      A21_montages == "Oui" | A243_musique == "oui" | A243_journal == "oui" | A243_romans == "oui" | A243_peinture == "oui" | A243_montages == "oui" | A243_cirque == "oui" | A243_poterie == "oui" | A243_theatre == "oui" | A243_dessin == "oui" | A243_danse == "oui" | A243_photo == "oui" | A243_genealogie == "oui" | A243_activite_scientifique == "oui" | A311 == "oui" | A312 == "oui" | A313 == "oui" ~ "oui",
      #inutile : is.na(A21_montages) | is.na(A243_musique) | is.na(A243_journal) | is.na(A243_romans) | is.na(A243_peinture) | is.na(A243_montages) | is.na(A243_cirque) | is.na(A243_poterie) | is.na(A243_theatre) | is.na(A243_dessin) | is.na(A243_danse) | is.na(A243_photo) | is.na(A243_genealogie) | is.na(A243_activite_scientifique) | is.na(A311) | is.na(A312) | is.na(A313) ~ "au moins 1 NA",
      #inutile : is.na(A21_montages) & is.na(A243_musique) & is.na(A243_journal) & is.na(A243_romans) & is.na(A243_peinture) & is.na(A243_montages) & is.na(A243_cirque) & is.na(A243_poterie) & is.na(A243_theatre) & is.na(A243_dessin) & is.na(A243_danse) & is.na(A243_photo) & is.na(A243_genealogie) & is.na(A243_activite_scientifique) & is.na(A311) & is.na(A312) & is.na(A313) ~ "tout NA",
      .default = "non"),
    utiliser_numerique_pour_pratiques_amateur = fct_relevel(utiliser_numerique_pour_pratiques_amateur, "oui")
  )

#base_jeunes |> desc_quali(utiliser_numerique_pour_pratiques_amateur, pond = base$POND[1:1219])

#base_jeunes |> multi_croise(var_princ = utiliser_numerique_pour_pratiques_amateur, diplome_parents, pond = base$POND[1:1219])

## Recodage de base$utiliser_numerique_pour_pratiques_amateur en base$utiliser_numerique_pour_pratiques_amateur_num
base$utiliser_numerique_pour_pratiques_amateur_num <- base$utiliser_numerique_pour_pratiques_amateur %>%
  fct_recode(
    "100" = "oui",
    "0" = "non"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$utiliser_numerique_pour_pratiques_amateur_num) <- "Utilisation du numérique pour une pratique amateur" # ou "dans le cadre d'une"

## Recodage de base$A21_montages en base$faire_montages
base$faire_montages <- base$A21_montages %>%
  fct_recode(
    "oui" = "Oui",
    "non" = "Non",
    "non" = "NSP",
    "non" = "REF"
  ) %>%
  fct_explicit_na("non")

var_label(base$faire_montages) <- "Faire du montage"

# Renommage (sans les supprimer) des variables de cours sur internet
## Recodage de base$A243_musique en base$musique_cours_internet
base$musique_cours_internet <- base$A243_musique |> 
  fct_explicit_na("non")

## Recodage de base$A243_journal en base$journal_cours_internet
base$journal_cours_internet <- base$A243_journal |> 
  fct_explicit_na("non")

## Recodage de base$A243_romans en base$romans_cours_internet
base$romans_cours_internet <- base$A243_romans |> 
  fct_explicit_na("non")

## Recodage de base$A243_peinture en base$peinture_cours_internet
base$peinture_cours_internet <- base$A243_peinture |> 
  fct_explicit_na("non")

## Recodage de base$A243_montages en base$montages_cours_internet
base$montages_cours_internet <- base$A243_montages |> 
  fct_explicit_na("non")

## Recodage de base$A243_cirque en base$cirque_cours_internet
base$cirque_cours_internet <- base$A243_cirque |> 
  fct_explicit_na("non")

## Recodage de base$A243_poterie en base$poterie_cours_internet
base$poterie_cours_internet <- base$A243_poterie |> 
  fct_explicit_na("non")

## Recodage de base$A243_theatre en base$theatre_cours_internet
base$theatre_cours_internet <- base$A243_theatre |> 
  fct_explicit_na("non")

## Recodage de base$A243_dessin en base$dessin_cours_internet
base$dessin_cours_internet <- base$A243_dessin |> 
  fct_explicit_na("non")

## Recodage de base$A243_danse en base$danse_cours_internet
base$danse_cours_internet <- base$A243_danse |> 
  fct_explicit_na("non")

## Recodage de base$A243_photo en base$photo_cours_internet
base$photo_cours_internet <- base$A243_photo |> 
  fct_explicit_na("non")

## Recodage de base$A243_genealogie en base$genealogie_cours_internet
base$genealogie_cours_internet <- base$A243_genealogie |> 
  fct_explicit_na("non")

## Recodage de base$A243_activite_scientifique en base$activite_scientifique_cours_internet
base$activite_scientifique_cours_internet <- base$A243_activite_scientifique |> 
  fct_explicit_na("non")

# Création de suivre au moins un cours en amateur sur internet
base <- base |>
  mutate(suivre_au_moins_un_cours_internet = if_else(
    base$musique_cours_internet == "oui" | base$journal_cours_internet == "oui" | base$romans_cours_internet == "oui" | base$peinture_cours_internet == "oui" | base$montages_cours_internet == "oui" | base$cirque_cours_internet == "oui" | base$poterie_cours_internet == "oui" | base$theatre_cours_internet == "oui" | base$dessin_cours_internet == "oui" | base$danse_cours_internet == "oui" | base$photo_cours_internet == "oui" | base$genealogie_cours_internet == "oui" | base$activite_scientifique_cours_internet == "oui", "oui", "non", missing = "non"
  ))
var_label(base$suivre_au_moins_un_cours_internet) <- "Suivre au moins un cours sur internet dans le cadre d’une pratique amateur"

## Recodage de base$A311 en base$utiliser_numerique_pour_apprendre_en_amateur
base$utiliser_numerique_pour_apprendre_en_amateur <- base$A311 |> 
  fct_explicit_na("non")
var_label(base$utiliser_numerique_pour_apprendre_en_amateur) <- "Utiliser le numérique pour apprendre dans le cadre d’une pratique amateur"

## Recodage de base$A312 en base$utiliser_numerique_pour_creer_en_amateur
base$utiliser_numerique_pour_creer_en_amateur <- base$A312 |> 
  fct_explicit_na("non")
var_label(base$utiliser_numerique_pour_creer_en_amateur) <- "Utiliser le numérique pour créer dans le cadre d'une pratique en amateur"

## Recodage de base$A313 en base$utiliser_numerique_pour_diffuser_en_amateur
base$utiliser_numerique_pour_diffuser_en_amateur <- base$A313 |> 
  fct_explicit_na("non")
var_label(base$utiliser_numerique_pour_diffuser_en_amateur) <- "Utiliser le numérique pour diffuser dans le cadre d’une pratique amateur"


# Création de la variable utiliser_numerique_pour_creer_ou_faire_montages

base <- base |> 
  mutate(
    utiliser_numerique_pour_creer_ou_faire_montages = if_else(
      utiliser_numerique_pour_creer_en_amateur == "oui" | faire_montages == "oui", "oui", "non", "non"
    )
  )
var_label(base$utiliser_numerique_pour_creer_ou_faire_montages) <- "Faire du montage ou créer avec le numérique dans le cadre d'une pratique amateur"


## Jeux-vidéos ----

# Jouer aux jeux-vidéos
## Recodage de base$B2 en base$jouer_jeux_videos_frequence
base$jouer_jeux_videos_frequence <- base$B2 %>%
  fct_recode(
    "Quotidiennement" = "Tous les jours ou presque",
    "Occasionnellement" = "Environ 3 ou 4 jours par semaine",
    "Occasionnellement" = "Environ 1 ou 2 jours par semaine",
    "Occasionnellement" = "Environ 1 à 3 jours par mois",
    "Occasionnellement" = "Plus rarement",
    "Jamais" = "Jamais ou pratiquement jamais",
    "Jamais" = "NSP",
    "Jamais" = "REF"
  ) %>%
  fct_explicit_na("Jamais")

#freq(base$jouer_jeux_videos_frequence)


## Recodage de base$B2 en base$jouer_jeux_videos_au_moins_hebdo
base$jouer_jeux_videos_au_moins_hebdo <- base$B2 %>%
  fct_recode(
    "oui" = "Tous les jours ou presque",
    "oui" = "Environ 3 ou 4 jours par semaine",
    "oui" = "Environ 1 ou 2 jours par semaine",
    "non" = "Environ 1 à 3 jours par mois",
    "non" = "Plus rarement",
    "non" = "Jamais ou pratiquement jamais",
    "non" = "NSP",
    "non" = "REF"
  ) %>%
  fct_explicit_na("non")

## Recodage de base$jouer_jeux_videos_au_moins_hebdo en base$jouer_jeux_videos_au_moins_hebdo_num
base$jouer_jeux_videos_au_moins_hebdo_num <- base$jouer_jeux_videos_au_moins_hebdo %>%
  fct_recode(
    "100" = "oui",
    "0" = "non"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$jouer_jeux_videos_au_moins_hebdo_num) <- "Pratique des jeux-vidéos"


## Recodage de base$B3 en base$jouer_jeux_en_ligne_au_moins_temps_en_temps
base$jouer_jeux_en_ligne_au_moins_temps_en_temps <- base$B3 %>%
  fct_recode(
    "oui" = "Oui, souvent",
    "oui" = "Oui, de temps en temps",
    "non" = "Oui, rarement",
    "non" = "Non, jamais",
    "non" = "NSP",
    "non" = "REF"
  ) %>%
  fct_explicit_na("non")
var_label(base$jouer_jeux_en_ligne_au_moins_temps_en_temps) <- "Jouer à des jeux en ligne"

## Films, séries, émissions ----

# Visionnage télévision
## Recodage de base$C1 en base$regarder_television_au_moins_hebdo
base$regarder_television_au_moins_hebdo <- base$C1 %>%
  fct_recode(
    "oui" = "Tous les jours ou presque",
    "oui" = "Environ 3 ou 4 jours par semaine",
    "oui" = "Environ 1 ou 2 jours par semaine",
    "non" = "Plus rarement",
    "non" = "Jamais ou pratiquement jamais",
    "non" = "NSP",
    "non" = "REF"
  )


## Recodage de base$C15 en base$regarder_videos_en_ligne_au_moins_hebdo
base$regarder_videos_en_ligne_au_moins_hebdo <- base$C15 %>%
  fct_recode(
    "oui" = "Tous les jours ou presque",
    "oui" = "Environ 3 ou 4 jours par semaine",
    "oui" = "Environ 1 ou 2 jours par semaine",
    "non" = "Plus rarement",
    "non" = "Jamais ou pratiquement jamais",
    "non" = "NSP",
    "non" = "REF"
  )

## Recodage de base$regarder_videos_en_ligne_au_moins_hebdo en base$regarder_videos_en_ligne_au_moins_hebdo_num
base$regarder_videos_en_ligne_au_moins_hebdo_num <- base$regarder_videos_en_ligne_au_moins_hebdo %>%
  fct_recode(
    "100" = "oui",
    "0" = "non"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$regarder_videos_en_ligne_au_moins_hebdo_num) <- "Visionnage de vidéos en ligne"

#base_jeunes |> multi_croise(var_princ = regarder_videos_en_ligne_au_moins_hebdo, tranches_age_jeunes, pond = base$POND[1:1219])

## Recodage de base$C15 en base$regarder_videos_en_ligne_frequence
base$regarder_videos_en_ligne_frequence <- base$C15 %>%
  fct_recode(
    "3-4 jours / semaine" = "Environ 3 ou 4 jours par semaine",
    "1-2 jours / semaine" = "Environ 1 ou 2 jours par semaine",
    "Jamais" = "Jamais ou pratiquement jamais",
    "Jamais" = "NSP",
    "Jamais" = "REF"
  )


# Création de la variable utiliser un équipement numérique (télé, ordi, tablette) pour regarder télé
base <- base |> 
  mutate(
    utiliser_equipement_numerique_pour_regarder_tele = if_else(
      C62 == "oui" | C63 == "oui" | C64 == "oui", "oui", "non", missing = "non")
    )
var_label(base$utiliser_equipement_numerique_pour_regarder_tele) <- "Utiliser un équipement numérique pour regarder la télévision"

# Création de la variable utiliser un équipement numérique (télé, ordi, tablette) pour regarder des films
base <- base |> 
  mutate(
    utiliser_equipement_numerique_pour_regarder_films = if_else(
      C212 == "oui" | C213 == "oui" | C214 == "oui", "oui", "non", missing = "non")
  )
var_label(base$utiliser_equipement_numerique_pour_regarder_films) <- "Utiliser un équipement numérique pour regarder des films"

# Création de la variable utiliser un équipement numérique (télé, ordi, tablette) pour regarder des séries
base <- base |> 
  mutate(
    utiliser_equipement_numerique_pour_regarder_series = if_else(
      C332 == "oui" | C333 == "oui" | C334 == "oui", "oui", "non", missing = "non")
  )
var_label(base$utiliser_equipement_numerique_pour_regarder_series) <- "Utiliser un équipement numérique pour regarder des séries"

# Création de la variable regarder télé, films ou séries sur support numérique

base <- base |> 
  mutate(
    regarder_tele_films_ou_series_sur_support_numerique = if_else(
      utiliser_equipement_numerique_pour_regarder_tele == "oui" | utiliser_equipement_numerique_pour_regarder_films == "oui" | utiliser_equipement_numerique_pour_regarder_series == "oui", "oui", "non", "non"
    )
  )

var_label(base$regarder_tele_films_ou_series_sur_support_numerique) <- "Utiliser un équipement numérique pour regarder la télé, des films ou des séries"

# Ci-dessous se trouvent les variables pour l'ACM

## Recodage de base$C1701 en base$regarder_sur_internet_films
base$regarder_sur_internet_films <- base$C1701 %>%
  fct_explicit_na("non")

## Recodage de base$C1702 en base$regarder_sur_internet_series
base$regarder_sur_internet_series <- base$C1702 %>%
  fct_explicit_na("non")

## Recodage de base$C1703 en base$regarder_sur_internet_bande_annonce
base$regarder_sur_internet_bande_annonce <- base$C1703 %>%
  fct_explicit_na("non")

## Recodage de base$C1704 en base$regarder_sur_internet_clips
base$regarder_sur_internet_clips <- base$C1704 %>%
  fct_explicit_na("non")

## Recodage de base$C1705 en base$regarder_sur_internet_infos
base$regarder_sur_internet_infos <- base$C1705 %>%
  fct_explicit_na("non")

## Recodage de base$C1706 en base$regarder_sur_internet_sport
base$regarder_sur_internet_sport <- base$C1706 %>%
  fct_explicit_na("non")

## Recodage de base$C1707 en base$regarder_sur_internet_youtubeurs
base$regarder_sur_internet_youtubeurs <- base$C1707 %>%
  fct_explicit_na("non")

## Recodage de base$C1708 en base$regarder_sur_internet_enfants
base$regarder_sur_internet_enfants <- base$C1708 %>%
  fct_explicit_na("non")

## Recodage de base$C1709 en base$regarder_sur_internet_reportages
base$regarder_sur_internet_reportages <- base$C1709 %>%
  fct_explicit_na("non")

## Recodage de base$C1710 en base$regarder_sur_internet_jeux
base$regarder_sur_internet_jeux <- base$C1710 %>%
  fct_explicit_na("non")

## Recodage de base$C1711 en base$regarder_sur_internet_autre
base$regarder_sur_internet_autre <- base$C1711 %>%
  fct_explicit_na("non")

## Recodage de base$C18 en base$videos_manqueraient
base$videos_manqueraient <- base$C18 %>%
  fct_recode(
    "Non, pas tellement" = "NSP",
    "Pas concerné" = "REF"
  ) %>%
  fct_explicit_na("Pas concerné")

# Le thème unificateur est : visionner du contenu en ligne
## Recodage de base$C161 en base$utiliser_pour_videos_internet_tele
base$utiliser_pour_videos_internet_tele <- base$C161 %>%
  fct_explicit_na("pas_concerné")
#freq(base$utiliser_pour_videos_internet_tele)

## Recodage de base$C162 en base$utiliser_pour_videos_internet_ordi
base$utiliser_pour_videos_internet_ordi <- base$C162 %>%
  fct_explicit_na("pas_concerné")
#freq(base$utiliser_pour_videos_internet_ordi)

## Recodage de base$C163 en base$utiliser_pour_videos_internet_tablette
base$utiliser_pour_videos_internet_tablette <- base$C163 %>%
  fct_explicit_na("pas_concerné")
#freq(base$utiliser_pour_videos_internet_tablette)

## Recodage de base$C164 en base$utiliser_pour_videos_internet_smartphone
base$utiliser_pour_videos_internet_smartphone <- base$C164 %>%
  fct_explicit_na("pas_concerné")
#freq(base$utiliser_pour_videos_internet_smartphone)

## Recodage de base$C165 en base$utiliser_pour_videos_internet_projecteur
base$utiliser_pour_videos_internet_projecteur <- base$C165 %>%
  fct_explicit_na("pas_concerné")
#freq(base$utiliser_pour_videos_internet_projecteur)



## Information ----

# Consulter la presse numérique

## Recodage de base$D34 en base$consulter_presse_numerique
base$consulter_presse_numerique <- base$D34 %>%
  fct_explicit_na("non")

#base_jeunes |> desc_quali(consulter_presse_numerique, pond = base$POND[1:1219])
#base_jeunes |> multi_croise(var_princ = consulter_presse_numerique, tranches_age_jeunes, pond = base$POND[1:1219])

## Recodage de base$consulter_presse_numerique en base$consulter_presse_numerique_num
base$consulter_presse_numerique_num <- base$consulter_presse_numerique %>%
  fct_recode(
    "0" = "non",
    "100" = "oui"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$consulter_presse_numerique_num) <- "Lecture de la presse numérique"

# Création de la variable utiliser le numérique pour se tenir informé 
base <- base |> 
  mutate(
    utiliser_numerique_pour_se_tenir_informe = if_else(
      D34 == "oui" | D35 == "oui" | D36 == "oui" | D37 == "oui", "oui", "non", missing = "non")
  )
var_label(base$utiliser_numerique_pour_se_tenir_informe) <- "Utiliser le numérique pour se tenir informé"


## Ecoute de musique et d'émissions ----


# Ecoute de la radio

## Recodage de base$E1 en base$ecoute_radio_au_moins_hebdo
base$ecoute_radio_au_moins_hebdo <- base$E1 %>%
  fct_recode(
    "oui" = "Tous les jours ou presque",
    "oui" = "Environ 3 ou 4 jours par semaine",
    "oui" = "Environ 1 ou 2 jours par semaine",
    "non" = "Plus rarement",
    "non" = "Jamais ou pratiquement jamais",
    "non" = "NSP",
    "non" = "REF"
  )

# Ecouter programmes radiophoniques en podcast/streaming dans les 12 derniers mois

## Recodage de base$E6 en base$programme_radio_podcast_streaming
base$programme_radio_podcast_streaming <- base$E6 %>%
  fct_recode(
    "oui" = "Oui",
    "non" = "Non",
    "non" = "NSP",
    "non" = "REF"
  ) %>%
  fct_explicit_na("non")

## Recodage de base$programme_radio_podcast_streaming en base$programme_radio_podcast_streaming_num
base$programme_radio_podcast_streaming_num <- base$programme_radio_podcast_streaming %>%
  fct_recode(
    "100" = "oui",
    "0" = "non"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$programme_radio_podcast_streaming_num) <- "Écoute de podcast radio"


# Ecouter de la musique sur support numérique (streaming ou fichier numérique hors CD)

base <- base |>
  mutate(
    ecoute_musique_support_numerique_au_moins_hebdo = if_else(
      (E7 == "Oui, tous les jours ou presque" | E7 == "Oui, environ 3 ou 4 jours par semaine" | E7 == "Oui, environ 1 ou 2 jours par semaine") &
        (E83 == "oui" | E84 == "oui" | E85 == "oui"), "oui", "non"
    )
  )
var_label(base$ecoute_musique_support_numerique_au_moins_hebdo) <- "Écoute de musique sur support numérique"

#base_jeunes |> desc_quali(ecoute_musique_support_numerique_au_moins_hebdo, pond = base$POND[1:1219])
#base_jeunes |> multi_croise(var_princ = ecoute_musique_support_numerique_au_moins_hebdo, diplome_parents, pond = base$POND[1:1219])


## Recodage de base$ecoute_musique_support_numerique_au_moins_hebdo en base$ecoute_musique_support_numerique_au_moins_hebdo_num
base$ecoute_musique_support_numerique_au_moins_hebdo_num <- base$ecoute_musique_support_numerique_au_moins_hebdo %>%
  fct_recode(
    "0" = "non",
    "100" = "oui"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$ecoute_musique_support_numerique_au_moins_hebdo_num) <- "Écoute de musique sur support numérique"


# Ecouter de la musique sur support numérique fréquence

base <- base |>
  mutate(
    ecoute_musique_support_numerique_frequence = case_when(
      !(E83 == "oui" | E84 == "oui" | E85 == "oui") ~ "Rarement ou jamais",
      E7 == "Oui, tous les jours ou presque" ~ "Quotidiennement",
      E7 == "Oui, environ 3 ou 4 jours par semaine" | E7 == "Oui, environ 1 ou 2 jours par semaine" ~ "Occasionnellement",
      .default = "Rarement ou jamais"
    )
  )

## Réordonnancement de base$ecoute_musique_support_numerique_frequence
base$ecoute_musique_support_numerique_frequence <- base$ecoute_musique_support_numerique_frequence %>%
  fct_relevel(
    "Quotidiennement", "Occasionnellement", "Rarement ou jamais"
  )


## Bibliothèques et livres ----


# Renommage F13_C_1 (sans la supprimer) en nb_livres_lu_num
base <- base |> 
  mutate(
    nb_livres_lu_num = I105
  )

# Nombre de livres lus
## Recodage de base$F13_C_1 en base$nb_livres_lus
base$nb_livres_lus <- cut(base$F13_C_1,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 1, 10, 20, 663)
  ) %>%
  fct_recode(
    "Aucun" = "[0,1)",
    "1 à 9" = "[1,10)",
    "10 à 19" = "[10,20)",
    "20 et plus" = "[20,663]"
  )

## Avoir lu au moins un livre
## Recodage de base$F13_C_1 en base$avoir_lu_au_moins_un_livre
base$avoir_lu_au_moins_un_livre <- cut(base$F13_C_1,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 1, 10, 20, 663)
  ) %>%
  fct_recode(
    "non" = "[0,1)",
    "oui" = "[1,10)",
    "oui" = "[10,20)",
    "oui" = "[20,663]"
  )

## Avoir lu au moins quatre livres
## Recodage de base$F13_C_1 en base$avoir_lu_au_moins_quatre_livre
base$avoir_lu_au_moins_quatre_livre <- cut(base$F13_C_1,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 4, 10, 20, 663)
  ) %>%
  fct_recode(
    "non" = "[0,4)",
    "oui" = "[4,10)",
    "oui" = "[10,20)",
    "oui" = "[20,663]"
  )
var_label(base$avoir_lu_au_moins_quatre_livre) <- "Avoir lu au moins quatre livres"

## Recodage de base$F123 en base$utiliser_tablette_pour_lire
base$utiliser_tablette_pour_lire <- base$F123 %>%
  fct_explicit_na("non")
var_label(base$utiliser_tablette_pour_lire) <- "Utiliser une tablette pour lire"

## Concerts, cinéma, théâtre, danse et festivals ----


# Être allé à un concert au cours des 12 derniers mois

base <- base |> 
  mutate(
    concert_au_moins_un = 
      if_else(G2501 == "oui" | G2502 == "oui" | G2503 == "oui" | G2504 == "oui" | G2505 == "oui" | G2506 == "oui" | G2507 == "oui" | G2508 == "oui" | G2509 == "oui" | G2510 == "oui" | G2511 == "oui" | G2512 == "oui", "oui", "non", "non")
  )
var_label(base$concert_au_moins_un) <- "Être allé à au moins un concert"



# Nombre de sorties au cinéma catégorielle
base <- base |> mutate(
  nb_sorties_cinema_cat = case_when(
    G5 == "Par semaine" | G5 == "Par mois" | (G5 == "Par an" & G4_C_1 >= 12) ~ "12 fois et plus",
    G5 == "Par an" & G4_C_1 >= 3 ~ "3 à 11 fois",
    G5 == "Par an" & G4_C_1 >= 1 ~ "1 à 2 fois",
    .default = "Jamais"
  )
)

# Nombre de sorties au cinéma numérique
base <- base |> 
  mutate(
    nb_sorties_cinema_num = case_when(
      is.na(G4_C_1) ~ 0,
      is.na(G5) ~ 0,
      G5 == "Par semaine" ~ G4_C_1 * 52,
      G5 == "Par mois" ~ G4_C_1 * 12,
      G5 == "Par an" ~ G4_C_1
    )
  )

# Être allé au moins 5 fois au cinéma
## Recodage de base$nb_sorties_cinema_num en base$sorties_cinema_au_moins_cinq
base$sorties_cinema_au_moins_cinq <- cut(base$nb_sorties_cinema_num,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 5, 300)
) %>%
  fct_recode(
    "non" = "[0,5)",
    "oui" = "[5,300]"
  )
var_label(base$sorties_cinema_au_moins_cinq) <- "Être allé au moins cinq fois au cinéma"


# Être allé à un spectacle vivant au cours des 12 derniers mois

base <- base |> 
  mutate(
    spectacle_vivant_au_moins_un = 
      if_else(G131 == "oui" | G132 == "oui" | G134 == "oui", "oui", "non", "non")
  )
var_label(base$spectacle_vivant_au_moins_un) <- "Être allé à au moins un spectacle vivant"


# Être allé à un festival au cours des 12 derniers mois

## Recodage de base$G32 en base$assister_festival
base$assister_festival <- base$G32 %>%
  fct_recode(
    "oui" = "Oui",
    "non" = "Non",
    "non" = "NSP",
    "non" = "REF"
  )

## Musées, expositions et patrimoine ----

# Avoir fait au moins une visite culturelle au cours des 12 derniers mois

base <- base |> 
  mutate(
    visite_culturelle_au_moins_une = 
      if_else(H202 == "oui" | H206 == "oui" | H208 == "oui" | H209 == "oui" | H210 == "oui" | H211 == "oui", "oui", "non", "non")
  )
var_label(base$visite_culturelle_au_moins_une) <- "Avoir fait au moins une visite culturelle (musée, exposition, etc.)"

## Equipements et internet ----

# Renommage I105 (sans la supprimer) en equipement_foyer_console_fixe
base <- base |> 
  mutate(
    equipement_foyer_console_fixe = I105
  )

# Renommage I106 (sans la supprimer) en equipement_foyer_console_portable
base <- base |> 
  mutate(
    equipement_foyer_console_portable = I106
  )

# Création variable équipement console fixe ou portable
base <- base |> 
  mutate(
    equipement_foyer_console_fixe_ou_portable = ifelse(I105 == "oui" | I106 == "oui", "oui", "non")
  )

# Renommage I108 (sans la supprimer) en equipement_foyer_ordinateur_fixe
base <- base |> 
  mutate(
    equipement_foyer_ordinateur_fixe = I108
  )

# Renommage I109 (sans la supprimer) en equipement_foyer_ordinateur_portable
base <- base |> 
  mutate(
    equipement_foyer_ordinateur_portable = I109
  )

# Création variable équipement ordinateur fixe ou portable
base <- base |> 
  mutate(
    equipement_foyer_ordinateur_fixe_ou_portable = ifelse(I108 == "oui" | I109 == "oui", "oui", "non")
  )

# Renommage I110 (sans la supprimer) en equipement_foyer_tablette
base <- base |> 
  mutate(
    equipement_foyer_tablette = I110
  )

# Renommage I111 (sans la supprimer) en equipement_foyer_telephone_portable
base <- base |> 
  mutate(
    equipement_foyer_telephone_portable = I111
  )

# Renommage I4 (sans la supprimer) en frequence_connexion_internet
base <- base |> 
  mutate(
    frequence_connexion_internet = I4
  )

## Recodage de base$I6 en base$utiliser_reseaux_sociaux_au_moins_hebdo
base$utiliser_reseaux_sociaux_au_moins_hebdo <- base$I6 %>%
  fct_recode(
    "oui" = "Tous les jours ou presque",
    "oui" = "Plusieurs fois par semaine",
    "oui" = "Environ 1 fois par semaine",
    "non" = "Plus rarement",
    "non" = "Jamais ou pratiquement jamais"
  ) %>%
  fct_explicit_na("non")

## Recodage de base$utiliser_reseaux_sociaux_au_moins_hebdo en base$utiliser_reseaux_sociaux_au_moins_hebdo_num
base$utiliser_reseaux_sociaux_au_moins_hebdo_num <- base$utiliser_reseaux_sociaux_au_moins_hebdo %>%
  fct_recode(
    "100" = "oui",
    "0" = "non"
  ) %>%
  as.character() %>%
  as.numeric()

var_label(base$utiliser_reseaux_sociaux_au_moins_hebdo_num) <- "Utilisation des réseaux sociaux"

## Recodage de base$I6 en base$frequence_reseaux_sociaux
base$frequence_reseaux_sociaux <- base$I6 %>%
  fct_recode(
    "Quotidiennement" = "Tous les jours ou presque",
    "Occasionnellement" = "Plusieurs fois par semaine",
    "Occasionnellement" = "Environ 1 fois par semaine",
    "Occasionnellement" = "Plus rarement",
    "Jamais" = "Jamais ou pratiquement jamais"
  ) %>%
  fct_explicit_na("Jamais")


# Création de la variable participer à une manifestation culturelle en ligne (expo, musée, concert, théâtre, danse)
base <- base |> 
  mutate(
    participer_manifestation_culturelle_en_ligne = if_else(
      I72 == "oui" | I73 == "oui" | I74 == "oui" | I75 == "oui", "oui", "non", missing = "non")
  )
var_label(base$participer_manifestation_culturelle_en_ligne) <- "Avoir participé récemment à une manifestation culturelle en ligne"

## Taille unité urbaine ----

## Recodage de base$TUU2016 en base$commune_residence
base$commune_residence <- base$TUU2016 %>%
  fct_recode(
    "Petites villes" = "Unité urbaine de moins de 5 000 habitants",
    "Petites villes" = "Unité urbaine de 5 000 à 9 999 habitants",
    "Villes moyennes" = "Unité urbaine de 10 000 à 19 999 habitants",
    "Villes moyennes" = "Unité urbaine de 20 000 à 49 999 habitants",
    "Villes moyennes" = "Unité urbaine de 50 000 à 99 999 habitants",
    "Grandes villes et métropoles" = "Unité urbaine de 100 000 à 199 999 habitants",
    "Grandes villes et métropoles" = "Unité urbaine de 200 000 à 1 999 999 habitants",
    "Région parisienne" = "Unité urbaine de Paris"
  )
var_label(base$commune_residence) <- "Taille de la ville de résidence"


## Recodage de base$TUU2016 en base$commune_residence_trois_cat
base$commune_residence_trois_cat <- base$TUU2016 %>%
  fct_recode(
    "Commune rurale et petites villes" = "Commune rurale",
    "Commune rurale et petites villes" = "Unité urbaine de moins de 5 000 habitants",
    "Commune rurale et petites villes" = "Unité urbaine de 5 000 à 9 999 habitants",
    "Commune rurale et petites villes" = "Unité urbaine de 10 000 à 19 999 habitants",
    "Villes moyennes et grandes" = "Unité urbaine de 20 000 à 49 999 habitants",
    "Villes moyennes et grandes" = "Unité urbaine de 50 000 à 99 999 habitants",
    "Villes moyennes et grandes" = "Unité urbaine de 100 000 à 199 999 habitants",
    "Métropoles et région parisienne" = "Unité urbaine de 200 000 à 1 999 999 habitants",
    "Métropoles et région parisienne" = "Unité urbaine de Paris"
  )
var_label(base$commune_residence_trois_cat) <- "Taille de la ville de résidence"


## Indice du nombre d'usages du numerique ----

base <- base |> 
  mutate(
    indice_nombre_usages_numerique = 
      if_else(faire_montages == "oui", 1, 0, 0) +
      if_else(suivre_au_moins_un_cours_internet == "oui", 1, 0, 0) +
      if_else(utiliser_numerique_pour_apprendre_en_amateur == "oui", 1, 0, 0) +
      if_else(utiliser_numerique_pour_creer_en_amateur == "oui", 1, 0, 0) +
      if_else(utiliser_numerique_pour_diffuser_en_amateur == "oui", 1, 0, 0) +
      if_else(jouer_jeux_videos_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(jouer_jeux_en_ligne_au_moins_temps_en_temps == "oui", 1, 0, 0) +
      if_else(utiliser_equipement_numerique_pour_regarder_tele == "oui", 1, 0, 0) +
      if_else(regarder_videos_en_ligne_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(utiliser_equipement_numerique_pour_regarder_films == "oui", 1, 0, 0) +
      if_else(utiliser_equipement_numerique_pour_regarder_series == "oui", 1, 0, 0) +
      if_else(utiliser_numerique_pour_se_tenir_informe == "oui", 1, 0, 0) +
      if_else(programme_radio_podcast_streaming == "oui", 1, 0, 0) +
      if_else(ecoute_musique_support_numerique_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(utiliser_tablette_pour_lire == "oui", 1, 0, 0) +
      if_else(utiliser_reseaux_sociaux_au_moins_hebdo == "Oui", 1, 0, 0) +
      if_else(participer_manifestation_culturelle_en_ligne == "oui", 1, 0, 0)
  )

## Recodage de base_jeunes$indice_nombre_usages_numerique en base_jeunes$indice_nombre_usages_numerique_cat
base$indice_nombre_usages_numerique_cat <- cut(base$indice_nombre_usages_numerique,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 4, 7, 11, 15)
)


## Recodage de base$indice_nombre_usages_numerique_cat en base$indice_nombre_usages_numerique_cat
base$indice_nombre_usages_numerique_cat <- base$indice_nombre_usages_numerique_cat %>%
  fct_recode(
    "Usage limité (0-3)" = "[0,4)",
    "Usage modéré (4-6)" = "[4,7)",
    "Usage diversifié (7-10)" = "[7,11)",
    "Usage très diversifié (11-15)" = "[11,15]"
  )


## Calcul du poids relatif des pratiques culturelles numériques dans l'ensemble des pratiques culturelles ----


# Score numérique

base <- base |> 
  mutate(
    score_pratiques_culturelles_numeriques = 1/7 * (
      if_else(ecoute_musique_support_numerique_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(jouer_jeux_videos_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(regarder_videos_en_ligne_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(utiliser_reseaux_sociaux_au_moins_hebdo == "oui", 1, 0, 0) +
      if_else(regarder_tele_films_ou_series_sur_support_numerique == "oui", 1, 0, 0) +
      if_else(utiliser_numerique_pour_creer_ou_faire_montages == "oui", 1, 0, 0) +
      if_else(participer_manifestation_culturelle_en_ligne == "oui", 1, 0, 0)
      )
  )

base <- base |>
  mutate(
    score_pratiques_culturelles_non_numeriques = 1 / 9 * (
        if_else(regarder_television_au_moins_hebdo == "oui", 1, 0, 0) +
        if_else(ecoute_radio_au_moins_hebdo == "oui", 1, 0, 0) +
        if_else(avoir_lu_au_moins_quatre_livre == "oui", 1, 0, 0) +
        if_else(sorties_cinema_au_moins_cinq == "oui", 1, 0, 0) +
        if_else(concert_au_moins_un == "oui", 1, 0, 0) +
        if_else(spectacle_vivant_au_moins_un == "oui", 1, 0, 0) +
        if_else(assister_festival == "oui", 1, 0, 0) +
        if_else(visite_culturelle_au_moins_une == "oui", 1, 0, 0) +
        if_else(pratique_artistique_au_moins_une == "oui", 1, 0, 0)
    )
  )

base <- base |>
  mutate(
    poids_relatif_numerique_dans_lensemble_des_pratiques = if_else(
      score_pratiques_culturelles_numeriques == 0 & score_pratiques_culturelles_non_numeriques == 0, NA, score_pratiques_culturelles_numeriques / (score_pratiques_culturelles_numeriques + score_pratiques_culturelles_non_numeriques)
    )
  )


## Recodage de base$score_pratiques_culturelles_non_numeriques en base$score_pratiques_culturelles_non_numeriques_cat
base$score_pratiques_culturelles_non_numeriques_cat <- cut(base$score_pratiques_culturelles_non_numeriques,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0, 0.444444444444444, 0.555555555555556, 1)
)

## Recodage de base$score_pratiques_culturelles_non_numeriques_cat en base$score_pratiques_culturelles_non_numeriques_cat
base$score_pratiques_culturelles_non_numeriques_cat <- base$score_pratiques_culturelles_non_numeriques_cat %>%
  fct_recode(
    "Peu de pratique non-numériques" = "[0,0.4444)",
    "Niveau moyen de pratiques non-numériques" = "[0.4444,0.5556)",
    "Beaucoup de pratiques non-numériques" = "[0.5556,1]"
  )
var_label(base$score_pratiques_culturelles_non_numeriques_cat) <- "Niveau de pratiques culturelles non numériques"


## Base pondérée et sous-base à utiliser je pense ----

base_jeunes <- base |> 
  filter(CRITAGE == "15-29 ans", !is.na(PCS_avec_exclusion)) 

basep <- svydesign(ids = ~ 1, weights = ~ POND, data = base)

base_jeunesp <- subset(basep, CRITAGE == "15-29 ans" & !is.na(PCS_avec_exclusion))
base_jeunesp$variables <- copy_labels(from = basep$variables, to = base_jeunesp$variables) # Avec subset() les labels ont été perdus donc on les reprend de basep

# Stats descr ----

## Tableau 1 - description de la base jeunes par genre, âge, capital culturel familial et pcs ----

# Effectifs
tdesc1 <- base_jeunesp |>
  tbl_svysummary(
    include = c("SEXE", "tranches_age_jeunes", "PCS_avec_exclusion", "diplome_parents"),
    statistic = list(
      all_categorical() ~ "{n_unweighted}"
    ),
    digits = list(
      all_categorical() ~ c(0, 1)
    ),
  )

# Proportion
tdesc2 <- base_jeunesp |>
  tbl_svysummary(
    include = c("SEXE", "tranches_age_jeunes", "PCS_avec_exclusion", "diplome_parents"),
    statistic = list(
      all_categorical() ~ "{p} %"
    ),
    digits = list(
      all_categorical() ~ c(1)
    ),
  )

tab_desc_fusion <- tbl_merge(list(tdesc1, tdesc2), tab_spanner = FALSE) |> 
  bold_labels() |> 
  modify_footnote(everything() ~ NA) |> 
  as_kable_extra(col.names = c("", "Effectif", "Proportion"), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Effectifs bruts, pourcentages pondérés.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 49,8% des individus de la population étudiée sont des femmes, soit 672 personnes."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )
  

#rm(tdesc1, tdesc2)


## Graphique 1 - taux d'équipement ----


# Fonction pour calculer le pourcentage de réponses 'oui' pour une colonne donnée
calculer_pourcentage_oui <- function(data, nom_colonne, nom_appareil) {
  data |> 
    group_by(diplome_parents) |> 
    summarise(total = n(), compteur_oui = sum(get(nom_colonne) == "oui")) |> 
    mutate(pourcentage_oui = (compteur_oui / total) * 100) |> 
    mutate(appareil = nom_appareil) |> 
    select(diplome_parents, appareil, pourcentage_oui)
}

# Calculer les pourcentages pour chaque appareil
pourcentages_ordinateur <- calculer_pourcentage_oui(base_jeunesp$variables, "equipement_foyer_ordinateur_fixe_ou_portable", "Ordinateur (fixe ou portable)")
pourcentages_smartphone <- calculer_pourcentage_oui(base_jeunesp$variables, "equipement_foyer_telephone_portable", "Téléphone portable ou smartphone")
pourcentages_tablette <- calculer_pourcentage_oui(base_jeunesp$variables, "equipement_foyer_tablette", "Tablette")
pourcentages_console <- calculer_pourcentage_oui(base_jeunesp$variables, "equipement_foyer_console_fixe_ou_portable", "Console de jeux (fixe ou portable)")

# Combiner les résultats
donnees_combinees <- bind_rows(pourcentages_ordinateur, pourcentages_smartphone, pourcentages_tablette, pourcentages_console)

# Spécifier l'ordre des appareils
donnees_combinees$appareil <- factor(donnees_combinees$appareil, levels = c("Ordinateur (fixe ou portable)", "Téléphone portable ou smartphone", "Tablette", "Console de jeux (fixe ou portable)"))

# Organiser les niveaux de diplôme dans l'ordre souhaité
donnees_combinees$diplome_parents <- factor(donnees_combinees$diplome_parents, levels = c("< Bac", "Bac", "> Bac"))

# Tracer les données

graph_taux_equipement <- ggplot(donnees_combinees, aes(x = appareil, y = pourcentage_oui, fill = diplome_parents, group = diplome_parents)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_x_discrete(labels = scales::label_wrap(5)) +
  scale_y_continuous(n.breaks = 10, labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = c("#bdc9e1", "#74a9cf", "#0570b0")) +
  labs(y = "Pourcentage", 
       x = "Présence dans le foyer d'un appareil numérique",
       fill = "Diplôme des parents"
       #title = "Taux d'équipement des foyers en appareils numériques",
       #subtitle = "Un sous-titre"
       ) +
  theme(legend.position = "bottom")# +
  #theme_minimal()

#rm(calculer_pourcentage_oui, pourcentages_ordinateur, pourcentages_smartphone, pourcentages_tablette, pourcentages_console, donnees_combinees)


## Graphique Annexe 1 - Fréquence connexion internet ----

graph_freq_connexion_internet <- basep$variables |>
  filter(frequence_connexion_internet != "REF", frequence_connexion_internet != "NSP") |> 
  ggplot(
    aes(x = groupes_age_donnat, fill = fct_rev(frequence_connexion_internet))
  ) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "PuBu") +
  scale_x_discrete(labels = scales::label_wrap(4)) +
  scale_y_continuous(n.breaks = 10, labels = scales::label_percent()) +
  labs(
    y = "Pourcentage", 
    x = "Classe d'âge",
    fill = "Connexion à internet",
    #title = "Taux d'équipement des foyers en appareils numériques"
    ) +
  #xlab("CSP") + A METTRE JUSTE CI DESSUS
  #ylab("Proportion") +
  #labs(fill = "Tranche d'âge")
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3, nrow = 3))


## Tableau 2 - Pratiques numériques, selon le genre, l'origine sociale et l'âge ----

tab_pratiques_complet <- base_jeunes |> 
  filter(diplome_parents %in% c("> Bac", "< Bac")) |>
  tbl_strata(
    strata = c(tranches_age_jeunes, diplome_parents),
    .tbl_fun = ~ .x |> 
      tbl_summary(
        by = SEXE,
        digits = list(all_continuous() ~ 0),
        type = list(
          regarder_videos_en_ligne_au_moins_hebdo_num ~ "continuous",
          ecoute_musique_support_numerique_au_moins_hebdo_num ~ "continuous",
          utiliser_reseaux_sociaux_au_moins_hebdo_num ~ "continuous",
          jouer_jeux_videos_au_moins_hebdo_num ~ "continuous",
          consulter_presse_numerique_num ~ "continuous",
          programme_radio_podcast_streaming_num ~ "continuous",
          utiliser_numerique_pour_pratiques_amateur_num ~ "continuous"
        ),
        statistic = list(
          all_continuous() ~ "{mean}"),
        include = c(
          "regarder_videos_en_ligne_au_moins_hebdo_num",
          "ecoute_musique_support_numerique_au_moins_hebdo_num",
          "utiliser_reseaux_sociaux_au_moins_hebdo_num",
          "jouer_jeux_videos_au_moins_hebdo_num",
          "consulter_presse_numerique_num",
          "programme_radio_podcast_streaming_num",
          "utiliser_numerique_pour_pratiques_amateur_num")
      )
  ) 

tab_pratiques_complet_mis_en_forme <- tab_pratiques_complet |> 
  bold_labels() |> 
  modify_spanning_header(all_stat_cols() ~ NA) %>%
  modify_footnote(everything() ~ NA) |> 
  as_kable_extra(col.names = c("", "H", "F", "H", "F", "H", "F", "H", "F", "H", "F", "H", "F"), booktabs = TRUE) |> 
  add_header_above(c(" ", "< Bac" = 2, "> Bac" = 2, "< Bac" = 2, "> Bac" = 2, "< Bac" = 2, "> Bac" = 2)) |> 
  add_header_above(c(" ", "15-19 ans" = 4, "20-24 ans" = 4, "25-29 ans" = 4)) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Pourcentages non pondérés.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 92% des hommes de 15-19 ans dont le plus haut diplôme parental est le bac déclarent regarder des vidéos en ligne."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )


## Tableau 3 - Nombre d'usages du numérique en fonction du genre, l'âge, de la PCS (comme tableau 1 Nathan) ----

tab_nb_usages_en_fct_genre_age_pcs <- tbl_stack(
  list(
    base_jeunesp |>
      tbl_svysummary(
        by = indice_nombre_usages_numerique_cat,
        statistic = all_categorical() ~ "{p} %",
        digits = all_categorical() ~ 0,
        include = c("SEXE", "tranches_age_jeunes", "PCS_avec_exclusion"),
        percent = "row"
      ) |>
      add_p(),
    base_jeunes %>%
      mutate(
        var_a_val_unique = if_else(CRITAGE == "15-29 ans", "Ensemble des jeunes", NA)
      ) |>
      tbl_cross(
        row = var_a_val_unique,
        col = indice_nombre_usages_numerique_cat,
        statistic = "{p} %",
        digits = list(indice_nombre_usages_numerique_cat = 0),
        margin = NULL
      )
  ),
  quiet = TRUE
) |>
  modify_table_body(
    ~ .x %>%
      filter(label != "var_a_val_unique")
  ) |>
  modify_table_body( # je rentre manuellement les valeur pour la ligne "Ensemble", ce qui n'est pas élégant, mais je n'ai pas eu le temps de trouver une autre solution, étant donné que je me suis aperçu que tbl_cross() n'acceptait pas d'objet survey. Les résultats peuvent être vérifiés en éxecutant "base_jeunes$desc_quali(indice_nombre_usages_numerique_cat, pond = base_jeunes$POND)"
    ~ .x %>%
      mutate(
        stat_1 = if_else(row_number() == n(), "15 %", stat_1),
        stat_2 = if_else(row_number() == n(), "34 %", stat_2),
        stat_3 = if_else(row_number() == n(), "43 %", stat_3),
        stat_4 = if_else(row_number() == n(), "9 %", stat_4)
      )
  ) |>
  bold_labels() |>
  modify_footnote(everything() ~ NA) |>
  as_kable_extra(col.names = c("", "Usage limité (0 à 3)", "Usage modéré (4 à 6)", "Usage diversifié (7 à 10)", "Usage très diversifié (11 à 15)", "p-valeur"), booktabs = TRUE) |>
  row_spec(14, bold = TRUE, background = "lightgray") |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Pourcentages pondérés.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 13% des hommes déclarent avoir entre 0 et 3 pratiques numériques parmi celles considérées."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

## Annexe A Tableau variables utilisées pour Nombre d'usages du numérique ----

# Ces instructions permettent de créer un tableau à deux colonnes pour répartir les labels de variables, colonnes qui sont plus grandes selon le nombre de variables (il suffit de changer le 2 pour modifier le nombre de colonnes)
liste_var_pour_nb_usage_num <- base_jeunes |> 
  select(faire_montages, 
         suivre_au_moins_un_cours_internet, 
         utiliser_numerique_pour_apprendre_en_amateur, 
         utiliser_numerique_pour_creer_en_amateur, 
         utiliser_numerique_pour_diffuser_en_amateur, 
         jouer_jeux_videos_au_moins_hebdo, 
         jouer_jeux_en_ligne_au_moins_temps_en_temps, 
         utiliser_equipement_numerique_pour_regarder_tele, 
         regarder_videos_en_ligne_au_moins_hebdo, 
         utiliser_equipement_numerique_pour_regarder_films, 
         utiliser_equipement_numerique_pour_regarder_series, 
         utiliser_numerique_pour_se_tenir_informe, 
         programme_radio_podcast_streaming, 
         ecoute_musique_support_numerique_au_moins_hebdo, 
         utiliser_tablette_pour_lire, 
         utiliser_reseaux_sociaux_au_moins_hebdo, 
         participer_manifestation_culturelle_en_ligne) |> 
  var_label() |> 
  enframe(name = "variables", value = "labels") |> 
  mutate(
    labels = as.character(labels),
    col_id = 1 + (row_number() - 1) %% 2,
    row_id = (row_number() - 1) %/% 2
  ) |> 
  select(-variables) |> 
  pivot_wider(names_from = col_id, values_from = labels, names_prefix = "Col") |>  
  select(-row_id) |> 
  mutate(across(everything(), ~replace(., is.na(.), ""))) |> 
  kbl(col.names = c("", ""), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down"))
  


## Graphique annexe 2 - Distribution de la diversification des usages du numérique ----

graphique_distribution_diversite_usages_numeriques <- base_jeunesp$variables |> 
  ggplot(aes(x = indice_nombre_usages_numerique)) +
  geom_bar(fill = "steelblue") +
  labs(
    #title = "",
    x = "Valeur de l'indice",
    y = "Nombre d'individus"
  )


## Graphique 2 - Distribution du poids relatif des pratiques culturelles numériques dans l'ensemble des pratiques culturelles ----

# A PRIORI ENLEVER JE PENSE -- REFORMULER Histogramme de l'indice de diversité détaillé : diversité des pratiques culturelle numériques
#base_jeunes |> 
#  ggplot(aes(x = indice_nombre_usages_numerique)) +
#  geom_bar(fill = "deepskyblue") +
#  labs(
#    title = "Valeurs pouvant aller de 0 à 48",
#    x = "Valeur de l'indice",
#    y = "Nombre d'individus"
#  )

# Note : peut-être dans le texte mettre une petite formule du calcul du poids relatif du numérique : (1/7 * score_pratiques_culturelles_numeriques) / (1/7 * score_pratiques_culturelles_numeriques + 1/9 * score_pratiques_culturelles_non_numeriques)

distrib_poids_relatif_num <- base_jeunesp$variables |> 
  ggplot(aes(x = PCS_avec_exclusion, y = poids_relatif_numerique_dans_lensemble_des_pratiques)) +
  geom_half_boxplot(fill = "#2b8cbe") +
  geom_half_violin(side = "r", fill = "#a6bddb") +
  coord_flip() +
  scale_x_discrete(labels = scales::label_wrap(3))+
  theme(legend.position = "none") +
  ylim(0.35, 0.75) +
  labs(
    #title = "Valeurs pouvant aller de 0 à 48",
    x = "PCS",
    y = "Score de prégnance relative"
  )


## Annexe B variables utilisées pour Score prat cult num  ----

# Ces instructions permettent de créer un tableau à deux colonnes pour répartir les labels de variables, colonnes qui sont plus grandes selon le nombre de variables (il suffit de changer le 2 pour modifier le nombre de colonnes)
liste_var_pour_score_prat_cult_num <- base_jeunes |> 
  select(ecoute_musique_support_numerique_au_moins_hebdo,
         jouer_jeux_videos_au_moins_hebdo,
         regarder_videos_en_ligne_au_moins_hebdo,
         utiliser_reseaux_sociaux_au_moins_hebdo,
         regarder_tele_films_ou_series_sur_support_numerique,
         utiliser_numerique_pour_creer_ou_faire_montages,
         participer_manifestation_culturelle_en_ligne) |> 
  var_label() |> 
  enframe(name = "variables", value = "labels") |> 
  mutate(
    labels = as.character(labels),
    col_id = 1 + (row_number() - 1) %% 2,
    row_id = (row_number() - 1) %/% 2
  ) |> 
  select(-variables) |> 
  pivot_wider(names_from = col_id, values_from = labels, names_prefix = "Col") |>  
  select(-row_id) |> 
  mutate(across(everything(), ~replace(., is.na(.), ""))) |> 
  kbl(col.names = c("", ""), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down"))


## Annexe C variables utilisées pour Score prat cult num ----

# Ces instructions permettent de créer un tableau à deux colonnes pour répartir les labels de variables, colonnes qui sont plus grandes selon le nombre de variables (il suffit de changer le 2 pour modifier le nombre de colonnes)
liste_var_pour_score_prat_cult_non_num <- base_jeunes |> 
  select(regarder_television_au_moins_hebdo,
         ecoute_radio_au_moins_hebdo,
         avoir_lu_au_moins_quatre_livre,
         sorties_cinema_au_moins_cinq,
         concert_au_moins_un,
         spectacle_vivant_au_moins_un,
         assister_festival,
         visite_culturelle_au_moins_une,
         pratique_artistique_au_moins_une) |> 
  var_label() |> 
  enframe(name = "variables", value = "labels") |> 
  mutate(
    labels = as.character(labels),
    col_id = 1 + (row_number() - 1) %% 2,
    row_id = (row_number() - 1) %/% 2
  ) |> 
  select(-variables) |> 
  pivot_wider(names_from = col_id, values_from = labels, names_prefix = "Col") |>  
  select(-row_id) |> 
  mutate(across(everything(), ~replace(., is.na(.), ""))) |> 
  kbl(col.names = c("", ""), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down"))


## Tableau 4 -- Modèle de régression linéaire sur la poids relatif du numérique dans l'ensemble des pratiques ----

## Préciser que moyenne 57 et écart-type 13.5
#mean(base_jeunes$poids_relatif_numerique_dans_lensemble_des_pratiques * 100)
#sd(base_jeunes$poids_relatif_numerique_dans_lensemble_des_pratiques * 100)

modele_lin <- lm(poids_relatif_numerique_dans_lensemble_des_pratiques * 100 ~ SEXE + tranches_age_jeunes + PCS_avec_exclusion, data = base_jeunes) # + utiliser_numerique_pour_pratiques_amateur

tab_modele_lin_mis_en_forme <- modele_lin |> 
  tbl_regression(
    intercept = TRUE
  ) |>
  bold_labels() |> 
  modify_header(estimate ~ "**Coefficient**") |>
  add_global_p(keep = TRUE) |>
  modify_column_hide(c("ci")) |> 
  as_kable_extra(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : « Toutes choses égales par ailleurs » être une femme plutôt qu'un homme diminue le score de prégnance relative des pratiques culturelles numériques de -4,3 points, et cet effet est significatif (p-valeur < 0,05)."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

# ACM ----

## Mise en oeuvre ----

preparation_base_acm1 <- base_jeunes |> 
  mutate(
    regarder_sur_internet_films = ifelse(is.na(regarder_sur_internet_films), regarder_sur_internet_films,
                         paste("Regarder films sur internet", str_to_lower(regarder_sur_internet_films), sep = " : ")),
    regarder_sur_internet_series = ifelse(is.na(regarder_sur_internet_series), regarder_sur_internet_series,
                         paste("Regarder séries sur internet", str_to_lower(regarder_sur_internet_series), sep = " : ")),
    regarder_sur_internet_bande_annonce = ifelse(is.na(regarder_sur_internet_bande_annonce), regarder_sur_internet_bande_annonce,
                         paste("Regarder bande-annonces sur internet", str_to_lower(regarder_sur_internet_bande_annonce), sep = " : ")),
    regarder_sur_internet_clips = ifelse(is.na(regarder_sur_internet_clips), regarder_sur_internet_clips,
                         paste("Regarder clips sur internet", str_to_lower(regarder_sur_internet_clips), sep = " : ")),
    regarder_sur_internet_infos = ifelse(is.na(regarder_sur_internet_infos), regarder_sur_internet_infos,
                         paste("Regarder infos sur internet", str_to_lower(regarder_sur_internet_infos), sep = " : ")),
    regarder_sur_internet_sport = ifelse(is.na(regarder_sur_internet_sport), regarder_sur_internet_sport,
                         paste("Regarder sport sur internet", str_to_lower(regarder_sur_internet_sport), sep = " : ")),
    regarder_sur_internet_youtubeurs = ifelse(is.na(regarder_sur_internet_youtubeurs), regarder_sur_internet_youtubeurs,
                         paste("Regarder youtubeurs sur internet", str_to_lower(regarder_sur_internet_youtubeurs), sep = " : ")),
    regarder_sur_internet_reportages = ifelse(is.na(regarder_sur_internet_reportages), regarder_sur_internet_reportages,
                         paste("Regarder reportages sur internet", str_to_lower(regarder_sur_internet_reportages), sep = " : ")),
    regarder_sur_internet_jeux = ifelse(is.na(regarder_sur_internet_jeux), regarder_sur_internet_jeux,
                         paste("Regarder jeux sur internet", str_to_lower(regarder_sur_internet_jeux), sep = " : ")),
    utiliser_pour_videos_internet_tele = ifelse(is.na(utiliser_pour_videos_internet_tele), utiliser_pour_videos_internet_tele,
                         paste("Utiliser télé pour regarder vidéos sur internet", str_to_lower(utiliser_pour_videos_internet_tele), sep = " : ")),
    utiliser_pour_videos_internet_ordi = ifelse(is.na(utiliser_pour_videos_internet_ordi), utiliser_pour_videos_internet_ordi,
                         paste("Utiliser ordi pour regarder vidéos sur internet", str_to_lower(utiliser_pour_videos_internet_ordi), sep = " : ")),
    utiliser_pour_videos_internet_tablette = ifelse(is.na(utiliser_pour_videos_internet_tablette), utiliser_pour_videos_internet_tablette,
                         paste("Utiliser tablette pour regarder vidéos sur internet", str_to_lower(utiliser_pour_videos_internet_tablette), sep = " : ")),
    utiliser_pour_videos_internet_smartphone = ifelse(is.na(utiliser_pour_videos_internet_smartphone), utiliser_pour_videos_internet_smartphone,
                         paste("Utiliser smartphone pour regarder vidéos sur internet", str_to_lower(utiliser_pour_videos_internet_smartphone), sep = " : ")),
    jouer_jeux_videos_frequence = ifelse(is.na(jouer_jeux_videos_frequence), jouer_jeux_videos_frequence,
                         paste("Fréquence jeux-vidéos", str_to_lower(jouer_jeux_videos_frequence), sep = " : ")),
    programme_radio_podcast_streaming = ifelse(is.na(programme_radio_podcast_streaming), programme_radio_podcast_streaming,
                         paste("Ecouter programmes radiophoniques en podcast/streaming", str_to_lower(programme_radio_podcast_streaming), sep = " : ")),
    utiliser_numerique_pour_pratiques_amateur = ifelse(is.na(utiliser_numerique_pour_pratiques_amateur), utiliser_numerique_pour_pratiques_amateur,
                         paste("Utiliser numérique pour pratique en amateur", str_to_lower(utiliser_numerique_pour_pratiques_amateur), sep = " : ")),
    frequence_reseaux_sociaux = ifelse(is.na(frequence_reseaux_sociaux), frequence_reseaux_sociaux,
                                       paste("Fréquence utilisation réseaux sociaux", str_to_lower(frequence_reseaux_sociaux), sep = " : ")),
    regarder_videos_en_ligne_frequence = ifelse(is.na(regarder_videos_en_ligne_frequence), regarder_videos_en_ligne_frequence,
                                                paste("Fréquence de visionnage de vidéos en ligne", str_to_lower(regarder_videos_en_ligne_frequence), sep = " : ")),
    SEXE = ifelse(is.na(SEXE), SEXE,
                         paste("Genre", str_to_lower(SEXE), sep = " : ")),
    tranches_age_jeunes = ifelse(is.na(tranches_age_jeunes), tranches_age_jeunes,
                         paste("Classe d'âge", str_to_lower(tranches_age_jeunes), sep = " : ")),
    PCS_avec_exclusion = ifelse(is.na(PCS_avec_exclusion), PCS_avec_exclusion,
                  paste("PCS", str_to_lower(PCS_avec_exclusion), sep = " : ")),
    score_pratiques_culturelles_non_numeriques_cat = ifelse(is.na(score_pratiques_culturelles_non_numeriques_cat), score_pratiques_culturelles_non_numeriques_cat,
                  paste("Prat. non-num.", str_to_lower(score_pratiques_culturelles_non_numeriques_cat), sep = " : "))
  ) |> 
  filter(regarder_videos_en_ligne_au_moins_hebdo == "oui") |> 
  select(
    regarder_sur_internet_films,
    regarder_sur_internet_series,
    regarder_sur_internet_bande_annonce,
    regarder_sur_internet_clips,
    regarder_sur_internet_infos,
    regarder_sur_internet_sport,
    regarder_sur_internet_youtubeurs,
    #regarder_sur_internet_enfants,
    regarder_sur_internet_reportages,
    regarder_sur_internet_jeux,
    #regarder_sur_internet_autre,
    #videos_manqueraient,
    utiliser_pour_videos_internet_tele,
    utiliser_pour_videos_internet_ordi,
    utiliser_pour_videos_internet_tablette,
    utiliser_pour_videos_internet_smartphone,
    #utiliser_pour_videos_internet_projecteur,
    jouer_jeux_videos_frequence,
    programme_radio_podcast_streaming,
    utiliser_numerique_pour_pratiques_amateur,
    #frequence_reseaux_sociaux, # je ne le mets pas
    regarder_videos_en_ligne_frequence,
    SEXE,
    tranches_age_jeunes,
    PCS_avec_exclusion,
    score_pratiques_culturelles_non_numeriques_cat
  )

# Eventuellement
# Fréquence utilisation réseaux sociaux
# Vidéos : vidéos manqueraient ?
# Ajouter variables détail sur musique : fréquence écoute de musique sur support numérique, nombre de genres écoutés, ?musique manquerait?
# Ajouter variables détail sur jeux-vidéos : jouer seul ou avec proches, jouer en ligne ou non, genres ou nombre de genres, ?jeux-vidéos manqueraient?
# Nombre de pratiques non-numériques (utiliser score pratiques non-numériques) -> peut-être ça en variable illustrative

acm1 <- MCA(preparation_base_acm1, quali.sup =(ncol(preparation_base_acm1)-3):ncol(preparation_base_acm1), graph = FALSE) # le nombre mis dans "quali.sup" correspond à un de moins que le nombre des variables supplémentaires qu'on veut inclure
# par défaut les 5 premiers axes sont gardés

#explor(acm1)

## Annexe D variables actives de l'ACM ----

# On utilise preparation_base_acm1 parce qu'on a juste la sélection de variables

# Ces instructions permettent de créer un tableau à deux colonnes pour répartir les labels de variables, colonnes qui sont plus grandes selon le nombre de variables (il suffit de changer le 2 pour modifier le nombre de colonnes)
liste_var_actives_acm <- preparation_base_acm1 |> 
  set_variable_labels(
    regarder_sur_internet_films = "Regarder films sur internet",
    regarder_sur_internet_series = "Regarder séries sur internet",
    regarder_sur_internet_bande_annonce = "Regarder bande-annonces sur internet",
    regarder_sur_internet_clips = "Regarder clips sur internet",
    regarder_sur_internet_infos = "Regarder infos sur internet",
    regarder_sur_internet_sport = "Regarder sport sur internet",
    regarder_sur_internet_youtubeurs = "Regarder youtubeurs sur internet",
    regarder_sur_internet_reportages = "Regarder reportages sur internet",
    regarder_sur_internet_jeux = "Regarder jeux sur internet",
    utiliser_pour_videos_internet_tele = "Utiliser télé pour regarder vidéos sur internet",
    utiliser_pour_videos_internet_ordi = "Utiliser ordi pour regarder vidéos sur internet",
    utiliser_pour_videos_internet_tablette = "Utiliser tablette pour regarder vidéos sur internet",
    utiliser_pour_videos_internet_smartphone = "Utiliser smartphone pour regarder vidéos sur internet",
    jouer_jeux_videos_frequence = "Fréquence jeux-vidéos",
    programme_radio_podcast_streaming = "Ecouter programmes radiophoniques en podcast/streaming",
    utiliser_numerique_pour_pratiques_amateur = "Utiliser numérique pour pratique en amateur",
    regarder_videos_en_ligne_frequence = "Fréquence de visionnage de vidéos en ligne"
  ) |> 
  select(regarder_sur_internet_films,
         regarder_sur_internet_series,
         regarder_sur_internet_bande_annonce,
         regarder_sur_internet_clips,
         regarder_sur_internet_infos,
         regarder_sur_internet_sport,
         regarder_sur_internet_youtubeurs,
         #regarder_sur_internet_enfants,
         regarder_sur_internet_reportages,
         regarder_sur_internet_jeux,
         #regarder_sur_internet_autre,
         #videos_manqueraient,
         utiliser_pour_videos_internet_tele,
         utiliser_pour_videos_internet_ordi,
         utiliser_pour_videos_internet_tablette,
         utiliser_pour_videos_internet_smartphone,
         #utiliser_pour_videos_internet_projecteur,
         jouer_jeux_videos_frequence,
         programme_radio_podcast_streaming,
         utiliser_numerique_pour_pratiques_amateur,
         #frequence_reseaux_sociaux, # je ne le mets pas
         regarder_videos_en_ligne_frequence,
         #SEXE,
         #tranches_age_jeunes,
         #PCS_avec_exclusion,
         #score_pratiques_culturelles_non_numeriques_cat
         ) |> 
  var_label() |> 
  enframe(name = "variables", value = "labels") |> 
  mutate(
    labels = as.character(labels),
    col_id = 1 + (row_number() - 1) %% 2,
    row_id = (row_number() - 1) %/% 2
  ) |> 
  select(-variables) |> 
  pivot_wider(names_from = col_id, values_from = labels, names_prefix = "Col") |>  
  select(-row_id) |> 
  mutate(across(everything(), ~replace(., is.na(.), ""))) |> 
  kbl(col.names = c("", ""), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down"))


## Diagramme d'éboulis des variances associées aux axes de l'ACM ----

diagramme_eboulis <- fviz_screeplot(acm1, 
               addlabels = TRUE, # on mets pas les pourcentages car ils sont coupés sur le graphique 
               barfill = ifelse(acm1$eig[1:10] > 0.08, "steelblue", "gray"), 
               barcolor = ifelse(acm1$eig[1:10] > 0.08, "steelblue", "gray"), 
               main = "", #"Diagramme d’éboulis des variances associées aux axes de l’ACM", 
               xlab = "Axes", 
               ylab = "Pourcentage d'inertie par axe",
               ylim = c(0, 17),
               xlim = c(1, 10.4)
               ) #+
  #theme(plot.margin = margin(-1, -1, -1, -1, "cm"))


## Tableaux des modalités les plus contributives aux axes 1 et 2 ----

# Reprise du code de JRDavalos (~ ligne 1000)

## Extraction de toutes les données de l'ACM
tab_agd <- function(agd, data, sortie = "res") {# création de tous les tableaux pour les acm
  type <- class(agd)
  if(class(agd)[1] %>% str_detect("MCA")) {# ne marche que si c'est une ACM
    variances.acm <- as.data.frame(agd$eig) %>%
      rownames_to_column() %>% # récupérer les noms de lignes (dim 1, dim 2, etc) dans une colonne distincte
      slice(1:nrow(agd$eig)) %>% # tout conserver
      mutate(Axes = str_replace_all(rowname, "dim", "Axe")) %>% # créer une nouvelle variable à partir de rowname, qui prend les valeurs "Axe 1, Axe 2, etc" au lieu de "dim 1, dim 2, etc."
      select(-rowname) %>%# on enlève cette colonne dont on n'a plus besoin
      rename(`Valeurs propres` = eigenvalue) %>%
      rename(`% de variance` = `percentage of variance`) %>% # on renomme les autres colonnes
      rename(`% cumulé de variance` = `cumulative percentage of variance`) %>% 
      mutate(Axes = fct_relevel(Axes, paste("Axe", 1:nrow(agd$eig))))
    
    # variables actives
    frequences.acm <- # fréquences de chaque modalité
      pivot_longer(data, everything(), names_to = "variables", values_to = "modalites") %>% # prend les modalités 
      count(variables,modalites) %>% # compte
      group_by(variables) %>% # regroupe
      mutate(pourcentage = round(100 * n / nrow(data), 1)) %>% # tri à plat des modalités
      ungroup() %>% 
      select(variables,modalites,n,pourcentage)
    
    coordonnees.acm <- # coordonnée de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$coord, 2)) %>% 
      rename_all(tolower) %>% # change les noms
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "coord", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    contributions.acm <- # contribution de chaque modalité à chaque axe
      as.data.frame(round(agd$var$contrib, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "contrib", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    cos2.acm <- # cos2 de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$cos2, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "cos2", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    vtest.acm <- # test de chaque modalité sur chaque axe
      as.data.frame(round(agd$var$v.test, 2)) %>% 
      rename_all(tolower) %>% # idem
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "vtest", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    resultats_actives.acm <- # fusion variables actives
      frequences.acm %>% 
      right_join(coordonnees.acm) %>% 
      right_join(contributions.acm) %>% 
      right_join(cos2.acm) %>% 
      right_join(vtest.acm) %>% 
      mutate(type = "Active") # pour repérer les variables actives dans le grand tableau
    
    # variables sup
    coordonnees_sup.acm <-  # coordonnée de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$coord, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "coord", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    cos2_sup.acm <-  # cos2 de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$cos2, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "cos2", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    vtest_sup.acm <-  # test de chaque modalité sur chaque axe
      as.data.frame(round(agd$quali.sup$v.test, 2)) %>% 
      rename_all(tolower) %>% 
      rename_all(~ str_replace(.," ","")) %>% 
      rename_all(~ str_c(., "vtest", sep = "_")) %>%
      mutate(modalites = rownames(.))
    
    resultats_sup.acm <- # fusion variables sup
      frequences.acm %>% 
      right_join(coordonnees_sup.acm) %>% 
      right_join(cos2_sup.acm) %>% 
      right_join(vtest_sup.acm) %>% 
      mutate(type = "Supplémentaire")
    
    resultats_complet.acm <- bind_rows(resultats_actives.acm, resultats_sup.acm) # fusion des deux grands tableaux
  }
  else {message("Pas une ACM")}
  if(sortie == "res") {return(resultats_complet.acm)}
  if(sortie == "var") {return(variances.acm)}
}

res.ACM <- tab_agd(acm1, preparation_base_acm1) %>% # résultats complets
  filter(!str_detect(modalites,"NA"))

## Inerties
var.ACM <- tab_agd(acm1, preparation_base_acm1, "var") %>% # récupération variance
  slice_head(n = 10) %>% # dix premiers axes
  rename(` ` = Axes) #%>% 
  #select(6,1:3) %>% # axes / valeurs propres / % variance / % variance cumulée
  #mutate_if(is.numeric, ~round(., 2)) # arrondir les num

## Axe 1
## tableaux (on divise en coord > 0 et coord < 0)
axe1_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim1_contrib > 3, dim1_coord < 0) %>% # contrib > la moyenne
  select(modalites, n, dim1_coord, dim1_contrib, dim1_cos2, dim1_vtest) %>% 
  arrange(desc(dim1_contrib)) %>% # ordre décroissant
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe1_2 <- res.ACM %>% # idem
  filter(type == "Active") %>% 
  filter(dim1_contrib > 3, dim1_coord > 0) %>% 
  select(modalites, n, dim1_coord, dim1_contrib, dim1_cos2, dim1_vtest) %>% 
  arrange(desc(dim1_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))
## On a gardé les modalité avec contrib > 3

axe1 <- bind_rows(axe1_1, axe1_2) # fusion
tab_modalites_contributives_axe1 <- axe1 # |> select(-N)


## Axe 2
# tableaux (même principe)
axe2_1 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim2_contrib > 2, dim2_coord < 0) %>% 
  select(modalites, n, dim2_coord, dim2_contrib, dim2_cos2, dim2_vtest) %>% 
  arrange(desc(dim2_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))

axe2_2 <- res.ACM %>% 
  filter(type == "Active") %>% 
  filter(dim2_contrib > 2, dim2_coord > 0) %>% 
  select(modalites, n, dim2_coord, dim2_contrib, dim2_cos2, dim2_vtest) %>% 
  arrange(desc(dim2_contrib)) %>% 
  `colnames<-`(c("Modalités", "N", "Coordonnée","Contribution", "cos2", "Test"))
## On a gardé les modalité avec contrib > 2

axe2 <- bind_rows(axe2_1, axe2_2)
tab_modalites_contributives_axe2 <- axe2 #|> select(-N)

#rm(axe1, axe1_1, axe1_2, axe2, axe2_1, axe2_2)

# Tableau modalités contributives axe 1

tab_modalites_contributives_axe1_mis_en_forme <- tab_modalites_contributives_axe1 |> 
  kbl(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine, et regardant au moins hebdomadairement des vidéos en ligne.",
      "Lecture : La modalité 'non' de la variable « regarder films sur internet » a son barycentre en coordonnée -0,62 sur l’axe 1, contribue pour 7% à la construction de l’axe, et a une bonne qualité de représentation."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

# Tableau modalités contributives axe 2

tab_modalites_contributives_axe2_mis_en_forme <- tab_modalites_contributives_axe2 |> 
  kbl(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine, et regardant au moins hebdomadairement des vidéos en ligne.",
      "Lecture : La modalité 'non' de la variable « regarder films sur internet » a son barycentre en coordonnée -0,66 sur l’axe 1, contribue pour 11% à la construction de l’axe, et a une bonne qualité de représentation."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

## Tableau annexe Représentation des variables supplémentaires  ----
# Inspiration A.3.17 de JRDavalos

tab_representation_var_supp <- tab_quali_agd(acm1) |>
  filter(type == "Supplementaire") |>
  select(
    modalite,
    pourcentage,
    Dim1_coord,
    Dim1_cos2,
    Dim2_coord,
    Dim2_cos2
    ) |> 
  mutate(across(c("pourcentage"), round, 0)) |> 
  mutate(across(c("Dim1_coord", "Dim2_coord"), round, 2)) |> 
  mutate(across(c("Dim1_cos2", "Dim2_cos2"), round, 2)) |> 
  kbl(col.names = c("Modalités", "Proportion dans l'ACM (en %)", "Coord", "cos2", "Coord", "cos2"), booktabs = TRUE) |> 
  add_header_above(c(" ", " ", "Axe 1" = 2, "Axe 2" = 2)) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine, et regardant au moins hebdomadairement des vidéos en ligne.",
      "Lecture : la modalité PCS ouvriers se situe à -0,16 sur l'axe 1 et à -0,26 sur l'axe 2."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )
  

## Projection du plan 1-2 de l'ACM ----

# paramètres du plan 
supp <- c("SEXE", "tranches_age_jeunes", "PCS_avec_exclusion", "score_pratiques_culturelles_non_numeriques_cat") # variables sup à garder
seuil = 2.5 # seuil de contributions
act <- res.ACM$variables[res.ACM$type == "Active" & # variables actives 
                           (res.ACM$dim1_contrib > seuil | #sup au seuil sur l'axe 1
                              res.ACM$dim2_contrib > seuil)] # et sur l'axe 2

# Projection du plan ----
projection_plan_factoriel <- res.ACM  %>% 
  filter(dim1_contrib > seuil | dim2_contrib > seuil |
           variables %in% supp) %>% # on part du tableau de résultat des modalités actives et on filtre uniquement celles dont la contribution dépasse le seuil pour l'un ou l'autre des deux axes (| est l'opérateur logique OU).
  
  ggplot(aes(x = dim1_coord, y = dim2_coord, # initialisation du graphique
             label = modalites, # les labels des points sont les modalités
             shape = type, # forme des points en fonction de sup/active
             colour = variables, # une couleur par variable
             alpha = dim1_cos2 + dim2_cos2, # transparence selon la représentation sur le plan
             size = n)) + # les formes des points dépendent des variables : à chaque variable son symbole
  
  coord_fixed() + # pour que les échelles des axes soient identiques
  scale_colour_manual(values = c(setNames(colorRampPalette(brewer.pal(12, "Paired"))(length(act)),act),
                                 setNames(rep("black",length(supp)), supp))) + # échelle en fonction du nombre de modalités
  
  geom_point(aes(size = n)) + # tracer les points
  scale_size_continuous(range = c(15, 22)) +
  #geom_label(size = 12) +
  geom_label_repel(size = 15, segment.alpha = 0.3, label.size = NA, label.padding	= 0, alpha = 1) + 
  # spécificité des labels
  
  geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne horizontale y = 0
  geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + # ligne verticale x = 0
  
  xlab(paste0("Niveau d'engagement dans les pratiques culturelles numériques (", round(var.ACM[1, 2], 1), " %)")) + # nom axe horizontal
  ylab(paste0("Types de pratiques numériques (", round(var.ACM[2, 2], 1), " %)")) + # idem pour l'axe vertical
  
  guides(colour = FALSE, #légende (couleur, forme, taille, transparence)
         shape = guide_legend(title="Type", override.aes = list(size = 18), title.position = "top", ncol = 1, order = 1),
         size = guide_legend(title="N", title.position = "top", ncol = 2, order = 2),
         alpha = guide_legend(title = "Qualité de représentation", override.aes = list(size = 18), title.position = "top", ncol = 3, order = 3)) +
  
  theme_minimal() + # thème sans rien
  theme(legend.position="bottom", # en ajoutant la légende 
        legend.direction="vertical",
        text = element_text(family = "Times", size = 70)) +
  
  geom_line(data = res.ACM[res.ACM$variables == "SEXE",], orientation = "y", # segments ages
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "red", alpha = 0.6) +
  geom_line(data = res.ACM[res.ACM$variables == "tranches_age_jeunes",], orientation = "y", # segments ages
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.6) +
  geom_line(data = res.ACM[res.ACM$variables == "PCS_avec_exclusion",], orientation = "y", # segments ages
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "blue", alpha = 0.6) +
  geom_line(data = res.ACM[res.ACM$variables == "score_pratiques_culturelles_non_numeriques_cat",], orientation = "y", # segments ages
            mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "brown", alpha = 0.6)
  #geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][1:3,], # segments année d'arrivée à lfi 
  #          mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2) +
  
  #geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][3:4,],# idem mais sur un autre plan (sinon désordre)
  #          mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2) +
  
  #geom_line(data = res.ACM[res.ACM$variables == "anneelfi",][4:6,], # idem
  #          mapping = aes(x = dim1_coord, y = dim2_coord), lwd = .6, color = "green", alpha = 0.2)



# Classification ----


hcpc1 <- HCPC(acm1, nb.clust = 3, graph = FALSE) # Cette méthode utilise le critère de Ward et est consolidée avec la méthode des k-means

# On ajoute dans la base de données filtrée (car sinon on a pas de valeur pour les jeunes qui ne regardent pas de vidéos en ligne) la variable class3, qui indique à quelle classe de la CAH l'individu appartient

base_jeunes_apres_acm <- base_jeunes |> 
  filter(regarder_videos_en_ligne_au_moins_hebdo == "oui")

base_jeunes_apres_acm$class3 <- hcpc1$data.clust$clust

base_jeunes_apres_acm$class3 <- fct_recode(base_jeunes_apres_acm$class3,
  "Les « distants » du numérique" = "1",
  "Les « classiques-légitimes » du numérique" = "2",
  "Les « omnivores » du numérique" = "3"
)

var_label(base_jeunes_apres_acm$class3) <- "Classes de la CAH"

#freq(base_jeunes_apres_acm$class3)
#on peut aussi croiser les classes avec d'autres variables: 
#cprop(table(EVS$class3, EVS$parti_pol))
#lprop(table(EVS$class3, EVS$parti_pol))


## Dendrogramme ----
dendrogramme <- fviz_dend(hcpc1, 
          lwd = 1.4,
          show_labels = FALSE,
          ylab = "Dissimilarité intra-classe",
          main = "")


## Projection des classes sur les axes 1 et 2 du plan factoriel ----

projection_classes <- fviz_cluster(hcpc1,
             geom = c("point"),
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "Dark2",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = ""
)


## Tri à plat des classes issues de la CAH ----

tri_a_plat_classes <- base_jeunes_apres_acm |> 
  desc_quali(class3) |> 
  select(-Variable) |> 
  slice(1:(n()-1)) |> 
  kbl(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 38,2% des individus sont regroupés dans la classe des « distants » du numérique, soit 395 personnes."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

## Annexe Tri croisé recoupement des classes avec celles de Lombardo et Wolff (tout numérique vs éclectisme vs bain audiovisuel augmenté vs autre) ----

tri_croise_classes_univers_pratiques <- base_jeunes_apres_acm |>
  tbl_summary(
    by = CLASS_univprat_quatre_cat,
    statistic = all_categorical() ~ "{p} %",
    digits = all_categorical() ~ 0,
    include = c("class3"),
    percent = "row"
  ) |>
  add_p() |>
  bold_labels() |>
  modify_footnote(everything() ~ NA) |>
  as_kable_extra(col.names = c("", "Univers du bain audiovisuel", "Univers du tout-numérique", "Univers de l'éclectisme augmenté", "Un des autres univers", "p-valeur"), booktabs = TRUE) |>
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine, et regardant au moins hebdomadairement des vidéos en ligne.",
      "Lecture : 28% des individus classés dans notre ACM comme des « distants » du numériques auraient été classés dans l'univers du bain audiovisuel dans la typologie de Lombardo et Wolff, contre 21% des individus classés dans notre ACM comme des « classiques-légitimes » du numériques."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )


## Caractéristiques de la classe 1/Tableau des modalités en lien avec la classe 1 ----
# cf notes lecture Thomas et JRDavalos

tab_caract_class1 <- hcpc1$desc.var$category$`1` |> as.data.frame() |> 
  rownames_to_column('modalite') |> 
  tibble() |> 
  rename(
    pctge_de_la_modalité = `Cla/Mod`,
    pctge_de_la_classe = `Mod/Cla`,
    pctge_global = Global,
    Test = v.test
    ) |>
  select(-p.value) |> 
  mutate(modalite = str_replace(modalite, ".*=", "")) |> 
  mutate(across(where(is.numeric), round, 1)) |> 
  kbl(col.names = c("Modalité", "% de la modalité", "% de la classe", "% global", "Test"), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 41,9% des femmes de l’échantillon appartiennent à cette classe ; leur part au sein de cette classe s’élève à 58%, contre 52,9% dans la population étudiée."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )


## Caractéristiques de la classe 2/Tableau des modalités en lien avec la classe 2 ----

tab_caract_class2 <- hcpc1$desc.var$category$`2` |> as.data.frame() |> 
  rownames_to_column('modalite') |> 
  tibble() |> 
  rename(
    pctge_de_la_modalité = `Cla/Mod`,
    pctge_de_la_classe = `Mod/Cla`,
    pctge_global = Global,
    Test = v.test
  ) |>
  select(-p.value) |> 
  mutate(modalite = str_replace(modalite, ".*=", "")) |> 
  mutate(across(where(is.numeric), round, 1)) |> 
  kbl(col.names = c("Modalité", "% de la modalité", "% de la classe", "% global", "Test"), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 51,1% des individus de l'échantillon regardant des séries sur internet appartiennent à cette classe ; leur part au sein de cette classe s’élève à 89,7%, contre 59,0% dans la population étudiée."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )


## Caractéristiques de la classe 3/Tableau des modalités en lien avec la classe 3 ----

tab_caract_class3 <- hcpc1$desc.var$category$`3` |> as.data.frame() |> 
  rownames_to_column('modalite') |> 
  tibble() |> 
  rename(
    pctge_de_la_modalité = `Cla/Mod`,
    pctge_de_la_classe = `Mod/Cla`,
    pctge_global = Global,
    Test = v.test
  ) |>
  select(-p.value) |> 
  mutate(modalite = str_replace(modalite, ".*=", "")) |> 
  mutate(across(where(is.numeric), round, 1)) |> 
  kbl(col.names = c("Modalité", "% de la modalité", "% de la classe", "% global", "Test"), booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : 65,4% des individus de l'échantillon regardant des vidéos de jeux vidéos sur internet appartiennent à cette classe ; leur part au sein de cette classe s’élève à 72,9%, contre 34,6% dans la population étudiée."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )


# Régression logistique multinomiale sur les classes ----


# Il faut choisir une modalité de référence (par ex B. Dubois choisit la catégorie qui a le plus grand effectif, dans son cas la 2e sur 3)
# Si on veut mettre la deuxième classe :
# base_jeunes_apres_acm$class3 <- relevel(base_jeunes_apres_acm$class3, "?La classe deux?")


# Fonction pour étendre en colonne les modèles pour chaque classe de la CAH

multinom_pivot_wider <- function(x) {
  # check inputs match expectatations
  if (!inherits(x, "tbl_regression") || !inherits(x$model_obj, "multinom")) {
    stop("`x=` must be class 'tbl_regression' summary of a `nnet::multinom()` model.")
  }
  
  # create tibble of results
  df <- tibble::tibble(outcome_level = unique(x$table_body$groupname_col))
  df$tbl <- 
    purrr::map(
      df$outcome_level,
      function(lvl) {
        gtsummary::modify_table_body(
          x, 
          ~dplyr::filter(.x, .data$groupname_col %in% lvl) %>%
            dplyr::ungroup() %>%
            dplyr::select(-.data$groupname_col)
        )
      }
    )
  
  tbl_merge(df$tbl, tab_spanner = paste0("**", df$outcome_level, "**"))
}

## Sans interaction ----

## Réalisation du modèle
regm_sans_interaction <- multinom(class3 ~ SEXE + tranches_age_jeunes + diplome_parents + commune_residence_trois_cat + score_pratiques_culturelles_non_numeriques_cat, data = base_jeunes_apres_acm) # + tranches_age_jeunes:diplome_parents ou + score_pratiques_culturelles_non_numeriques_cat:diplome_parents

# Tableau de régression:
tab_modele_log_sans_interaction_mis_en_forme <- regm_sans_interaction |>
  tbl_regression(
    exponentiate = TRUE,
    intercept = TRUE
  ) |>
  bold_labels() |> 
  modify_header(estimate ~ "**Rapport des cotes**") |>
  add_global_p(keep = TRUE) |>
  modify_column_hide(c("ci")) |> 
  multinom_pivot_wider() |> 
  modify_footnote(everything() ~ NA) |>
  as_kable_extra(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : Être une femme plutôt qu’un homme implique « toutes choses égales par ailleurs » d’avoir 0,31 fois plus de chance de se trouver dans la classe des « omnivores » du numérique plutôt que la classe des « distants » du numérique, cela étant significatif au seuil de 5% (p-valeur < 0,05)."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )




## Interaction age:diplome_parents ----

regm_interaction_age_dipl_parents <- multinom(class3 ~ SEXE + tranches_age_jeunes + diplome_parents + commune_residence_trois_cat + score_pratiques_culturelles_non_numeriques_cat + tranches_age_jeunes:diplome_parents, data = base_jeunes_apres_acm) # + tranches_age_jeunes:diplome_parents ou + score_pratiques_culturelles_non_numeriques_cat:diplome_parents

# Tableau de régression:
tab_modele_log_interaction_age_dipl_parents_mis_en_forme <- regm_interaction_age_dipl_parents |>
  tbl_regression(
    exponentiate = TRUE,
    intercept = TRUE
  ) |>
  bold_labels() |> 
  modify_header(estimate ~ "**Rapport des cotes**") |>
  add_global_p(keep = TRUE) |>
  modify_column_hide(c("ci")) |> 
  multinom_pivot_wider() |> 
  modify_footnote(everything() ~ NA) |>
  as_kable_extra(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : Lorsqu'on contrôle avec la variable du niveau de diplôme parental, les 20-24 ans ont 1,60 fois plus de chances que les 15-19 ans de se trouver dans la classe des « classiques-légitimes » du numérique plutôt que la classe des « distants » du numérique, cela étant significatif au seuil de 5% (p-valeur < 0,05)."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )

## Passage comme classe de référence de la 1 à la 2 ----

base_jeunes_apres_acm$class3 <- relevel(base_jeunes_apres_acm$class3, "Les « classiques-légitimes » du numérique")

regm_classe2_en_ref_interaction_age_dipl_parents <- multinom(class3 ~ SEXE + tranches_age_jeunes + diplome_parents + commune_residence_trois_cat + score_pratiques_culturelles_non_numeriques_cat + tranches_age_jeunes:diplome_parents, data = base_jeunes_apres_acm) # + tranches_age_jeunes:diplome_parents ou + score_pratiques_culturelles_non_numeriques_cat:diplome_parents

# Tableau de régression:
tab_modele_log_classe2_en_ref_interaction_age_dipl_parents_mis_en_forme <- regm_classe2_en_ref_interaction_age_dipl_parents |>
  tbl_regression(
    exponentiate = TRUE,
    intercept = TRUE
  ) |>
  bold_labels() |> 
  modify_header(estimate ~ "**Rapport des cotes**") |>
  add_global_p(keep = TRUE) |>
  modify_column_hide(c("ci")) |> 
  multinom_pivot_wider() |> 
  modify_footnote(everything() ~ NA) |>
  as_kable_extra(booktabs = TRUE) |> 
  kable_styling(full_width = FALSE, latex_options = c("scale_down")) |> 
  row_spec(0, bold = TRUE) |> # Met la ligne de header en gras
  footnote(
    general = c(
      "Source : Enquête Pratiques culturelles des Français, DEPS, 2018. Données non-pondérées.",
      "Champ : Ensemble des individus âgés de 15 à 29 ans résidant en France métropolitaine.",
      "Lecture : Avoir beaucoup de pratiques non-numériques par rapport par rapport à avoir peu de pratiques non-numériques implique « toutes choses égales par ailleurs » d’avoir 2,09 fois plus de chance de se trouver dans la classe des « omnivores » du numérique plutôt que la classe des « classiques-légitimes » du numérique, cela étant significatif au seuil de 5% (p-valeur < 0,05)."
    ),
    general_title = "",
    threeparttable = TRUE # Force la largeur de la légende et des footnotes à être de la largeur du tableau initial.
  )
