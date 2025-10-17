##### Librairie -----------
library(shiny)
library(scales)
library(sf)
library(ggrepel)
library(bslib)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidyr)
library(plotly)
library(ggplot2)
library(DT)
library(zip)

####### Initialisation -------------
options(shiny.maxRequestSize = 50 * 1024^2) # 50 MB

dep59 <- st_read("arrondissements-59-nord.geojson", quiet = TRUE)
dep62 <- st_read("arrondissements-62-pas-de-calais.geojson", quiet = TRUE)
npdc <- rbind(dep59, dep62)

####### Fonctions de calcul -------------

# Fonction générique pour calculer les demandes
calculer_demandes_generique <- function(data, groupe_vars, filtre_echelon = FALSE, dernier_tour = FALSE, avec_residence = FALSE) {
  df <- data
  
  # Colonnes de base nécessaires
  cols_necessaires <- c("Année de gestion", "Secteur", "INE")
  if (avec_residence) cols_necessaires <- c(cols_necessaires, "Résidence")
  
  # Filtres
  df <- df %>% filter(if_all(all_of(cols_necessaires), ~ !is.na(.)))
  
  if (filtre_echelon) {
    df <- df %>% filter(!is.na(`Echelon social`), !(`Echelon social` %in% c("", "Hors Barème")))
  }
  
  if (dernier_tour) {
    df <- df %>%
      filter(!is.na(`Sous-phase (Libellé)`)) %>%
      mutate(num_tour = as.numeric(gsub("\\D", "", `Sous-phase (Libellé)`))) %>%
      group_by(across(all_of(groupe_vars))) %>%
      filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
      ungroup()
  }
  
  df %>%
    distinct(across(all_of(c(groupe_vars, "INE")))) %>%
    group_by(across(all_of(groupe_vars))) %>%
    summarise(n = n(), .groups = "drop")
}

# Fonction pour calculer les places
calculer_places <- function(data, type = "total", groupe_vars = c("Année de gestion", "Secteur")) {
  col_place <- if (type == "total") "Nombre logement" else "Places Total"
  col_result <- if (type == "total") "Logement total" else "Places proposées"
  
  data %>%
    filter(!is.na(`Secteur`), !is.na(!!sym(col_place)), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
    distinct(across(all_of(c(groupe_vars, "Résidence"))), .keep_all = TRUE) %>%
    group_by(across(all_of(groupe_vars))) %>%
    summarise(!!col_result := sum(!!sym(col_place), na.rm = TRUE), .groups = "drop")
}

# Fonction pour calculer le solde du dernier tour
calculer_solde_dernier_tour <- function(data, avec_residence = FALSE) {
  groupe_vars <- if (avec_residence) {
    c("Année de gestion", "Secteur", "Résidence")
  } else {
    c("Année de gestion", "Secteur")
  }
  
  data %>%
    filter(!is.na(INE), !is.na(`Sous-phase (Libellé)`)) %>%
    {if (avec_residence) filter(., !is.na(Résidence)) else .} %>%
    mutate(num_tour = as.numeric(str_extract(`Sous-phase (Libellé)`, "\\d+"))) %>%
    group_by(across(all_of(groupe_vars))) %>%
    filter(num_tour == max(num_tour, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(INE, across(all_of(c(groupe_vars, "num_tour")))) %>%
    group_by(across(all_of(c(groupe_vars, "num_tour")))) %>%
    summarise(Etudiants_demandeurs_dernier_tour = n(), .groups = "drop") %>%
    left_join(
      data %>%
        pivot_longer(cols = starts_with("Places Tour "), names_to = "tour", values_to = "places") %>%
        mutate(num_tour = as.numeric(str_extract(tour, "\\d+"))) %>%
        group_by(across(all_of(c(groupe_vars, "num_tour", "Résidence")))) %>%
        summarise(places_res = ifelse(all(is.na(places)), 0, max(places, na.rm = TRUE)), .groups = "drop") %>%
        {if (!avec_residence) group_by(., across(all_of(c(groupe_vars, "num_tour")))) %>% 
            summarise(places_res = sum(places_res, na.rm = TRUE), .groups = "drop") else .},
      by = c(groupe_vars, "num_tour")
    ) %>%
    mutate(`Étudiant sans affectation dernier tour` = Etudiants_demandeurs_dernier_tour - places_res) %>%
    select(-places_res, -num_tour)
}

# Fonction pour calculer la tension complète
calculer_tension_complete <- function(data, groupe_vars = c("Année de gestion", "Secteur")) {
  avec_residence <- "Résidence" %in% groupe_vars
  
  # Calculs des demandes
  demandes <- calculer_demandes_generique(data, groupe_vars, avec_residence = avec_residence) %>%
    rename(Demandes = n)
  
  demandes_tour4_filt <- calculer_demandes_generique(data, groupe_vars, 
                                                     filtre_echelon = TRUE, 
                                                     dernier_tour = TRUE, 
                                                     avec_residence = avec_residence) %>%
    rename(Demandes_tour_4_filtres = n)
  
  demandes_tour4 <- calculer_demandes_generique(data, groupe_vars, 
                                                dernier_tour = TRUE, 
                                                avec_residence = avec_residence) %>%
    rename(`Étudiants demandeurs dernier tour` = n)
  
  # Calculs des places
  places_total <- calculer_places(data, "total", groupe_vars)
  places_proposees <- calculer_places(data, "proposees", groupe_vars)
  
  # Places complémentaires
  places_compl <- data %>%
    filter(!is.na(`Secteur`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
    {if (avec_residence) filter(., !is.na(Résidence)) else .} %>%
    distinct(across(all_of(c(groupe_vars, "Places phase complémentaire")))) %>%
    group_by(across(all_of(groupe_vars))) %>%
    summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`), .groups = "drop")
  
  # Coordonnées (seulement pour secteur)
  if (!avec_residence) {
    coords <- data %>%
      filter(!is.na(`Secteur`), !is.na(`Latitude`), !is.na(`Longitude`), !is.na(`Année de gestion`)) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(Latitude = mean(Latitude, na.rm = TRUE), 
                Longitude = mean(Longitude, na.rm = TRUE), .groups = "drop")
  }
  
  # Assemblage
  tension <- demandes %>%
    left_join(places_total, by = groupe_vars) %>%
    left_join(places_proposees, by = groupe_vars) %>%
    left_join(demandes_tour4, by = groupe_vars) %>%
    left_join(places_compl, by = groupe_vars) %>%
    left_join(demandes_tour4_filt, by = groupe_vars)
  
  if (!avec_residence) {
    tension <- tension %>% left_join(coords, by = groupe_vars)
  }
  
  # Calcul des tensions et concentrations
  nom_concentration <- if (avec_residence) "Concentration résidence" else "Concentration secteur"
  
  tension %>%
    group_by(`Année de gestion`) %>%
    mutate(
      poids = Demandes / sum(Demandes),
      moyenne_poids = mean(poids, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      `Tension Brute` = round(Demandes / `Places proposées`, 2),
      `Tension réajustée` = round((Demandes_tour_4_filtres + (`Places proposées` - `Places phase complémentaire`)) / `Places proposées`, 2),
      !!nom_concentration := round(poids / moyenne_poids, 2),
      !!paste0(nom_concentration, " (%)") := round(poids * 100, 2)
    ) %>%
    select(-poids, -moyenne_poids)
}

# Fonction pour calculer les renouvellements
calculer_renouvellement <- function(data, groupe_vars = c("Année de gestion", "Secteur")) {
  avec_residence <- "Résidence" %in% groupe_vars
  
  renouvellement <- data %>%
    filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Année de gestion`)) %>%
    {if (avec_residence) filter(., !is.na(Résidence)) else .} %>%
    distinct(across(all_of(c(groupe_vars, "Résidence"))), .keep_all = TRUE) %>%
    group_by(across(all_of(groupe_vars))) %>%
    summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")
  
  places_total <- calculer_places(data, "total", groupe_vars)
  
  renouvellement %>%
    left_join(places_total, by = groupe_vars) %>%
    mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))
}

# Fonction pour créer une carte de tension
creer_carte_tension <- function(map_data, npdc, tension_type = "Brute", annee) {
  tension_col <- if (tension_type == "Brute") "Tension Brute" else "Tension réajustée"
  map_data$label <- paste0(map_data$Secteur, "\n", map_data[[tension_col]])
  
  map_data <- map_data %>%
    mutate(
      tension_classe = cut(
        !!sym(tension_col),
        breaks = c(-Inf, 1, 5, 8, 10, Inf),
        labels = c("≤1", "1 à 4", "5 à 7", "8 à 9", ">10"),
        right = FALSE
      ),
      tension_classe = factor(tension_classe, levels = c(">10", "8 à 9", "5 à 7", "1 à 4", "≤1"))
    )
  
  couleurs_tension <- c("≤1" = "#A8D38D", "1 à 4" = "#7BA05B", "5 à 7" = "#FFD990", 
                        "8 à 9" = "#FFA970", ">10" = "#BC2023")
  tailles_tension <- c("≤1" = 2, "1 à 4" = 4, "5 à 7" = 8, "8 à 9" = 12, ">10" = 16)
  
  ggplot() +
    geom_sf(data = npdc, fill = NA, color = "#004080", size = 0.4) +
    geom_point(
      data = map_data,
      aes(x = Longitude, y = Latitude, size = tension_classe, fill = tension_classe),
      color = "white", stroke = 0.5, shape = 21
    ) +
    geom_label_repel(
      data = map_data,
      aes(x = Longitude, y = Latitude, label = label),
      color = "white", fill = alpha("black", 0.6), size = 3.5, fontface = "bold",
      box.padding = 0.6, point.padding = 0.7, segment.color = "grey30",
      segment.size = 0.6, min.segment.length = 0
    ) +
    scale_fill_manual(values = couleurs_tension, name = paste("Tension", tolower(tension_type))) +
    scale_size_manual(values = tailles_tension, name = paste("Tension", tolower(tension_type))) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21, color = "white")),
      size = guide_legend(override.aes = list(shape = 21, color = "white"))
    ) +
    ggtitle(paste0("Tension ", tolower(tension_type), " par secteur en ", annee)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 11.5),
      axis.title = element_blank(), axis.text = element_blank(),
      axis.ticks = element_blank(), panel.grid = element_blank(),
      plot.margin = grid::unit(rep(0, 4), "lines")
    )
}

# Fonction pour générer le tableau final filtré
generer_tableau_final <- function(data, tension, renouvellement, solde, filtres_bassin, annee_selectionnee, avec_residence = FALSE) {
  groupe_vars <- if (avec_residence) {
    c("Année de gestion", "Secteur", "Résidence")
  } else {
    c("Année de gestion", "Secteur")
  }
  
  # Vérification des données d'entrée
  req(tension, renouvellement, solde)
  
  # Construction progressive
  result <- data %>%
    filter(!is.na(`Secteur`), !is.na(`Places Total`), !is.na(`INE`))
  
  if (avec_residence) {
    result <- result %>% filter(!is.na(Résidence))
  }
  
  result <- result %>%
    filter(
      tolower(Secteur) %in% tolower(filtres_bassin), 
      as.character(`Année de gestion`) == as.character(annee_selectionnee)
    ) %>%
    group_by(across(all_of(c(groupe_vars, "INE")))) %>%
    summarise(Total_Voeux = n(), .groups = "drop") %>%
    group_by(across(all_of(groupe_vars))) %>%
    summarise(Total_Distinct_INE = n_distinct(INE), .groups = "drop")
  
  # Jointures progressives
  if (nrow(result) > 0) {
    result <- result %>%
      left_join(tension, by = groupe_vars) %>%
      left_join(renouvellement, by = groupe_vars) %>%
      left_join(solde, by = groupe_vars)
    
    # Calculer le pourcentage non réservé
    if ("Places phase complémentaire" %in% names(result) && "Places proposées" %in% names(result)) {
      result <- result %>%
        mutate(`(%) Non réservé` = round(`Places phase complémentaire` / `Places proposées` * 100, 2))
    }
  }
  
  # Déterminer la colonne de concentration
  col_concentration <- if (avec_residence) "Concentration résidence (%)" else "Concentration secteur (%)"
  
  # Sélectionner les colonnes finales
  colonnes_finales <- c(
    groupe_vars,
    "Logement total",
    "Places proposées",
    "Places phase complémentaire",
    "(%) Non réservé",
    "Demandes",
    "Étudiants demandeurs dernier tour",
    "Étudiant sans affectation dernier tour",
    col_concentration,
    "Tension Brute",
    "Tension réajustée",
    "Renouvellement confirmé",
    "(%) du parc"
  )
  
  # Ne garder que les colonnes existantes
  colonnes_existantes <- colonnes_finales[colonnes_finales %in% names(result)]
  
  result <- result %>%
    select(all_of(colonnes_existantes))
  
  # Renommer Demandes
  if ("Demandes" %in% names(result)) {
    result <- result %>%
      rename(`Étudiants demandeurs` = Demandes)
  }
  
  return(result)
}

####### Interface -------------
ui <- fluidPage(
  tags$head(tags$script(HTML("window.localStorage.clear(); window.sessionStorage.clear();"))),
  div(
    style = "display: flex; align-items: center; padding: 10px; border-bottom: 1px solid #ccc;",
    tags$img(src = "https://irtshdf.fr/wp-content/uploads/2021/04/Crous-logo-lille-nord-pas-de-calais.png",
             height = "60px", style = "margin-right: 10px;"),
    tags$h1("Tension des logements étudiants")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Télécharger un fichier Excel", accept = c(".xls", ".xlsx")),
      actionButton("select", "Sélectionner / Désélectionner tout"),
      br(), br(),
      checkboxGroupInput("bassin", label = "Choix bassin :",
                         choices = c("Arras", "Béthune", "Boulogne-sur-Mer", "Calais", "Cambrai", "Dunkerque", 
                                     "Lens-Liévin", "Lille Centre", "Lille Est", "Longuenesse - Saint Omer", 
                                     "Maubeuge", "Roubaix - Tourcoing", "Valenciennes"),
                         selected = c("Arras", "Béthune", "Boulogne-sur-Mer", "Calais", "Cambrai", "Dunkerque", 
                                      "Lens-Liévin", "Lille Centre", "Lille Est", "Longuenesse - Saint Omer", 
                                      "Maubeuge", "Roubaix - Tourcoing", "Valenciennes")
      ),
      uiOutput("select_annee_ui"), br(),
      downloadButton("download_all_maps", label = "Télécharger toutes les cartes par bassin (zip)", icon = icon("download"))
    ),
    mainPanel(
      navset_card_underline(
        nav_panel("Accueil",
                  h2("Bienvenue sur l'application de suivi de la tension locative – Crous Lille Nord-Pas de Calais"),
                  p(tags$strong("Usage interne ", style = "font-size: 1.1em;"), br(), br(),
                    "Cette application est réservée aux personnels du Crous Lille Nord-Pas de Calais.",
                    "Les résultats produits sont confidentielles  et destinées à l'optimisation de la gestion du logement étudiant."),
                  p("L'outil permet de visualiser la tension sur le parc de logements étudiants, secteur par secteur, pour faciliter le pilotage de l'activité et la prise de décision."),
                  h3("Mode d'emploi"),
                  tags$ol(
                    tags$li(strong("Télécharger les données : "), "Importez le fichier Excel contenant les informations sur la tension locative"),
                    tags$li(strong("Sélectionner les critères : "), "Choisissez un ou plusieurs bassins géographiques et l'année universitaire à étudier."),
                    tags$li(strong("Explorer les résultats : "), "Naviguez dans les différents onglets pour affiner votre analyse.")
                  ),
                  h3("Navigation"),
                  tags$ul(
                    tags$li(strong("Secteur :"), " Dans cette page vous trouverez des cartes et tableaux récapitulatif de la tension tension par secteur. ", br(),
                            em("Note : "), "vous pouvez basculer entre la tension brute et réajustée en cliquant sur les boutons correspondants sous le graphique."), br(),
                    tags$li(strong("Résidence :"), " Analyse par résidence, classement des résidences selon la tension.", br(),
                            em("Note : "), "le graphique affiche le TOP 10 des résidences par tension."), br(),
                    tags$li(strong("Global :"), " Présentation des Évolutions globales de la tension, de la demande et de l'offre sur les différentes années"), br(),
                    tags$li(strong("Population :"), " Affichage de la population boursière demandeuse de logement  "), br(),
                    tags$li(strong("Glossaire :"), " Explications des principaux termes et méthodes de calcul utilisés dans les colonnes.")
                  ),
                  h3("Fonctionnalités"),
                  tags$ul(
                    tags$li("Tous les graphiques et tableaux s'adaptent automatiquement en fonction des filtres sélectionnés."),
                    tags$li("Possibilité de télécharger les tableaux et graphiques pour vos besoins de reporting interne."),
                    tags$li("Sélection et navigation rapides entre les différents secteurs d'analyse."),
                    tags$li("Affichage d'informations détaillées lors du survol de certains graphiques.")
                  ),
                  h3("Sécurité et confidentialité"),
                  p("Merci de respecter la confidentialité des données issues du fichier principal. 
            Aucune diffusion extérieure ou partage des résultats n'est autorisé sans validation de la Direction."), br(),
                  em("Application développée par - Jérémie Dupont - pour le Crous Lille Nord-Pas de Calais")
        ),
        nav_panel("Secteur",
                  div(style = "position: relative; width: 100%;",
                      plotOutput("NPDC", width = "100%", height = "500px"),
                      div(style = "position: absolute; top: 1px; left: 1px; z-index: 1;",
                          downloadButton("download_map", label = "Télécharger la carte", icon = icon("save"))),
                      div(style = "display: flex; justify-content: flex-end;",
                          actionButton("btn1", "Brute"), actionButton("btn2", "Réajustée")),
                      br(), br(),
                      downloadButton("download_tab", label = "Télécharger le tableau", icon = icon("save")),
                      br(), br(), DTOutput("preview")
                  )
        ),
        nav_panel("Résidence",
                  plotlyOutput("bubbleChart1", width = "100%", height = "400px"),
                  DTOutput("résidence")
        ),
        nav_panel("Global",
                  div(style = "display: inline-block;",
                      downloadButton("download_tab1", label = "Télécharger le graphique", icon = icon("save")),
                      plotlyOutput("graph2", width = "100%", height = "400px"), br(), br(),
                      div(style = "display: inline-block;",
                          downloadButton("download_tab2", label = "Télécharger le tableau", icon = icon("save"))),
                      br(), br(), DTOutput("global")
                  )
        ),
        nav_panel("Population",
                  plotlyOutput("graph_boursiers", width = "100%", height = "400px"),
                  br(), br(), DTOutput("boursiers")
        ),
        nav_panel("Glossaire",
                  h2("Documentation"),
                  p("Dans cette partie, vous trouverez un glossaire des principaux termes utilisés dans l'application."),
                  tags$ul(
                    tags$li(strong("Logement total : "), "Nombre total de logements étudiants disponibles au sein du CROUS."), br(),
                    tags$li(strong("Places proposées : "), "Nombre de places proposées lors du tour principal d'attribution des logements, après retrait des demandes de renouvellement, travaux, etc."), br(),
                    tags$li(strong("Places phase complémentaire : "), "Nombre de places restées non attribuées lors du tour principal, puis proposées lors de la phase complémentaire."), br(),
                    tags$li(strong("(% ) Non réservé : "), "Pourcentage de places restées sans attribution après la phase complémentaire, par rapport au nombre total de Places proposées."), br(),
                    tags$li(strong("Étudiants demandeurs : "), "Nombre d'étudiants distincts ayant formulé au moins une demande dans le secteur. Chaque étudiant n'est comptabilisé qu'une seule fois, quel que soit le nombre de demandes déposées pour le même secteur et sur l'ensemble des tours."), br(),
                    tags$li(strong("Étudiants demandeurs dernier tour : "), "Nombre d'étudiants ayant fait une demande lors du dernier tour d'attribution, selon le même principe que les Étudiants demandeurs."), br(),
                    tags$li(strong("Étudiants sans affectation dernier tour : "), "Nombre d'étudiants demandeurs du dernier tour se retrouvant sans affection, selon le même principe que les Étudiants demandeurs."), br(),
                    tags$li(strong("Concentration secteur (%) : "), "Part du secteur dans l'ensemble des demandes formulées à l'échelle du CROUS."), br(),
                    tags$li(strong("Tension brute : "), "Indicateur de pression de la demande : nombre de demandes rapporté au nombre de Places proposées au tour principal.", br(), br(),
                            em("Calcul : Demandes / Places proposées au tour")), br(),
                    tags$li(strong("Tension réajustée : "), "Indicateur prenant en compte la spécificité du dernier tour, en intégrant les places restées vacantes lors de la phase complémentaire.", br(), br(),
                            em("Calcul : (Étudiants demandeurs du dernier tour* + Places réservées**) / Places proposées au tour"), br(), br(),
                            em("*En excluant les candidats non boursiers ainsi que ceux dont les revenus dépassent les seuils"), br(),
                            em("** Places réservées = Places proposées au tour – Places non réservées par les étudiants")), br(),
                    tags$li(strong("Renouvellement confirmé : "), "Nombre d'étudiants ayant renouvelé leur logement pour l'année suivante."), br(),
                    tags$li(strong("(% ) du parc : "), "Pourcentage de logements du secteur occupés par des renouvellements, rapporté au total du parc."), br(),
                    tags$li(strong("Non-boursiers : "), "Étudiants ayant déposé une demande de DSE, mais n'ayant pas finalisé leur dossier."), br(),
                    tags$li(strong("Hors Barème : "), "Étudiants ayant complété leur DSE, mais dont les ressources dépassent le plafond requis pour bénéficier d'une bourse.")
                  )
        )
      )
    )
  )
)

###### Serveur -------------
server <- function(input, output, session) {
  # Données réactives
  data <- reactive({
    req(input$file)
    on.exit(unlink(input$file$datapath), add = TRUE)
    read_excel(input$file$datapath)
  })
  
  all_choices <- c("Arras", "Béthune", "Boulogne-sur-Mer", "Calais", "Cambrai", "Dunkerque", 
                   "Lens-Liévin", "Lille Centre", "Lille Est", "Longuenesse - Saint Omer", 
                   "Maubeuge", "Roubaix - Tourcoing", "Valenciennes")
  
  # Gestion de la sélection des bassins
  observeEvent(input$select, {
    if (setequal(input$bassin, all_choices)) {
      updateCheckboxGroupInput(session, "bassin", selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "bassin", selected = all_choices)
    }
  })
  
  # Sélection de l'année
  output$select_annee_ui <- renderUI({
    req(data())
    choices <- sort(unique(data()[["Année de gestion"]]), decreasing = TRUE)
    selectInput("select_annee", label = h3("Année de gestion"), choices = choices, selected = max(choices, na.rm = TRUE))
  })
  
  # Choix du type de graphique (Brute/Réajustée)
  graph_choice <- reactiveVal("Brute")
  observeEvent(input$btn1, { graph_choice("Brute") })
  observeEvent(input$btn2, { graph_choice("réajustée") })
  
  # Données calculées pour les secteurs
  tension_secteur <- reactive({
    req(data())
    calculer_tension_complete(data(), c("Année de gestion", "Secteur"))
  })
  
  renouvellement_secteur <- reactive({
    req(data())
    calculer_renouvellement(data(), c("Année de gestion", "Secteur"))
  })
  
  solde_secteur <- reactive({
    req(data())
    calculer_solde_dernier_tour(data(), avec_residence = FALSE)
  })
  
  places_compl_secteur <- reactive({
    req(data())
    data() %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Secteur`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`), .groups = "drop")
  })
  
  # Données calculées pour les résidences
  tension_residence <- reactive({
    req(data())
    calculer_tension_complete(data(), c("Année de gestion", "Secteur", "Résidence"))
  })
  
  renouvellement_residence <- reactive({
    req(data())
    calculer_renouvellement(data(), c("Année de gestion", "Secteur", "Résidence"))
  })
  
  solde_residence <- reactive({
    req(data())
    calculer_solde_dernier_tour(data(), avec_residence = TRUE)
  })
  
  places_compl_residence <- reactive({
    req(data())
    data() %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`, `Secteur`, `Résidence`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`), .groups = "drop")
  })
  
  #----------- Téléchargements -------
  
  # Téléchargement de toutes les cartes par bassin (ZIP)
  output$download_all_maps <- downloadHandler(
    filename = function() { paste0("cartes_par_bassin_", input$select_annee, ".zip") },
    content = function(file) {
      req(data(), input$select_annee)
      tmp_dir <- tempfile()
      dir.create(tmp_dir)
      
      tension <- tension_secteur()
      
      bassins <- list(
        "Lille" = c("Lille Centre", "Roubaix - Tourcoing", "Lille Est"),
        "Littoral" = c("Boulogne-sur-Mer", "Calais", "Longuenesse - Saint Omer", "Dunkerque"),
        "Artois" = c("Lens-Liévin", "Béthune", "Arras"),
        "Valenciennois" = c("Cambrai", "Valenciennes", "Maubeuge")
      )
      
      bbox_list <- list(
        "Lille" = c(xmin = 2.8, xmax = 3.35, ymin = 50.5, ymax = 50.83),
        "Littoral" = c(xmin = 1.5, xmax = 2.7, ymin = 50.5, ymax = 51.2),
        "Artois" = c(xmin = 2.3, xmax = 3.1, ymin = 50.22, ymax = 50.60),
        "Valenciennois" = c(xmin = 3.15, xmax = 4.1, ymin = 49.95, ymax = 50.55)
      )
      
      size_map <- c("≤1" = 2, "1 à 4" = 4, "5 à 7" = 8, "8 à 9" = 12, ">10" = 16)
      coef_lat <- 0.002
      coef_lon <- 0.002
      
      for (nom_bassin in names(bassins)) {
        secteurs_bassin <- bassins[[nom_bassin]]
        map_data <- tension %>%
          filter(Secteur %in% secteurs_bassin, as.character(`Année de gestion`) == as.character(input$select_annee))
        
        if (nrow(map_data) == 0) next
        
        map_data <- map_data %>%
          mutate(
            classe_brute = as.character(cut(`Tension Brute`, breaks = c(-Inf, 1, 5, 8, 10, Inf), 
                                            labels = c("≤1", "1 à 4", "5 à 7", "8 à 9", ">10"), right = FALSE)),
            classe_pond = as.character(cut(`Tension réajustée`, breaks = c(-Inf, 1, 5, 8, 10, Inf), 
                                           labels = c("≤1", "1 à 4", "5 à 7", "8 à 9", ">10"), right = FALSE)),
            brute_size = size_map[classe_brute],
            pond_size = size_map[classe_pond]
          )
        
        pts <- bind_rows(
          map_data %>% mutate(Type = "Brute", Tension = `Tension Brute`, 
                              Label = paste0(Secteur, "\nBrute\n", Tension)),
          map_data %>% mutate(
            Type = "Réajustée", Tension = `Tension réajustée`,
            Longitude = Longitude + coef_lon * (brute_size + pond_size) / (2 * cos(Latitude * pi / 180)),
            Latitude = Latitude - coef_lat * (brute_size + pond_size) / 2,
            Label = paste0(Secteur, "\nRéajustée\n", Tension)
          )
        ) %>%
          mutate(
            tension_classe = cut(Tension, breaks = c(-Inf, 1, 5, 8, 10, Inf), 
                                 labels = c("≤1", "1 à 4", "5 à 7", "8 à 9", ">10"), right = FALSE),
            tension_classe = factor(tension_classe, levels = c(">10", "8 à 9", "5 à 7", "1 à 4", "≤1"))
          )
        
        bbox_vals <- bbox_list[[nom_bassin]]
        
        p <- ggplot() +
          geom_sf(data = npdc, fill = NA, color = "#004080", size = 0.4) +
          geom_point(data = pts, aes(x = Longitude, y = Latitude, fill = tension_classe, size = tension_classe),
                     shape = 21, color = "white", stroke = 0.5) +
          geom_label_repel(data = pts, aes(x = Longitude, y = Latitude, label = Label),
                           color = "white", fill = alpha("black", 0.7), size = 3.5, fontface = "bold",
                           box.padding = 0.6, point.padding = 1.2, segment.color = "grey30",
                           segment.size = 0.6, min.segment.length = 0) +
          scale_fill_manual(values = c("≤1" = "#A8D38D", "1 à 4" = "#7BA05B", "5 à 7" = "#FFD990", 
                                       "8 à 9" = "#FFA970", ">10" = "#BC2023"), name = "Tension") +
          scale_size_manual(values = size_map, name = "Tension") +
          guides(fill = guide_legend(override.aes = list(shape = 21, color = "white")),
                 size = guide_legend(override.aes = list(shape = 21, color = "white"))) +
          coord_sf(xlim = c(bbox_vals["xmin"], bbox_vals["xmax"]), 
                   ylim = c(bbox_vals["ymin"], bbox_vals["ymax"]), expand = FALSE) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                legend.title = element_text(size = 12, face = "bold"),
                legend.text = element_text(size = 10),
                axis.title = element_blank(), axis.text = element_blank(),
                axis.ticks = element_blank(), panel.grid = element_blank())
        
        bassin_safe <- gsub("[^a-zA-Z0-9]", "_", iconv(nom_bassin, to = "ASCII//TRANSLIT"))
        file_path <- file.path(tmp_dir, paste0(bassin_safe, "_", input$select_annee, ".png"))
        ggsave(filename = file_path, plot = p, width = 16, height = 8, dpi = 150)
      }
      
      oldwd <- setwd(tmp_dir)
      on.exit(setwd(oldwd))
      all_pngs <- list.files(tmp_dir, pattern = "\\.png$", full.names = FALSE)
      zip::zip(zipfile = file, files = all_pngs)
    }
  )
  
  # Téléchargement de la carte secteur
  output$download_map <- downloadHandler(
    filename = function() { paste0("carte_tension_", input$select_annee, ".png") },
    content = function(file) {
      req(data(), input$select_annee, input$bassin)
      map_data <- tension_secteur() %>%
        filter(tolower(Secteur) %in% tolower(input$bassin), 
               as.character(`Année de gestion`) == as.character(input$select_annee))
      p <- creer_carte_tension(map_data, npdc, graph_choice(), input$select_annee)
      ggsave(filename = file, plot = p, width = 10, height = 8, dpi = 150)
    }
  )
  
  # Téléchargement du tableau secteur
  output$download_tab <- downloadHandler(
    filename = function() { paste0("Tableau_tension_secteur_", input$select_annee, ".xlsx") },
    content = function(file) {
      req(data(), input$select_annee, input$bassin)
      final <- generer_tableau_final(data(), tension_secteur(), renouvellement_secteur(), 
                                     solde_secteur(),input$bassin,
                                     input$select_annee, avec_residence = FALSE)
      write_xlsx(final, path = file)
    }
  )
  
  # Téléchargement du graphique global
  output$download_tab1 <- downloadHandler(
    filename = function() { "Graphique_evolution_globale.png" },
    content = function(file) {
      req(data(), input$bassin)
      filtered_data <- data() %>%
        filter(!is.na(`Secteur`), tolower(`Secteur`) %in% tolower(input$bassin))
      
      place_data <- filtered_data %>%
        filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Places proposées` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")
      
      demandes <- filtered_data %>%
        filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
        distinct(`Année de gestion`, `Secteur`, `INE`) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Étudiants demandeurs` = n(), .groups = "drop")
      
      demandes_tour4 <- calculer_demandes_generique(data(), c("Année de gestion"), dernier_tour = TRUE) %>%
        rename(`Étudiants demandeurs dernier tour` = n)
      
      demandes_tour4_filtrees <- calculer_demandes_generique(data(), c("Année de gestion"), 
                                                             filtre_echelon = TRUE, dernier_tour = TRUE) %>%
        rename(`Demandes_tour_4_filtrees` = n)
      
      places_complémentaire <- filtered_data %>%
        filter(!is.na(`Secteur`), !is.na(`Places phase complémentaire`), !is.na(`Année de gestion`)) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop")
      
      resume_annee <- place_data %>%
        left_join(demandes, by = "Année de gestion") %>%
        left_join(places_complémentaire, by = "Année de gestion") %>%
        left_join(demandes_tour4, by = "Année de gestion") %>%
        left_join(demandes_tour4_filtrees, by = "Année de gestion") %>%
        mutate(
          `Tension réajustée` = round((`Demandes_tour_4_filtrees` + (`Places proposées` - `Places phase complémentaire`)) / `Places proposées`, 2),
          `Tension brute` = round(`Étudiants demandeurs` / `Places proposées`, 2)
        ) %>%
        arrange(`Année de gestion`)
      
      d_long <- resume_annee %>%
        pivot_longer(cols = c(`Étudiants demandeurs`, `Étudiants demandeurs dernier tour`, `Places proposées`, `Places phase complémentaire`),
                     names_to = "type", values_to = "valeur")
      
      max_y <- max(d_long$valeur, na.rm = TRUE)
      max_tension <- max(resume_annee$`Tension réajustée`, resume_annee$`Tension brute`, na.rm = TRUE)
      
      p <- ggplot() +
        geom_col(data = d_long, aes(x = as.factor(`Année de gestion`), y = valeur, fill = type),
                 position = position_dodge(width = 0.8), width = 0.7) +
        geom_text(data = d_long, aes(x = as.factor(`Année de gestion`), y = valeur, label = valeur, group = type),
                  position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
        geom_line(data = resume_annee, aes(x = as.factor(`Année de gestion`), 
                                           y = `Tension réajustée` * max_y / max_tension, group = 1, color = "Tension réajustée"), size = 1.2) +
        geom_point(data = resume_annee, aes(x = as.factor(`Année de gestion`), 
                                            y = `Tension réajustée` * max_y / max_tension, color = "Tension réajustée"), size = 3) +
        geom_text(data = resume_annee, aes(x = as.factor(`Année de gestion`),
                                           y = `Tension réajustée` * max_y / max_tension + 0.03 * max_y,
                                           label = round(`Tension réajustée`, 2)), vjust = 4.5, size = 3, color = "black", fontface = "bold") +
        geom_line(data = resume_annee, aes(x = as.factor(`Année de gestion`), 
                                           y = `Tension brute` * max_y / max_tension, group = 1, color = "Tension brute"), size = 1.2, linetype = "dashed") +
        geom_point(data = resume_annee, aes(x = as.factor(`Année de gestion`), 
                                            y = `Tension brute` * max_y / max_tension, color = "Tension brute"), size = 3) +
        geom_text(data = resume_annee, aes(x = as.factor(`Année de gestion`),
                                           y = `Tension brute` * max_y / max_tension + 0.03 * max_y,
                                           label = round(`Tension brute`, 2)), vjust = 0, size = 3, color = "black", fontface = "bold") +
        scale_fill_manual(values = c("Étudiants demandeurs" = "#F4A280", "Étudiants demandeurs dernier tour" = "#F4A261",
                                     "Places proposées" = "#7EB3D5", "Places phase complémentaire" = "#7EB1E7"), name = "") +
        scale_color_manual(values = c("Tension réajustée" = "#3498db", "Tension brute" = "#004080"), name = "") +
        scale_y_continuous(name = "Demandes et places", breaks = seq(0, max_y, by = 2000),
                           labels = scales::label_number(scale = 1 / 1000, suffix = "k", accuracy = 1),
                           sec.axis = sec_axis(~ . * max_tension / max_y, breaks = seq(0, max_tension, by = 0.5),
                                               name = "Tension réajustée / Tension brute")) +
        labs(title = "Évolution des demandes, places et tensions par année", x = "Année de gestion") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
              axis.title.y = element_text(size = 13),
              axis.title.y.right = element_text(size = 13, color = "black"),
              legend.position = "bottom")
      
      ggsave(filename = file, plot = p, width = 10, height = 7, dpi = 150)
    }
  )
  
  # Téléchargement du tableau global
  output$download_tab2 <- downloadHandler(
    filename = function() { "Tableau_global.xlsx" },
    content = function(file) {
      req(data(), input$bassin)
      filtered_data <- data() %>%
        filter(!is.na(`Secteur`), tolower(`Secteur`) %in% tolower(input$bassin))
      
      places_total <- calculer_places(data(), "total", "Année de gestion")
      place_data <- filtered_data %>%
        filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Places proposées` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")
      
      demandes <- filtered_data %>%
        filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
        distinct(`Année de gestion`, `Secteur`, `INE`) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Étudiants demandeurs` = n(), .groups = "drop")
      
      demandes_tour4 <- calculer_demandes_generique(data(), "Année de gestion", dernier_tour = TRUE) %>%
        rename(`Étudiants demandeurs dernier tour` = n)
      
      places_complémentaire <- filtered_data %>%
        filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
        distinct(`Année de gestion`, `Résidence`, `Places phase complémentaire`) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop")
      
      renouvellement_data <- data() %>%
        filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
        distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
        group_by(`Année de gestion`) %>%
        summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")
      
      renouvellement_data1 <- renouvellement_data %>%
        left_join(places_total, by = "Année de gestion") %>%
        mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))
      
      demandes_tour4_filtrees <- calculer_demandes_generique(data(), "Année de gestion", 
                                                             filtre_echelon = TRUE, dernier_tour = TRUE) %>%
        rename(`Demandes_tour_4_filtrees` = n)
      
      resume_annee <- place_data %>%
        left_join(places_total, by = "Année de gestion") %>%
        left_join(demandes, by = "Année de gestion") %>%
        left_join(demandes_tour4, by = "Année de gestion") %>%
        left_join(places_complémentaire, by = "Année de gestion") %>%
        left_join(renouvellement_data1[, c("Année de gestion", "Renouvellement confirmé", "(%) du parc")], by = "Année de gestion") %>%
        left_join(demandes_tour4_filtrees, by = "Année de gestion") %>%
        arrange(`Année de gestion`) %>%
        mutate(
          `Tension brute` = round(`Étudiants demandeurs` / `Places proposées`, 2),
          `Tension réajustée` = round((`Demandes_tour_4_filtrees` + (`Places proposées` - `Places phase complémentaire`)) / `Places proposées`, 2),
          `Évolution demandes (%)` = round((`Étudiants demandeurs` / lag(`Étudiants demandeurs`) - 1) * 100, 1),
          `Évolution places (%)` = round((`Places proposées` / lag(`Places proposées`) - 1) * 100, 1),
          `(%) Non réservé` = round(`Places phase complémentaire` / `Places proposées` * 100, 2)
        )
      
      final1 <- resume_annee %>%
        select(`Année de gestion`, `Logement total`, `Places proposées`, `Places phase complémentaire`,
               `(%) Non réservé`, `Étudiants demandeurs`, `Étudiants demandeurs dernier tour`, `Tension brute`,
               `Tension réajustée`, `Évolution places (%)`, `Évolution demandes (%)`, `Renouvellement confirmé`, `(%) du parc`)
      
      writexl::write_xlsx(final1, path = file)
    }
  )
  
  #----------- Graphiques et tableaux -----------
  
  # Cartographie
  output$NPDC <- renderPlot({
    req(data(), input$select_annee, input$bassin)
    
    map_data <- tension_secteur() %>%
      filter(
        tolower(Secteur) %in% tolower(input$bassin), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      )
    
    if (nrow(map_data) == 0) {
      plot.new()
      title(main = "Aucune donnée à afficher")
    } else {
      creer_carte_tension(map_data, npdc, graph_choice(), input$select_annee)
    }
  }, res = 80)
  
  # Graphique bulles résidences
  output$bubbleChart1 <- renderPlotly({
    req(data(), input$select_annee, input$bassin)
    
    tension_data1 <- tension_residence() %>%
      filter(
        tolower(Secteur) %in% tolower(input$bassin), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      ) %>%
      arrange(desc(`Tension Brute`)) %>%
      slice_head(n = 10)
    
    p1 <- ggplot(tension_data1, aes(
      x = Demandes, y = `Tension Brute`, size = `Tension Brute`, color = Résidence,
      text = paste("Année :", `Année de gestion`, "<br>Résidence :", Résidence,
                   "<br>Places:", `Places proposées`, "<br>Demandes:", Demandes, "<br>Tension:", `Tension Brute`)
    )) +
      geom_point(alpha = 0.6, position = position_jitter(width = 0.25, height = 0)) +
      scale_size(range = c(2, 10)) +
      scale_y_continuous(trans = "log10") +
      labs(x = "Nombre de demande", y = "Taux de tension", color = "Résidence", size = NULL,
           title = "Taux de tension des 10 résidences les plus demandées par année") +
      theme_minimal()
    
    ggplotly(p1, tooltip = "text")
  })
  
  # Graphique global
  output$graph2 <- renderPlotly({
    req(data(), input$bassin)
    filtered_data <- data() %>%
      filter(!is.na(`Secteur`), tolower(`Secteur`) %in% tolower(input$bassin))
    
    place_data <- filtered_data %>%
      filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places proposées` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")
    
    demandes <- filtered_data %>%
      filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      distinct(`Année de gestion`, `Secteur`, `INE`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Étudiants demandeurs` = n(), .groups = "drop")
    
    demandes_tour4 <- calculer_demandes_generique(data(), "Année de gestion", dernier_tour = TRUE) %>%
      rename(`Étudiants demandeurs dernier tour` = n)
    
    demandes_tour4_filtrees <- calculer_demandes_generique(data(), "Année de gestion", 
                                                           filtre_echelon = TRUE, dernier_tour = TRUE) %>%
      rename(`Demandes_tour_4_filtrees` = n)
    
    places_complémentaire <- filtered_data %>%
      filter(!is.na(`Secteur`), !is.na(`Places phase complémentaire`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop")
    
    resume_annee <- place_data %>%
      left_join(demandes, by = "Année de gestion") %>%
      left_join(places_complémentaire, by = "Année de gestion") %>%
      left_join(demandes_tour4_filtrees, by = "Année de gestion") %>%
      left_join(demandes_tour4, by = "Année de gestion") %>%
      mutate(
        `Tension réajustée` = round((`Demandes_tour_4_filtrees` + (`Places proposées` - `Places phase complémentaire`)) / `Places proposées`, 2),
        `Tension brute` = round(`Étudiants demandeurs` / `Places proposées`, 2)
      ) %>%
      arrange(`Année de gestion`)
    
    plot_ly(resume_annee, x = ~ factor(`Année de gestion`)) %>%
      add_bars(y = ~`Étudiants demandeurs`, name = "Étudiants demandeurs", marker = list(color = "#F4A280"),
               text = ~`Étudiants demandeurs`, textposition = "outside", hoverinfo = "none") %>%
      add_bars(y = ~`Étudiants demandeurs dernier tour`, name = "Demandes dernier tour", marker = list(color = "#F4A261"),
               text = ~`Étudiants demandeurs dernier tour`, textposition = "outside", hoverinfo = "none") %>%
      add_bars(y = ~`Places proposées`, name = "Places proposées", marker = list(color = "#7EB3D5"),
               text = ~`Places proposées`, textposition = "outside", hoverinfo = "none") %>%
      add_bars(y = ~`Places phase complémentaire`, name = "Places phase complémentaire", marker = list(color = "#7EB1E7"),
               text = ~`Places phase complémentaire`, textposition = "outside", hoverinfo = "none") %>%
      add_lines(y = ~`Tension réajustée`, name = "Tension réajustée", yaxis = "y2",
                line = list(color = "#3498db", width = 3), mode = "lines+markers",
                marker = list(color = "#3498db", size = 8),                text = ~ paste("Tension réajustée :", `Tension réajustée`), hoverinfo = "text") %>%
      add_lines(y = ~`Tension brute`, name = "Tension brute", yaxis = "y2",
                line = list(color = "#004080", width = 3, dash = "dot"), mode = "lines+markers",
                marker = list(color = "#004080", size = 8),
                text = ~ paste("Tension brute :", `Tension brute`), hoverinfo = "text") %>%
      layout(
        barmode = "group",
        title = list(text = "Évolution des demandes, places et tensions par année", font = list(size = 18)),
        xaxis = list(title = "Année de gestion", tickangle = -45, titlefont = list(size = 14), tickfont = list(size = 12)),
        yaxis = list(title = "Demandes et places", titlefont = list(size = 14), tickfont = list(size = 12),
                     showgrid = TRUE, gridcolor = "rgba(200,200,200,0.3)"),
        yaxis2 = list(title = "Tensions", overlaying = "y", side = "right", showgrid = FALSE,
                      titlefont = list(size = 14), tickfont = list(size = 12), title_standoff = 70, automargin = TRUE),
        legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center")
      )
  })
  
  # Graphique boursiers
  output$graph_boursiers <- renderPlotly({
    req(data(), input$select_annee, input$bassin)
    
    boursiers_filtrés <- data() %>%
      filter(!is.na(INE), !is.na(`Echelon social`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      filter(
        as.character(`Secteur`) %in% as.character(input$bassin), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      ) %>%
      mutate(`Echelon social` = as.character(`Echelon social`)) %>%
      group_by(INE) %>%
      slice_max(order_by = as.numeric(`Echelon social`), n = 1, with_ties = FALSE) %>%
      ungroup()
    
    boursiers_data <- boursiers_filtrés %>%
      group_by(`Secteur`, `Echelon social`) %>%
      summarise(`Étudiants demandeurs` = n(), .groups = "drop") %>%
      tidyr::complete(`Secteur`, `Echelon social`, fill = list(`Étudiants demandeurs` = 0)) %>%
      tidyr::pivot_wider(names_from = `Echelon social`, values_from = `Étudiants demandeurs`, values_fill = 0)
    
    etudiants_totaux <- data() %>%
      filter(
        !is.na(INE), 
        !is.na(Secteur), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      ) %>%
      filter(as.character(Secteur) %in% as.character(input$bassin)) %>%
      distinct(Secteur, INE) %>%
      count(Secteur, name = "INE_total")
    
    echelon_cols <- setdiff(names(boursiers_data), "Secteur")
    boursiers_data <- boursiers_data %>%
      left_join(etudiants_totaux, by = "Secteur")
    somme_echelons <- rowSums(boursiers_data[, echelon_cols], na.rm = TRUE)
    boursiers_data <- boursiers_data %>%
      mutate(`Non-boursiers` = pmax(INE_total - somme_echelons, 0)) %>%
      select(-INE_total)
    
    donut_data <- boursiers_data %>%
      summarise(across(-Secteur, sum)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "Echelon social", values_to = "Étudiants demandeurs") %>%
      filter(`Étudiants demandeurs` > 0)
    
    ordre_echelons <- c("0 Bis", "1", "2", "3", "4", "5", "6", "7", "Non-boursiers", "Hors Barème")
    donut_data$`Echelon social` <- factor(
      donut_data$`Echelon social`,
      levels = ordre_echelons[ordre_echelons %in% donut_data$`Echelon social`]
    )
    donut_data <- donut_data[order(donut_data$`Echelon social`), ]
    
    plot_ly(
      donut_data,
      labels = ~`Echelon social`,
      values = ~`Étudiants demandeurs`,
      type = "pie",
      hole = 0.6,
      text = ~ paste0("<b>", round(100 * `Étudiants demandeurs` / sum(`Étudiants demandeurs`), 1), "%</b>"),
      textinfo = "text",
      textposition = "inside",
      sort = FALSE,
      textfont = list(size = 14, color = "black", family = "Arial"),
      marker = list(line = list(color = "#FFFFFF", width = 1)),
      hoverinfo = "none"
    ) %>%
      layout(
        title = list(text = "<b>Répartition des boursiers selon l'échelon social</b>", x = 0.50, xanchor = "center", font = list(size = 18)),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 0.8, xanchor = "left", y = 0.5, yanchor = "middle",
                      font = list(size = 13), title = list(text = "<b>Échelon</b>", font = list(size = 14))),
        margin = list(l = 10, r = 10, b = 10, t = 70),
        annotations = list(
          list(text = paste0("Total<br>", sum(donut_data$`Étudiants demandeurs`)), x = 0.5, y = 0.5,
               font = list(size = 14, color = "black"), showarrow = FALSE, xref = "paper", yref = "paper")
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
  })
  
  # Tableau secteurs
  output$preview <- renderDT({
    req(data(), input$select_annee, input$bassin)
    req(tension_secteur(), renouvellement_secteur(), solde_secteur())
    
    final <- generer_tableau_final(
      data(), 
      tension_secteur(),
      renouvellement_secteur(),
      solde_secteur(),
      input$bassin, 
      input$select_annee, 
      avec_residence = FALSE
    )
    
    datatable(final, escape = FALSE, 
              options = list(
                columnDefs = list(list(className = "dt-center", targets = "_all")),
                pageLength = 25,
                scrollX = TRUE
              ),
              rownames = FALSE, 
              class = "stripe hover")
  })
  
  # Tableau résidences
  output$résidence <- renderDT({
    req(data(), input$select_annee, input$bassin)
    req(tension_residence(), renouvellement_residence(), solde_residence())
    
    final <- generer_tableau_final(
      data(), 
      tension_residence(),
      renouvellement_residence(),
      solde_residence(),
      input$bassin, 
      input$select_annee, 
      avec_residence = TRUE
    )
    
    datatable(final, escape = FALSE,
              options = list(
                columnDefs = list(list(className = "dt-center", targets = "_all")),
                pageLength = 25,
                scrollX = TRUE
              ),
              rownames = FALSE, 
              class = "stripe hover")
  })
  
  # Tableau global
  output$global <- renderDT({
    req(data(), input$bassin)
    filtered_data <- data() %>%
      filter(!is.na(`Secteur`), tolower(`Secteur`) %in% tolower(input$bassin))
    
    places_total <- calculer_places(data(), "total", "Année de gestion")
    place_data <- filtered_data %>%
      filter(!is.na(`Places Total`), !is.na(`Année de gestion`), !is.na(`Secteur`), !is.na(`Résidence`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, .keep_all = TRUE) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places proposées` = sum(`Places Total`, na.rm = TRUE), .groups = "drop")
    
    demandes <- filtered_data %>%
      filter(!is.na(`INE`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      distinct(`Année de gestion`, `Secteur`, `INE`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Étudiants demandeurs` = n(), .groups = "drop")
    
    demandes_tour4 <- calculer_demandes_generique(data(), "Année de gestion", dernier_tour = TRUE) %>%
      rename(`Étudiants demandeurs dernier tour` = n)
    
    places_complémentaire <- filtered_data %>%
      filter(!is.na(`Secteur`), !is.na(`Résidence`), !is.na(`Année de gestion`), !is.na(`Places phase complémentaire`)) %>%
      distinct(`Année de gestion`, `Résidence`, `Places phase complémentaire`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Places phase complémentaire` = sum(`Places phase complémentaire`, na.rm = TRUE), .groups = "drop")
    
    renouvellement_data <- data() %>%
      filter(!is.na(`Secteur`), !is.na(`Nombre logement`), !is.na(`Résidence`), !is.na(`Année de gestion`)) %>%
      distinct(`Année de gestion`, `Secteur`, `Résidence`, `Renouvellement confirmé`, `Nombre logement`) %>%
      group_by(`Année de gestion`) %>%
      summarise(`Renouvellement confirmé` = sum(`Renouvellement confirmé`, na.rm = TRUE), .groups = "drop")
    
    renouvellement_data1 <- renouvellement_data %>%
      left_join(places_total, by = "Année de gestion") %>%
      mutate(`(%) du parc` = round(`Renouvellement confirmé` / `Logement total` * 100, 2))
    
    demandes_tour4_filtrees <- calculer_demandes_generique(data(), "Année de gestion", 
                                                           filtre_echelon = TRUE, dernier_tour = TRUE) %>%
      rename(`Demandes_tour_4_filtrees` = n)
    
    resume_annee <- place_data %>%
      left_join(places_total, by = "Année de gestion") %>%
      left_join(demandes, by = "Année de gestion") %>%
      left_join(demandes_tour4, by = "Année de gestion") %>%
      left_join(places_complémentaire, by = "Année de gestion") %>%
      left_join(renouvellement_data1[, c("Année de gestion", "Renouvellement confirmé", "(%) du parc")], by = "Année de gestion") %>%
      left_join(demandes_tour4_filtrees, by = "Année de gestion") %>%
      arrange(`Année de gestion`) %>%
      mutate(
        `Tension brute` = round(`Étudiants demandeurs` / `Places proposées`, 2),
        `Tension réajustée` = round((`Demandes_tour_4_filtrees` + (`Places proposées` - `Places phase complémentaire`)) / `Places proposées`, 2),
        `Évolution demandes (%)` = round((`Étudiants demandeurs` / lag(`Étudiants demandeurs`) - 1) * 100, 1),
        `Évolution places (%)` = round((`Places proposées` / lag(`Places proposées`) - 1) * 100, 1),
        `(%) Non réservé` = round(`Places phase complémentaire` / `Places proposées` * 100, 2)
      )
    
    final1 <- resume_annee %>%
      select(`Année de gestion`, `Logement total`, `Places proposées`, `Places phase complémentaire`,
             `(%) Non réservé`, `Étudiants demandeurs`, `Étudiants demandeurs dernier tour`, `Tension brute`,
             `Tension réajustée`, `Évolution places (%)`, `Évolution demandes (%)`, `Renouvellement confirmé`, `(%) du parc`)
    
    datatable(final1,
              options = list(columnDefs = list(list(className = "dt-center", targets = "_all"))),
              rownames = FALSE, class = "stripe hover")
  })
  
  # Tableau boursiers
  output$boursiers <- renderDT({
    req(data(), input$select_annee, input$bassin)
    
    boursiers_filtrés <- data() %>%
      filter(!is.na(INE), !is.na(`Echelon social`), !is.na(`Année de gestion`), !is.na(`Secteur`)) %>%
      filter(
        tolower(`Secteur`) %in% tolower(input$bassin), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      ) %>%
      mutate(`Echelon social` = as.character(`Echelon social`)) %>%
      group_by(INE) %>%
      slice_max(order_by = as.numeric(`Echelon social`), n = 1, with_ties = FALSE) %>%
      ungroup()
    
    boursiers_data <- boursiers_filtrés %>%
      group_by(`Secteur`, `Echelon social`) %>%
      summarise(`Étudiants demandeurs` = n(), .groups = "drop") %>%
      tidyr::complete(`Secteur`, `Echelon social`, fill = list(`Étudiants demandeurs` = 0)) %>%
      pivot_wider(names_from = `Echelon social`, values_from = `Étudiants demandeurs`, values_fill = 0)
    
    etudiants_totaux <- data() %>%
      filter(
        !is.na(INE), 
        !is.na(Secteur), 
        as.character(`Année de gestion`) == as.character(input$select_annee)
      ) %>%
      filter(tolower(Secteur) %in% tolower(input$bassin)) %>%
      distinct(Secteur, INE) %>%
      count(Secteur, name = "INE_total")
    
    echelon_cols <- setdiff(names(boursiers_data), "Secteur")
    boursiers_data <- boursiers_data %>%
      left_join(etudiants_totaux, by = "Secteur")
    somme_echelons <- rowSums(boursiers_data[, echelon_cols], na.rm = TRUE)
    boursiers_data <- boursiers_data %>%
      mutate(`Non-boursiers` = pmax(INE_total - somme_echelons, 0)) %>%
      select(-INE_total)
    
    cols_total <- setdiff(names(boursiers_data), "Secteur")
    boursiers_data <- boursiers_data %>%
      mutate(Total = rowSums(select(., all_of(cols_total)), na.rm = TRUE))
    
    ligne_totale <- boursiers_data %>%
      summarise(across(-Secteur, sum)) %>%
      mutate(Secteur = "Total") %>%
      select(names(boursiers_data))
    
    boursiers_final <- bind_rows(boursiers_data, ligne_totale)
    
    datatable(boursiers_final,
              options = list(columnDefs = list(list(className = "dt-center", targets = "_all"))),
              rownames = FALSE, class = "stripe hover")
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)