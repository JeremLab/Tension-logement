# 📊 Application Tension des Logements Étudiants

> Application Shiny pour l'analyse et le suivi de la tension locative dans les résidences étudiantes du Crous Lille Nord-Pas de Calais.

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7%2B-brightgreen.svg)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

---

## 📋 Table des matières

- [À propos](#à-propos)
- [Fonctionnalités](#fonctionnalités)
- [Captures d'écran](#captures-décran)
- [Prérequis](#prérequis)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Structure du projet](#structure-du-projet)
- [Optimisations techniques](#optimisations-techniques)
- [Licence](#licence)
- [Auteur](#auteur)

---

## 📖 À propos

Cette application permet au Crous Lille Nord-Pas de Calais de :
- **Visualiser** la tension locative par secteur géographique et par résidence
- **Analyser** l'évolution des demandes et de l'offre de logements sur plusieurs années
- **Optimiser** la gestion du parc de logements étudiants
- **Piloter** les décisions stratégiques grâce à des indicateurs clés

### Contexte

Développée en 2025 dans le cadre d'un stage au Crous Lille Nord-Pas de Calais, cette application répond aux besoins de suivi et d'analyse de la tension locative sur l'ensemble des 13 secteurs géographiques de la région.

**⚠️ Usage interne :** Cette application traite des données confidentielles et est destinée exclusivement aux personnels autorisés du Crous.

---

## ✨ Fonctionnalités

### 🗺️ Visualisation cartographique
- Cartes interactives de la tension par secteur
- Affichage simultané de la tension brute et réajustée
- Export des cartes par bassin géographique (ZIP)

### 📈 Analyses statistiques
- **Tension brute** : Demandes / Places proposées
- **Tension réajustée** : Prend en compte les places non réservées et les demandes filtrées
- Concentration des demandes par secteur/résidence
- Évolution temporelle sur plusieurs années

### 🏠 Indicateurs par niveau
- **Secteur** : 13 bassins géographiques (Lille, Arras, Valenciennes, etc.)
- **Résidence** : Analyse détaillée par établissement
- **Global** : Vue d'ensemble régionale

### 👥 Population étudiante
- Répartition des boursiers par échelon social (0 bis à 7)
- Identification des étudiants non-boursiers et hors barème
- Graphiques interactifs (donut chart)

### 📥 Exports
- Tableaux Excel (.xlsx)
- Graphiques PNG haute résolution
- Archives ZIP de cartes multiples

---

## 🖼️ Captures d'écran

### Interface principale
```
┌─────────────────────────────────────────────────────────┐
│  🏫 CROUS Lille   Tension des logements étudiants      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  📂 Fichier Excel  [Choisir un fichier...]              │        
│  ☑️  Bassins                                            │
│        ☑ Arras                                          │
│        ☑ Lille                                          │
│        ☑ Valenciennes....                               │
│  📅 Année de gestion                                    │
│         [2025 ▼]                                        │
│                                                         │
│  ┌──────────────────────────────────────────────────┐   │
│  │  🗺️  Carte de tension par secteur               │   │
│  │                                                  │   │
│  │      [Carte interactive avec points colorés]     │   │
│  │                                                  │   │
│  └──────────────────────────────────────────────────┘   │
│                                                         │
│  📊 Tableau détaillé par secteur                       │
│  ┌──────────────────────────────────────────────────┐   │
│  │ Secteur │ Places │ Demandes │ Tension │ ...      │   │
│  │ Arras   │   450  │   892    │  1.98   │ ...      │   │
│  └──────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

---

## 🔧 Prérequis

### Logiciels requis
- **R** >= 4.0.0
- **RStudio** (recommandé) >= 2022.07.0

### Packages R nécessaires
```r
# Visualisation et interface
shiny (>= 1.7.0)
bslib
plotly
ggplot2
DT

# Manipulation de données
dplyr
tidyr
stringr

# Cartographie
sf
ggrepel

# Import/Export
readxl
writexl
zip

# Autres
scales
```

---

## 🚀 Installation

### 1. Cloner le dépôt

```bash
git clone https://github.com/JeremLab/tension-logements.git
cd tension-logements
```

### 2. Installer les dépendances R

```r
# Installer tous les packages nécessaires
install.packages(c(
  "shiny", "bslib", "plotly", "ggplot2", "DT",
  "dplyr", "tidyr", "stringr",
  "sf", "ggrepel",
  "readxl", "writexl", "zip",
  "scales"
))
```

### 3. Vérifier les fichiers géographiques

Assurez-vous que ces fichiers sont présents dans le répertoire racine :
- `arrondissements-59-nord.geojson`
- `arrondissements-62-pas-de-calais.geojson`

---

## 📂 Structure du projet

```
tension-logements-crous/
│
├── app.R                                    # Application Shiny principale (~1000 lignes)
├── README.md                                # Ce fichier
├── LICENSE                                  # Licence MIT
│
├── arrondissements-59-nord.geojson         # Carte département Nord
├── arrondissements-62-pas-de-calais.geojson # Carte Pas-de-Calais
│── logo-crous.png
```

---

## 💻 Utilisation

### Démarrage de l'application

1. Ouvrir `app.R` dans RStudio
2. Cliquer sur **"Run App"** (bouton vert en haut à droite)

### Workflow d'utilisation

1. **📂 Importer les données**
   - Cliquer sur "Télécharger un fichier Excel"
   - Sélectionner le fichier de données du Crous (.xlsx)

2. **🎯 Sélectionner les filtres**
   - Choisir les bassins géographiques à analyser
   - Sélectionner l'année universitaire

3. **📊 Explorer les onglets**
   - **Accueil** : Documentation et mode d'emploi
   - **Secteur** : Cartes et tableaux par secteur
   - **Résidence** : Analyse détaillée par résidence
   - **Global** : Évolution sur plusieurs années
   - **Population** : Répartition des boursiers
   - **Glossaire** : Définitions des indicateurs

4. **💾 Exporter les résultats**
   - Télécharger les cartes (PNG)
   - Exporter les tableaux (Excel)
   - Télécharger toutes les cartes (ZIP)

---

## 📊 Format des données

### Colonnes Excel requises

Le fichier Excel importé doit contenir les colonnes suivantes :

| Colonne | Type | Description | Obligatoire |
|---------|------|-------------|-------------|
| `Année de gestion` | Texte | Ex: "2024-2025" | ✅ |
| `Secteur` | Texte | Bassin géographique | ✅ |
| `Résidence` | Texte | Nom de la résidence | ✅ |
| `INE` | Texte | Identifiant étudiant | ✅ |
| `Nombre logement` | Numérique | Capacité totale | ✅ |
| `Places Total` | Numérique | Places proposées | ✅ |
| `Places phase complémentaire` | Numérique | Places non réservées | ✅ |
| `Renouvellement confirmé` | Numérique | Renouvellements | ✅ |
| `Echelon social` | Texte | Échelon boursier (0 bis à 7) | ✅ |
| `Sous-phase (Libellé)` | Texte | Tour d'attribution | ✅ |
| `Latitude` | Numérique | Coordonnées GPS | ✅ |
| `Longitude` | Numérique | Coordonnées GPS | ✅ |
| `Places Tour 1`, `Places Tour 2`, etc. | Numérique | Places par tour | ⚠️ |

⚠️ **Important :** Ne jamais inclure de données nominatives dans les exports !

---

## 🔬 Optimisations techniques

### Architecture du code

Le code comporte ~1000 lignes** :

#### 1. Fonctions génériques réutilisables
```r
# 1 fonction générique
calculer_demandes_generique(data, groupe_vars, 
                            filtre_echelon = FALSE, 
                            dernier_tour = FALSE)
```

#### 2. Composition fonctionnelle
```r
# Petites fonctions qui s'assemblent
calculer_tension_complete() {
  demandes <- calculer_demandes_generique(...)
  places <- calculer_places(...)
  # Assemble intelligemment
}
```

#### 3. Reactive values pour éviter recalculs
```r
tension_secteur <- reactive({
  calculer_tension_complete(data(), c("Année de gestion", "Secteur"))
})
```

#### 4. Paramètres dynamiques
```r
# S'adapte automatiquement au niveau d'agrégation
group_by(across(all_of(groupe_vars)))
```

### Performances

- ✅ **Calculs optimisés** : Pas de duplication
- ✅ **Réactivité Shiny** : Mise à jour intelligente
- ✅ **Mémoire** : Gestion efficace des données
- ✅ **Chargement** : Limite de 50 MB

---

## 📈 Indicateurs calculés

### Tension brute
```
Tension brute = Étudiants demandeurs / Places proposées
```
Indicateur simple de la pression de la demande.

### Tension réajustée
```
Tension réajustée = (Demandes dernier tour filtrées* + Places réservées**) / Places proposées
```
*Hors non-boursiers et hors barème  
**Places proposées - Places phase complémentaire

### Concentration
```
Concentration secteur (%) = (Demandes secteur / Demandes totales) × 100
```

### Taux de renouvellement
```
(%) du parc = (Renouvellements confirmés / Logements totaux) × 100
```

---

## 🐛 Signaler un bug

Si vous rencontrez un problème :

1. Vérifier qu'il n'existe pas déjà dans les [Issues](https://github.com/JeremLab/tension-logements/issues)
2. Créer une nouvelle issue avec :
   - Description claire du problème
   - Étapes de reproduction
   - Version de R et des packages
   - Messages d'erreur (si applicable)

---

## 📜 Licence

Ce projet est sous licence **MIT** - voir le fichier [LICENSE](LICENSE) pour plus de détails.

```
MIT License

Copyright (c) 2025 Jérémie Dupont - Crous Lille Nord-Pas de Calais

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction...
```

---

## 👤 Auteur

**Jérémie Dupont**  
📧 Email : jeremie.dupont@crous-lille.fr  
🏢 Organisation : Crous Lille Nord-Pas de Calais  
💼 LinkedIn : [jeremie-dupont](https://linkedin.com/in/jeremie-dupont)  
🐙 GitHub : [@JeremLab](https://github.com/JeremLab)

---

## 🙏 Remerciements

- **Crous Lille Nord-Pas de Calais** pour l'opportunité de développer cet outil
- **Équipe logement** pour les retours et tests utilisateurs
- **Communauté R/Shiny** pour les packages open-source

---

## 📚 Ressources

### Documentation
- [Shiny by RStudio](https://shiny.rstudio.com/)
- [dplyr Documentation](https://dplyr.tidyverse.org/)
- [ggplot2 Documentation](https://ggplot2.tidyverse.org/)

### Guides utiles
- [Shiny Best Practices](https://github.com/daattali/advanced-shiny)
- [R for Data Science](https://r4ds.had.co.nz/)
- [Mastering Shiny](https://mastering-shiny.org/)

---

## 📝 Changelog

### Version 1.0.0 (2025-01-16)
- ✨ Version initiale
- 🗺️ Cartes interactives par secteur
- 📊 Tableaux d'analyse détaillés
- 📈 Graphiques d'évolution temporelle
- 👥 Analyse de la population boursière
- 💾 Exports multiples (Excel, PNG, ZIP)
- ⚡ Code optimisé (-64% de lignes)

---

## 🔮 Roadmap

### Fonctionnalités futures envisagées
- [ ] 🔐 Authentification utilisateurs
- [ ] 📊 Dashboard temps réel
- [ ] 🌍 Extension à d'autres Crous

---

<div align="center">

**Développé pour le Crous Lille Nord-Pas de Calais**

![R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Powered%20by-Shiny-2C8EBB?logo=rstudio&logoColor=white)


</div>
