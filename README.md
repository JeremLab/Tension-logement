# 📊 Application Tension des Logements Étudiants

> Application Shiny pour l'analyse et le suivi de la tension locative dans les résidences étudiantes du Crous Lille Nord-Pas de Calais.

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7%2B-brightgreen.svg)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Status](https://img.shields.io/badge/status-demo-orange.svg)]()

---

## 💡 À propos de ce projet

**Ce dépôt est une démonstration du travail réalisé.**

- 🎯 **Objectif principal** : Montrer ce qui a été développé pour le Crous Lille
- 📂 Le **code source complet** est fourni pour que vous puissiez le tester
- ❌ Les **données réelles** ne sont **pas incluses** (confidentielles)
- ✅ **Tout le monde peut l'utiliser** pour tester l'application avec ses propres données

**Vous voulez tester l'application ?**  
Créez simplement un fichier Excel avec vos données (format détaillé ci-dessous) et lancez l'application !

---

## 📋 Table des matières

- [À propos de ce projet](#-à-propos-de-ce-projet)
- [Fonctionnalités](#-fonctionnalités)
- [Prérequis](#-prérequis)
- [Installation rapide](#-installation-rapide)
- [Créer vos données de test](#-créer-vos-données-de-test)
- [Lancer l'application](#️-lancer-lapplication)
- [Structure du projet](#-structure-du-projet)
- [Architecture du code](#-architecture-du-code)
- [Cas d'usage](#-cas-dusage)
- [Problèmes ?](#-problèmes-)
- [Licence](#-licence)
- [Auteur](#-auteur)
- [FAQ](#-faq)

---

## ✨ Fonctionnalités

Cette application dispose des fonctionnalités suivantes :

### 🗺️ Visualisation cartographique interactive
- Cartes avec marqueurs de tension par zone géographique
- Affichage simultané de plusieurs indicateurs (tension brute/réajustée)
- Export des visualisations
  
### 📈 Tableaux 
- **Indicateurs de performance** : Taux d'occupation, tension, concentration
- **Évolution temporelle** : Suivi sur plusieurs années

### 📊 Graphiques interactifs
- Graphiques à bulles pour identifier les zones de tension
- Évolution des demandes vs offre sur le temps
- Répartition de la population par critères sociaux

### 💾 Exports
- Tableaux Excel formatés
- Graphiques PNG 
- Archives ZIP 
---
## 🔧 Prérequis

### Logiciels
- **R** >= 4.0.0 ([Télécharger R](https://cran.r-project.org/))
- **RStudio** >= 2022.07.0 (optionnel mais recommandé) ([Télécharger RStudio](https://posit.co/download/rstudio-desktop/))

### Packages R
```r
install.packages(c(
  # Interface et visualisation
  "shiny", "bslib", "plotly", "ggplot2", "DT",
  # Manipulation de données
  "dplyr", "tidyr", "stringr",
  # Cartographie
  "sf", "ggrepel",
  # Import/Export
  "readxl", "writexl", "zip",
  # Utilitaires
  "scales"
))
```

---

## 🚀 Installation rapide

### 1️⃣ Cloner le projet

```bash
git clone https://github.com/JeremLab/tension-logements.git
cd tension-logements
```

### 2️⃣ Installer les dépendances

Ouvrir R ou RStudio et exécuter :
```r
install.packages(c(
  "shiny", "bslib", "plotly", "ggplot2", "DT",
  "dplyr", "tidyr", "stringr", "sf", "ggrepel",
  "readxl", "writexl", "zip", "scales"
))
```

### 3️⃣ Préparer vos données

Créez un fichier Excel avec vos données (voir format ci-dessous).

---

## 📊 Créer vos données de test

### ⚠️ IMPORTANT : Aucune donnée réelle fournie

Pour **tester l'application**, vous devez créer votre propre fichier Excel.

### Format du fichier Excel requis

Créez un fichier `.xlsx` avec ces colonnes :

| Colonne | Type | Description | Exemple |
|---------|------|-------------|---------|
| `Année de gestion` | Texte | Période d'analyse | "2025" |
| `Secteur` | Texte | Zone géographique | "Zone A", "Zone B" |
| `Sous-phase (Libellé)` | Texte | Phase du tour | "Tour 1", "Tour 2" |
| `INE` | Texte | Identifiant unique (anonymisé) | "ID001", "ID002" |
| `Résidence` | Texte | Nom de l'établissement | "Résidence Alpha" |
| `Places Tour X` | Nombre | Places proposées à ce tour | 420 |
| `Places Total` | Nombre | Places proposées | 420 |
| `Nombre logement` | Nombre | Capacité totale | 450 |
| `Places phase complémentaire` | Nombre | Places restantes | 35 |
| `Demandes renouvellement` | Nombre | Nombre de demandes de renouvellements | 69 |
| `Renouvellements confirmés` | Nombre | Renouvellements acceptés | 47 |
| `Taux d'acceptation` | Nombre |Renouvellements confirmés/demandes | 68,12 |
| `Echelon social` | Texte | Catégorie sociale | "0 bis" à "7", "Hors Barème" |
| `Sous-phase (Libellé)` | Texte | Phase d'attribution | "Tour 1", "Tour 2", etc. |
| `Latitude` | Nombre | Coordonnée GPS du secteur | 50.6292 |
| `Longitude` | Nombre | Coordonnée GPS du secteur | 3.0573 |

Note `Places Tour X` : pour chaque Sous-phase (Libellé), mettez le nombre de réservation pour le Tour correspondant et appliquer 0 pour les autres Places Tour

Exemple :	

| Sous-phase (Libellé) | Places Tour 1 | Places Tour 2 | Résidence |
|---------|------|-------------|---------|
| Tour 1 | 25 | 0 | X |
| Tour 2 | 0 | 25 | X |

### 💾 Exemple de données de test

Vous pouvez créer un petit jeu de test comme ceci :

```
| Année de gestion | Secteur  | Résidence         | INE     | Nombre logement | Places Total | ... | Latitude | Longitude |
|------------------|----------|-------------------|---------|-----------------|--------------|-----|----------|-----------|
| 2024-2025        | Zone A   | Résidence Alpha   | ID001   | 450             | 420          | ... | 50.6292  | 3.0573    |
| 2024-2025        | Zone A   | Résidence Alpha   | ID002   | 450             | 420          | ... | 50.6292  | 3.0573    |
| 2024-2025        | Zone B   | Résidence Beta    | ID003   | 200             | 185          | ... | 50.2919  | 2.7778    |
| 2024-2025        | Zone B   | Résidence Beta    | ID004   | 200             | 185          | ... | 50.2919  | 2.7778    |
| 2023-2024        | Zone A   | Résidence Alpha   | ID005   | 450             | 410          | ... | 50.6292  | 3.0573    |
```

**Information :**
- Chaque ligne = 1 demande d'un étudiant
- Un étudiant peut avoir plusieurs lignes s'il fait plusieurs demandes
- Les coordonnées sont identiques pour toutes les lignes d'un même secteur

---

## ▶️ Lancer l'application

1. Ouvrir `app.R` dans RStudio
2. Cliquer sur **"Run App"** (bouton vert ▶️)

### Utilisation

1. **📂 Importer** votre fichier Excel
2. **🎯 Sélectionner** les filtres (zones, année)
3. **📊 Explorer** les différents onglets
4. **💾 Exporter** vos résultats

---

## 📂 Structure du projet

```
tension-logements/
│
├── app.R                                    # Application Shiny
├── README.md                                # Ce fichier
├── LICENSE                                  # Licence MIT
├── arrondissements-59-nord.geojson          # Carte département Nord
├── arrondissements-62-pas-de-calais.geojson # Carte Pas-de-Calais
├── Latitude_Longitude                       # Fichier des coordonées GPS des secteurs
│
└── data/                                    # Vos données (à créer)
```

**📝 Note** : Les fichiers GeoJSON et altidutes fournis sont pour le Nord-Pas de Calais. Si vous testez avec une autre région, téléchargez les fichiers correspondants sur [France GeoJson](https://france-geojson.gregoiredavid.fr/)

---

## 🔬 Architecture du code

Le code comporte ~1300 lignes :

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
# Calculé 1 fois, réutilisé partout
```

#### 4. Paramètres dynamiques
```r
# S'adapte automatiquement au niveau d'agrégation
group_by(across(all_of(groupe_vars)))
```
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

## 🎓 Cas d'usage

### Ce que vous pouvez faire avec

- 📊 Tester avec vos propres données
- 🔧 Adapter à votre contexte spécifique
- 🎨 Personnaliser l'interface
- 🚀 Déployer en interne

---

## 🐛 Problèmes ?

### Erreurs fréquentes

#### "Cannot open file"
➡️ Vérifiez que votre fichier Excel est bien fermé avant import

#### "Column not found"
➡️ Vérifiez que toutes les colonnes requises sont présentes dans votre Excel

#### "Error in st_read"
➡️ Vérifiez que les fichiers GeoJSON sont dans le même dossier que `app.R`

### Besoin d'aide ?

- 🐙 **Issues GitHub** : [Ouvrir une issue](https://github.com/JeremLab/tension-logements/issues)
---

## 📜 Licence

Ce projet est sous licence **MIT** - voir le fichier [LICENSE](LICENSE).

**En résumé :**
- ✅ **Utilisation libre**
- ✅ **Modification autorisée**
- ✅ **Distribution autorisée**
- ✅ **Attribution requise** (mentionnez l'auteur original)
- ❌ **Aucune garantie**

```
MIT License

Copyright (c) 2025 Jérémie Dupont - Crous Lille Nord-Pas de Calais

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software...
```

---

## 👤 Auteur

**Jérémie Dupont**  
🎓 Développé dans le cadre d'un stage au Crous Lille Nord-Pas de Calais  
📧 Email : jeremie.dupont@crous-lille.fr  
🐙 GitHub : [@JeremLab](https://github.com/JeremLab)  

---

## 🙏 Remerciements

- **Crous Lille Nord-Pas de Calais** pour l'opportunité de développer ce projet
- **Communauté R/Shiny** pour les excellents packages open-source
- **Vous** qui testez cette application !

---

## 📚 Ressources utiles

### Documentation
- [📖 Shiny Documentation](https://shiny.rstudio.com/)
- [📊 dplyr](https://dplyr.tidyverse.org/)
- [🗺️ sf Package](https://r-spatial.github.io/sf/)

### Tutoriels
- [🎓 Mastering Shiny](https://mastering-shiny.org/) (gratuit)
- [📘 R for Data Science](https://r4ds.had.co.nz/)
- [💡 Shiny Best Practices](https://github.com/daattali/advanced-shiny)

---

### 📝 Changelog

### Version 1.0.0 (2025-01-16)
- ✨ Publication initiale
- 🗺️ Cartes interactives
- 📊 Tableaux d'analyse
- 📈 Graphiques temporels
- 👥 Analyse démographique
- 💾 Exports multiples
- ⚡ Code optimisé (-64%)

---

### 🔮 Roadmap

### Fonctionnalités futures envisagées
- 📱 Version responsive mobile
- 🔐 Authentification utilisateurs
- 🌍 Extension à d'autres Crous

## ❓ FAQ

### Puis-je utiliser ce code pour mon projet ?
**Oui !** C'est libre et gratuit (licence MIT). Adaptez-le à vos besoins.

### Où sont les données ?
**Non fournies.** Créez votre propre fichier Excel avec le format indiqué ci-dessus.

### Ça fonctionne avec mes données ?
**Oui**, tant qu'elles respectent le format Excel requis. Testez avec un petit échantillon d'abord.

### Puis-je modifier le code ?
**Absolument !** C'est même encouragé. La licence MIT vous autorise toutes modifications.

### Comment obtenir de l'aide ?
Ouvrez une [issue GitHub](https://github.com/JeremLab/tension-logements/issues) ou contactez-moi par email.

### L'application fonctionne-t-elle hors ligne ?
**Oui**, une fois les packages installés, tout fonctionne en local sans connexion internet.

---

<div align="center">

![R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Powered%20by-Shiny-2C8EBB?logo=rstudio&logoColor=white)

---

**Code libre • Testez avec vos données • Adaptez à vos besoins**

</div>
