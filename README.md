# 📊 Analyse du comportement de crédit des clients bancaires

Ce projet vise à analyser et modéliser le comportement de crédit des clients d’une banque à l’aide de données sociodémographiques, financières et comportementales. L’objectif principal est de prédire si un client est susceptible d’obtenir ou d’être en défaut de crédit.

## Objectifs

- Construire un modèle de scoring pour prédire le comportement de crédit.
- Identifier les variables les plus influentes dans l’octroi ou le défaut de crédit.
- Améliorer les performances via le nettoyage, la transformation et l’enrichissement des données.
- Comparer différentes approches de traitement du déséquilibre des classes (SMOTE, sous-échantillonnage).

## Données

Le jeu de données contient 33 663 clients et 21 variables décrivant :
- Informations sociodémographiques (âge, sexe, situation familiale, etc.)
- Données financières (revenus, crédits, échéances…)
- Historique de crédit (ancienneté, canaux de souscription…)
- Moyens de contact (email, téléphone…)

## Méthodologie

1. **Exploration initiale** : analyse descriptive, détection d’outliers, visualisation des distributions.
2. **Nettoyage** : gestion des valeurs manquantes et des incohérences.
3. **Création d’indicateurs dérivés** :
   - Taux d’endettement
   - Montant moyen par crédit
   - Ancienneté moyenne…
4. **Discrétisation** : transformation des variables continues en classes.
5. **Modélisation** :
   - Régression logistique (modèles bruts et discrétisés)
   - Évaluation via précision, rappel, AUC
6. **Rééquilibrage des classes** : SMOTE et sous-échantillonnage.

## Résultats

| Version | Précision | Rappel | AUC |
|--------|-----------|--------|-----|
| Modèle brut | 55.6 % | 16.9 % | 0.7268 |
| Données nettoyées | 78.2 % | 95.6 % | 0.7353 |
| + Indicateurs dérivés | 78 % | 96 % | 0.740 |
| + Variables discrétisées | 78 % | 96 % | 0.743 |
| Sous-échantillonnage | 37.6 % | 62.3 % | 0.7173 |
| SMOTE | 14.7 % | 41.9 % | 0.7185 |

## Recommandations

- Enrichir le modèle avec des données dynamiques (transactions, navigation…).
- Explorer des algorithmes plus complexes (XGBoost, Random Forest…).
- Mettre en place une segmentation client pour des modèles spécialisés.
- Utiliser les modèles comme outils d’aide à la décision.

## Auteur

**Kenson FAVEUR**  
Date du projet : 02/05/2025

---

📘 Rapport complet disponible dans `Rapport projet final Banque.pdf`
