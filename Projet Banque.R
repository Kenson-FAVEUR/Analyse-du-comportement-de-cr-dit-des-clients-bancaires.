
# Partie I: Appréhender les données
# Charger les packages nécessaires
library(tidyverse)      # Manipulation des données et graphiques
library(conflicted)     # Gestion des conflits de noms de fonctions entre packages
library(skimr)          # Statistiques descriptives
library(questionr)      # Statistiques descriptives
library(ggstats)        # Statistiques descriptives
library(BioStatR)       # Mesures de liaison pour les variables quantitatives (rapport de corrélation)
library(DescTools)      # Mesures de liaison pour les variables qualitatives (Cramer, Tschuprow)
library(FactoMineR)     # Caractérisation des classes
library(UBL)            # Réequilibrage
library(naniar)         # Gestion des valeurs manquantes
library(zoo)            # Gestion des valeurs manquantes
library(VIM)            # Gestion des valeurs manquantes
library(woeBinning)     # Discrétisation supervisée
library(smbinning)      # Discrétisation supervisée
library(caret)          # Modélisation - Général (nombreux outils pour la mod?lisation)
library(pROC)           # Modélisation - Général (courbe ROC)
library(car)            # Modélisation - Régression logistique
library(emmeans)        # Modélisation - Régression logistique
library(blorr)          # Modélisation - Régression logistique
library(broom)          # Modélisation - Régression logistique
library(rpart)          # Modélisation - Arbre de décision
library(rpart.plot)     # Modélisation - Arbre de décision
library(randomForest)   # Modélisation - Forets aléatoires

# Définir le repertoire de travail
setwd("C:/Users/PC/OneDrive/Bureau/Vue d'ensemble/Sources")

# Lire le fichier
Banque <- read.table("Banque.txt", sep = ";", header = TRUE)

# Vérification des variables
skim(Banque)

# Convertir en facteurs
Banque$sexe <- as.factor(Banque$sexe)
Banque$situation_familiale <- as.factor(Banque$situation_familiale)
Banque$statut_logement <- as.factor(Banque$statut_logement)
Banque$canal_premier_credit <- as.factor(Banque$canal_premier_credit)
Banque$canal_dernier_credit <- as.factor(Banque$canal_dernier_credit)
Banque$flag_credit <- as.factor(Banque$flag_credit)
Banque$flag_tel <- as.factor(Banque$flag_tel)
Banque$flag_email <- as.factor(Banque$flag_email)


# Résumé statistique.
summary(Banque)

# Vérifier l'équilibre de la variable à expliquer
# Calculer la fréquence brute
table(Banque$flag_credit)
prop.table(table(Banque$flag_credit))

# Visualisation 
library(ggplot2)
ggplot(Banque, aes(x = flag_credit)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution de flag_credit", x = "Flag Credit", y = "Nombre d'observations") +
  theme_minimal()

# Corrélation avec quelques variables
# Convertir flag_credit en numérique pour corrélation
Banque$flag_credit_num <- as.numeric(as.character(Banque$flag_credit))

# Corrélation age ~ flag_credit
cor.test(Banque$age, Banque$flag_credit_num, method = "kendall")

# Corrélation revenu ~ flag_credit
cor.test(Banque$revenu, Banque$flag_credit_num, method = "kendall")

# Vérifier la variable Banque
str(Banque)

# Partie II: Modèle Brut
# Construction du model logistique
# Choix des variables explicatives
vars_model <- c(
   "age", "sexe",  "situation_familiale", "statut_logement",
  "revenu", "flag_tel",  "flag_email",
  "nb_credits_total", "mt_credits_total",
  "nb_credits_actuel", "mt_credits_actuel",
  "mt_echeances_actuel","duree_remboursement_actuel", "mt_premier_credit", "anc_premier_credit", "canal_premier_credit", "mt_dernier_credit", "anc_dernier_credit",
  "canal_dernier_credit"
)

# Construction du modèle
# Formule dynamique à partir du vecteur de variables choisies
formule <- as.formula(paste("flag_credit~", paste(vars_model, collapse = "+")))

# Modèle logistique
rl <- glm(formula = formule,
          data = Banque,
          family = "binomial")

# Diagnostique du modèle
## Test de significativité globale du modèle (test du rapport de vraisemblance)
pchisq(q = rl$null.deviance - rl$deviance,
       df = rl$df.null - rl$df.residual,
       lower.tail = FALSE)

## R² de McFadden (~ R² en régression linéaire)
1 - (rl$deviance/(-2)) / (rl$null.deviance/(-2))

## Utilité de chaque variable
Anova(rl, type = 3)

# Reconstruire le modèle sans les variables non significatives
# Choix des variables explicatives en gardant uniquement les variables significatives
vars_importantes <- c(
  "age", "sexe", "situation_familiale", "statut_logement", "flag_email",
  "nb_credits_total", "nb_credits_actuel", "mt_credits_actuel",
  "mt_echeances_actuel", "duree_remboursement_actuel",
  "anc_premier_credit", "canal_premier_credit",
  "mt_dernier_credit", "canal_dernier_credit"
)


# Construction du modèle
# Formule dynamique à partir du vecteur de variables choisies
formule1 <- as.formula(paste("flag_credit~", paste(vars_importantes, collapse = "+")))

# Modèle logistique
r2 <- glm(formula = formule1,
          data = Banque,
          family = "binomial")


# Diagnostique du modèle
## Test de significativité globale du modèle (test du rapport de vraisemblance)
pchisq(q = r2$null.deviance - r2$deviance,
       df = r2$df.null - r2$df.residual,
       lower.tail = FALSE)

## R² de McFadden (~ R² en régression linéaire)
1 - (r2$deviance/(-2)) / (r2$null.deviance/(-2))

## Utilité de chaque variable
Anova(r2, type = 3)


# Résumé du modèle r2
summary(r2)

# Comparaison des modalité pour chaque variable catégorielle
library(emmeans)

# Comparaisons multiples sur les variables catégorielles
pairs(emmeans(r2,~ sexe))
pairs(emmeans(r2,~ canal_premier_credit))
pairs(emmeans(r2, ~ situation_familiale))
pairs(emmeans(r2, ~ statut_logement))
pairs(emmeans(r2, ~ canal_dernier_credit))
pairs(emmeans(r2,~ flag_email))

# Regroupement des modalités
Banque2 <- Banque %>%
  mutate(
    # Regroupement de situation_familiale
    situation_familiale = fct_collapse(
      situation_familiale,
      "Non marié" = c("Célibataire", "Divorcé"),
      "Marié ou assimilé" = c("Marié", "Union libre")
    ),
    
    # Sexe reste "F" et "M" (aucun regroupement ici)
    sexe = fct_collapse(
      sexe,
      "Homme" = c("M"),
      "Femme" = c("F")
    ),
    
    # Regroupement des canaux pour premier et dernier crédit
    canal_premier_credit = fct_collapse(
      canal_premier_credit,
      "Direct" = c("Agence", "Partenaire"),
      "Mailing" = c("Mailing")
    ),
    
    canal_dernier_credit = fct_collapse(
      canal_dernier_credit,
      "Direct" = c("Agence", "Partenaire"),
      "Mailing" = c("Mailing")
    ),
    
    # Regroupement de statut_logement
    statut_logement = fct_collapse(
      statut_logement,
      "Locataire" = c("Locataire"),
      "Propriétaire" = c("Propriétaire")
    )
  )


# Relancer le modèle à nouveau
r2_bis <- glm(formula = formule1, family = "binomial", data = Banque2)

# Résultats détaillés du modèle final
## Test de significativité globale du modèle (test du rapport de vraisemblance)

pchisq(q = r2_bis$null.deviance - rl$deviance,
       df = r2_bis$df.null - r2_bis$df.residual,
       lower.tail = FALSE)


## R² de McFadden (<=> R² en régression linéaire)
1 - (r2_bis$deviance/(-2)) / (r2_bis$null.deviance/(-2))

## Utilité de chaque variable
Anova(r2_bis, type = 3)

## Vérifier la multicolinéarité
vif(r2_bis)

# Calcul des VIF pour le modèle
vif_r2_bis <- vif(r2_bis)

# Transforme les résultats VIF en dataframe et renomme les colonnes
df_vif <- as.data.frame(vif_r2_bis) %>% 
  rename(vif = "GVIF^(1/(2*Df))") %>% 
  rownames_to_column(var = "variable")

# Création du graphique
ggplot(data = df_vif) +
  aes(x = vif, y = reorder(variable, vif)) +  # Trie les variables par VIF
  geom_col(fill = "steelblue4") +  # Ajoute les barres verticales
  geom_vline(xintercept = 5, linewidth = 1, color = "indianred", linetype = "dashed") +  # Ligne de seuil à VIF=5
  labs(x = "VIF", y = "Variables") +  # Titres des axes
  theme_minimal()  # Applique un thème minimal


# Calculer les indicateurs de performence de ce modèle
Banque2 <- r2_bis$model  # récupère les données exactes utilisées dans le modèle
Banque2$proba <- predict(r2_bis, type = "response")
Banque2$pred <- ifelse(Banque2$proba >= 0.5, 1, 0)

# Calcul de précision et Rappel
table_pred <- table(Prediction = Banque2$pred, Réel = Banque2$flag_credit)

# Précision = VP / (VP + FP)
precision_val <- table_pred["1", "1"] / sum(table_pred["1", ])
# Rappel = VP / (VP + FN)
recall_val <- table_pred["1", "1"] / sum(table_pred[, "1"])

precision_val
recall_val

# Calcul AUC
roc_obj <- roc(Banque2$flag_credit, Banque2$proba)
auc_val <- auc(roc_obj)
auc_val

# Choisir un seuil
seuil <- 0.5

# Nouvelle prédiction binaire
Banque2$pred_05 <- ifelse(Banque2$proba >= seuil, 1, 0)

# Matrice de confusion
table_pred_05 <- table(Prediction = Banque2$pred_05, Réel = Banque2$flag_credit)

# Calculs
precision_05 <- table_pred_05["1", "1"] / sum(table_pred_05["1", ])
recall_05 <- table_pred_05["1", "1"] / sum(table_pred_05[, "1"])

# Résumé du modèle
summary(r2_bis)

# Affichage
precision_05
recall_05
auc_val

# Calcul de l'objet ROC
roc_obj <- roc(Banque2$flag_credit, Banque2$proba)

# Tracer la courbe ROC
plot(roc_obj, col = "#2C77B2", lwd = 3, main = "Courbe ROC", legacy.axes = TRUE)
 
# Ligne de hasard (diagonale)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Ajouter l'AUC sur le graphique
auc_val <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_val, 3)),
       col = "#2C77B2", lwd = 3, bty = "n")

# Partie III: Néttoyer les données
# Afficher les valeurs manquantes
colSums(is.na(Banque))
# Visualiser lignes avec les valeurs manquantes
Banque[!complete.cases(Banque), ]

# Nombre total des valeurs manquantes
sum(is.na(Banque))

# Traiter les valeurs manquantes
Banque$revenu <- ave(Banque$revenu, Banque$sexe, FUN = function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
Banque$duree_remboursement_actuel[is.na(Banque$duree_remboursement_actuel)] <- median(Banque$duree_remboursement_actuel, na.rm = TRUE)
Banque$anc_premier_credit[is.na(Banque$anc_premier_credit)] <- median(Banque$anc_premier_credit, na.rm = TRUE)
Banque$anc_dernier_credit[is.na(Banque$anc_dernier_credit)] <- median(Banque$anc_dernier_credit, na.rm = TRUE)

# Vérification après traitement
colSums(is.na(Banque))

# Construction du model logistique
# Choix des variables explicatives
vars_model1 <- c(
  "age", "sexe", "situation_familiale", "statut_logement", "flag_email",
  "nb_credits_total", "nb_credits_actuel", "mt_credits_actuel",
  "mt_echeances_actuel", "duree_remboursement_actuel",
  "anc_premier_credit", "canal_premier_credit",
  "mt_dernier_credit", "canal_dernier_credit"
)

# Construction du modèle après néttoyage des données
# Formule dynamique à partir du vecteur de variables choisies
formule <- as.formula(paste("flag_credit~", paste(vars_model1, collapse = "+")))

# Modèle logistique 2
model2 <- glm(formula = formule,
          data = Banque,
          family = "binomial")

# Diagnostique du modèle
## Test de significativité globale du modèle (test du rapport de vraisemblance)
pchisq(q = model2$null.deviance - model2$deviance,
       df = rl$df.null - rl$df.residual,
       lower.tail = FALSE)

## R² de McFadden (~ R² en régression linéaire)
1 - (rl$deviance/(-2)) / (rl$null.deviance/(-2))

## Utilité de chaque variable
Anova(model2, type = 3)

# Résumé du modèle
summary(model2)


# Calculer les indicateurs de performances
# Prédire les probabilités du modèle
predictions_prob <- predict(model2, type = "response")

# Convertir les probabilités en prédictions binaires (seuil = 0.5)
predictions_binaires <- ifelse(predictions_prob > 0.5, 1, 0)

# Calculer la précision (accuracy)
library(caret)
conf_matrix <- confusionMatrix(factor(predictions_binaires), factor(Banque$flag_credit))
precision <- conf_matrix$overall['Accuracy']
print(paste("Précision : ", precision))

# Calculer le rappel (recall)
recall <- conf_matrix$byClass['Sensitivity']
print(paste("Rappel : ", recall))

# Calculer l'AUC (Area Under the ROC Curve)
library(pROC)
roc_curve <- roc(Banque$flag_credit, predictions_prob)
auc_value <- auc(roc_curve)
print(paste("AUC : ", auc_value))


## Partie IV: Construire des nouveaux indicateurs
# Construction des nouveaux indicateurs

# Charger les bibliothèques nécessaires
library(dplyr)
library(forcats)
library(caret)
library(pROC)

# Créer les 5 nouveaux indicateurs
Banque3 <- Banque %>%
  mutate(
    # 1. Taux d'endettement
    endettement = mt_echeances_actuel / mt_credits_actuel,
    
    # 2. Montant moyen par crédit actif
    mt_moyen_credit_actuel = mt_credits_actuel / nb_credits_actuel,
    
    # 3. Durée moyenne par crédit
    duree_moyenne_credit = duree_remboursement_actuel / nb_credits_actuel,
    
    # 4. Ratio du dernier crédit sur le total
    dernier_credit_ratio = mt_dernier_credit / mt_credits_actuel,
    
    # 5. Ancienneté moyenne par crédit
    anciennete_par_credit = anc_premier_credit / nb_credits_total
  )

# Statistique de ma nouvelle base
skim(Banque3)


# Vérifier s'il y a pas des valeurs manquantes
total_missings <- sum(is.na(Banque3))
print(paste("Nombre total de valeurs manquantes : ", total_missings))

# Vérifier les colonnes qui ont des valeurs manquantes
colSums(is.na(Banque3))

# Imputer les valeurs manquantes par la médiane
Banque3$mt_moyen_credit_actuel[is.na(Banque3$mt_moyen_credit_actuel)] <- 
  median(Banque3$mt_moyen_credit_actuel, na.rm = TRUE)

Banque3$duree_moyenne_credit[is.na(Banque3$duree_moyenne_credit)] <- 
  median(Banque3$duree_moyenne_credit, na.rm = TRUE)

Banque3$endettement[is.na(Banque3$endettement)] <- 
  median(Banque3$endettement, na.rm = TRUE)

# Vérifier s'il y a des valeurs manquantes
total_missings <- sum(is.na(Banque3))
print(paste("Nombre total de valeurs manquantes : ", total_missings))

# Mettre à jour les variables du modèle avec les nouvelles variables explicatives
vars_model2 <- c(
  "age", "sexe", "situation_familiale", "statut_logement", "flag_email",
  "nb_credits_total", "nb_credits_actuel", "mt_credits_actuel",
  "mt_echeances_actuel", "duree_remboursement_actuel",
  "anc_premier_credit", "canal_premier_credit",
  "mt_dernier_credit", "canal_dernier_credit" , "endettement","mt_moyen_credit_actuel", "duree_moyenne_credit", "dernier_credit_ratio","anciennete_par_credit"
)

# Créer la formule du modèle avec les nouvelles variables
formule2 <- as.formula(paste("flag_credit ~", paste(vars_model2, collapse = " + ")))

# Vérifier si chaque colonne contient des valeurs infinies
sapply(Banque3, function(x) sum(is.infinite(x)))

# Vérifier si chaque colonne contient des valeurs NaN
sapply(Banque3, function(x) sum(is.nan(x)))

# Remplacer les valeurs infinies par NA
Banque3[is.infinite(Banque3$endettement), "endettement"] <- NA
Banque3[is.infinite(Banque3$duree_moyenne_credit), "duree_moyenne_credit"] <- NA
Banque3[is.infinite(Banque3$dernier_credit_ratio), "dernier_credit_ratio"] <- NA


# Relancer le modèle avec les nouvelles variables explicatives
model3 <- glm(formule2, family = "binomial", data = Banque3)

# Résumé du modèle
summary(model3)


# Calculer les indicateurs de performances

# Prédictions sur l'ensemble de données
predictions <- predict(model3, newdata = Banque3, type = "response")
predictions_class <- ifelse(predictions > 0.5, 1, 0)  # Prédictions binaires (1 ou 0)

# Matrice de confusion
conf_matrix <- confusionMatrix(as.factor(predictions_class), as.factor(Banque3$flag_credit))

# Précision
precision <- conf_matrix$overall['Accuracy']
print(paste("Précision : ", precision))

# Rappel
recall <- conf_matrix$byClass['Sensitivity']
print(paste("Rappel : ", recall))

# Calcul de l'AUC (Area Under the ROC Curve)
roc_curve <- roc(Banque3$flag_credit, predictions)
auc_value <- auc(roc_curve)
print(paste("AUC : ", auc_value))

# Partie V: Optimiser les variables explicatives

# Discrétiser plusieurs variables continues
variables_continues <- c("age", "revenu", "nb_credits_total", "mt_credits_total", 
                         "nb_credits_actuel", "mt_credits_actuel", "mt_echeances_actuel", 
                         "duree_remboursement_actuel", "mt_premier_credit", 
                         "anc_premier_credit", "mt_dernier_credit", "anc_dernier_credit", 
                         "endettement", "mt_moyen_credit_actuel", "duree_moyenne_credit", 
                         "dernier_credit_ratio", "anciennete_par_credit")

# Vérifier les quantiles et ne discrétiser que si les seuils sont uniques
for (var in variables_continues) {
  # Calcul des quantiles
  q <- quantile(Banque3[[var]], probs = 0:4/4, na.rm = TRUE)
  
  # Vérifier si les seuils sont uniques
  if (length(unique(q)) == length(q)) {
    Banque3[[paste0(var, "_discret")]] <- cut(Banque3[[var]],
                                              breaks = q,
                                              include.lowest = TRUE,
                                              labels = c("faible", "moyenne", "élevée", "très élevée"))
  } else {
    message(paste("Variable ignorée pour discrétisation (valeurs de découpage non uniques) :", var))
  }
}


# Sélectionner les variables discrétisées qui ont été créées
vars_discret <- paste0(setdiff(variables_continues, c("nb_credits_total", "nb_credits_actuel")), "_discret")

# Ajouter les variables qualitatives existantes
vars_model_discret <- c("sexe", "situation_familiale", "statut_logement", "flag_email", vars_discret)

# Construire la formule
formule_discret <- as.formula(paste("flag_credit ~", paste(vars_model_discret, collapse = " + ")))


## Relancer le modèle
model_discret <- glm(formule_discret, family = "binomial", data = Banque3)
summary(model_discret)

# Calculer les indicateurs de performances
# Identifier les lignes sans NA dans les variables du modèle
vars_utilisees <- all.vars(formule_discret)
donnees_model <- Banque3[complete.cases(Banque3[, vars_utilisees]), ]

# Prédiction sur les données nettoyées
pred_probs_discret <- predict(model_discret, newdata = donnees_model, type = "response")
pred_class_discret <- ifelse(pred_probs_discret > 0.5, 1, 0)

# Calcul des métriques
library(caret)
conf_matrix_discret <- confusionMatrix(factor(pred_class_discret), factor(donnees_model$flag_credit))
precision <- conf_matrix_discret$overall["Accuracy"]
recall <- conf_matrix_discret$byClass["Sensitivity"]

# AUC
library(pROC)
roc_curve_discret <- roc(donnees_model$flag_credit, pred_probs_discret)
auc_discret <- auc(roc_curve_discret)

# Affichage
print(paste("Précision :", round(precision, 4)))
print(paste("Rappel :", round(recall, 4)))
print(paste("AUC :", round(auc_discret, 4)))

# Partie VI : Rééquilibrer la variable à expliquer
# Chargement des packages
library(caret)
library(ROSE)
library(pROC)
library(smotefamily)
set.seed(42)

# Sélection des variables
vars_interet <- c("flag_credit", "age", "revenu", "nb_credits_total", "mt_credits_total", 
                  "nb_credits_actuel", "mt_credits_actuel", "mt_echeances_actuel", 
                  "duree_remboursement_actuel", "mt_premier_credit", "anc_premier_credit", 
                  "mt_dernier_credit", "anc_dernier_credit", "endettement", 
                  "mt_moyen_credit_actuel", "duree_moyenne_credit", 
                  "dernier_credit_ratio", "anciennete_par_credit")

# Sous-ensemble des données
Banque3_sel <- Banque3[, vars_interet]

# Séparation train/test
index <- createDataPartition(Banque3_sel$flag_credit, p = 0.7, list = FALSE)
train <- Banque3_sel[index, ]
test <- Banque3_sel[-index, ]

# Formule du modèle
formule <- as.formula(paste("flag_credit ~", paste(vars_interet[-1], collapse = " + ")))

# Imputation des valeurs manquantes
vars_na <- c("endettement", "duree_moyenne_credit", "dernier_credit_ratio")
for (var in vars_na) {
  train[[var]][is.na(train[[var]])] <- median(train[[var]], na.rm = TRUE)
}

# Rééquilibrage
# Sous-échantillonnage
train_under <- ovun.sample(formule, data = train, method = "under", seed = 42)$data

# SMOTE
train$flag_credit <- as.factor(train$flag_credit)
train$flag_credit_num <- as.numeric(as.character(train$flag_credit))
smote_result <- SMOTE(X = train[, -1], target = train$flag_credit_num, K = 5, dup_size = 2)
train_smote <- smote_result$data
train_smote$flag_credit <- factor(ifelse(train_smote$class == 1, "0", "1"))
train_smote$class <- NULL

# Modélisation
model_under <- glm(formule, family = "binomial", data = train_under)
model_smote <- glm(formule, family = "binomial", data = train_smote)

# Évaluation
evaluate_model <- function(model, test_data, label = "") {
  probs <- predict(model, newdata = test_data, type = "response")
  preds <- factor(ifelse(probs > 0.5, "1", "0"), levels = c("0", "1"))
  actuals <- factor(test_data$flag_credit, levels = c("0", "1"))
  
  cm <- confusionMatrix(preds, actuals, positive = "1")
  auc_val <- auc(actuals, probs)
  
  cat("===", label, "===\n")
  cat("Précision :", round(cm$byClass["Precision"], 4), "\n")
  cat("Rappel    :", round(cm$byClass["Recall"], 4), "\n")
  cat("AUC       :", round(auc_val, 4), "\n\n")
}

# Résultats
evaluate_model(model_under, test, "Sous-échantillonnage")
evaluate_model(model_smote, test, "SMOTE")



 
 