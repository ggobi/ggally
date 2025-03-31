###Original text for the issue ####

# Chargement des bibliothèques nécessaires
library(effects)
library(ggplot2)
library(GGally)
# Chargement des données d'arrestations
data(Arrests)
####Plot####
plot <- ggpairs(Arrests)
plot

####Première proposition de chatGPT####

# Création d'un graphique de paires en sélectionnant uniquement la colonne 'released' contre toutes les autres variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
# Affichage du graphique

plot

#Ce code a enlevé les colonnes checks et citizen alors que cela n'était pas demandé, et n'a pas fait ce qui était attendu mais à rajouté le canal, couleurs qui aident à la compréhension globale des graphiques
#J'ai donc redemandé à ChatGPT, voici la deuxième proposition : 

# Création d'un graphique de paires en sélectionnant uniquement la colonne 'released' contre toutes les autres variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released)) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10))

plot

#On rencontre toujours le même problème

#Je teste du code à l'aide de forums et de recherches 

# Test de différentes configurations :

# Création d'un graphique de paires en sélectionnant uniquement la colonne 'released' contre toutes les autres variables
plot <- ggpairs(Arrests, columns = c(1, 2:5),
                mapping = ggplot2::aes(color = released),
                upper = list(continuous = wrap("points")),
                lower = list(continuous = wrap("points"))) +
  theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10))

# Ajout de thèmes pour s'assurer que les étiquettes de l'axe x apparaissent sur chaque sous-graphique
plot <- plot + theme(strip.background = ggplot2::element_blank(),
                     strip.text.x = ggplot2::element_text(size = 10),
                     axis.text.x = ggplot2::element_text(size = 9))
# Affichage du graphique

plot

#Je rencontre toujours le même problème avec les axes 
# Fonction pour ajouter des étiquettes d'axe x et des lignes de grille à chaque sous-graphique
add_axes_and_grids <- function(plot) {
  plot + theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.ticks.x = ggplot2::element_line(),
    panel.grid.major = ggplot2::element_line(color = "grey80"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 10),
    axis.title.x = ggplot2::element_text(size = 12)
  )
}

# Création d'un graphique de paires en sélectionnant uniquement la colonne 'released' contre toutes les autres variables
base_plot <- ggpairs(Arrests, columns = c(1, 2:5),
                     mapping = ggplot2::aes(color = released),
                     upper = list(continuous = wrap("points")),
                     lower = list(continuous = wrap("points")),
                     diag = list(continuous = wrap("barDiag")))

# Ajout de thèmes pour s'assurer que les étiquettes de l'axe x et les lignes de grille sont visibles sur chaque sous-graphique
plot <- base_plot +
  theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid.major = ggplot2::element_line(color = "grey80"),
    panel.grid.minor = ggplot2::element_line(color = "grey90"),
    strip.background = ggplot2::element_blank(),
    strip.text.x = ggplot2::element_text(size = 10),
    axis.title.x = ggplot2::element_text(size = 12),
    axis.ticks.x = ggplot2::element_line(),
    axis.text.x.bottom = ggplot2::element_text(size = 10)
  )

# Affichage du graphique

plot


# Encore des tests

library(GGally)
library(ggplot2)
library(effects)

data(Arrests)

# Création d'un graphique de paires en sélectionnant uniquement la colonne 'released' contre toutes les autres variables
base_plot <- ggpairs(Arrests,
                     columns = c(1, 2, 3, 4, 5),  # Garder uniquement 'released' vs autres
                     mapping = aes(color = released),
                     upper = list(continuous = wrap("points")),
                     lower = list(continuous = wrap("points")),
                     diag = list(continuous = wrap("barDiag")),
                     columnLabels = c("Released", "Colour", "Year", "Age", "Sex"))

# Fonction pour ajouter des étiquettes d'axe x et appliquer une grille
fix_x_axis_labels <- function(plot) {
  for (i in 2:length(plot$plots)) {
    plot$plots[[i, 1]] <- plot$plots[[i, 1]] +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10),
            axis.title.x = element_text(size = 12))
  }
  return(plot)
}

# Application de la fonction pour appliquer les étiquettes d'axe x sur tous les sous-graphiques
final_plot <- fix_x_axis_labels(base_plot)

# Affichage du graphique final

print(final_plot)


# Create a pair plot selecting only 'released' against all other variables
base_plot <- ggpairs(Arrests,
                     columns = c(1, 2, 3, 4, 5),  # Garder uniquement 'released' vs autres
                     mapping = aes(color = released),
                     upper = list(continuous = wrap("points")),
                     lower = list(continuous = wrap("points")),
                     diag = list(continuous = wrap("barDiag")))

# Application manuelle des étiquettes d'axe x sur chaque sous-graphique
final_plot <- base_plot +
  theme(
    strip.text = element_text(size = 10), # Assurer la visibilité des étiquettes de facettes
    axis.text.x = element_text(angle = 45, vjust = 1, size = 10),
    axis.title.x = element_text(size = 12)
  )
# Affichage du graphique final

print(final_plot)


