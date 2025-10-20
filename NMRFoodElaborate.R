library(readxl)
library(dplyr)
library(svglite)
library(ggplot2)
CollectionNMRFood <- read_excel("C:/Users/spigno/OneDrive - Università di Napoli Federico II/Dottorato/spettri NMR/NMRFood/CollectionNMRFood.xlsx",
                                sheet = "datidef")


#### Formatting the Table ####
# Filter keeping just the rows including the ppm from 0 to 200

CollectionNMRFood_filtered <- CollectionNMRFood %>%
  filter((ppm >= 0 & ppm < 200))




#####---------------Normalizing Data--------
NMRfood<- as.data.frame(CollectionNMRFood_filtered[,-1])
rownames(NMRfood) <- CollectionNMRFood_filtered$ppm


normalize_columns <- function(df) {
  # Calcola la somma totale per ogni colonna
  column_sums <- colSums(df, na.rm = TRUE)
  
  # Normalizza ogni elemento della colonna dividendo per la somma totale della colonna
  normalized_df <- as.data.frame(lapply(seq_along(df), function(i) {
    df[[i]] / column_sums[i]
  }))
  
  # Mantieni i nomi delle colonne e delle righe
  colnames(normalized_df) <- colnames(df)
  rownames(normalized_df) <- rownames(df)
  
  return(normalized_df)
}

normalized_df <- normalize_columns(NMRfood)

rownames(normalized_df) <- CollectionNMRFood_filtered$ppm

normalized_df$ppm <-  CollectionNMRFood_filtered$ppm

# Aggiunta della colonna MolecularClasses con 7 condizioni
normalized_df$MolecularClasses <- ifelse(
  normalized_df$ppm <= 45, "Alkyl C",
  ifelse(
    normalized_df$ppm > 45 & normalized_df$ppm <= 60, "Methoxyl N-Alkyl-C",
    ifelse(
      normalized_df$ppm > 60 & normalized_df$ppm <= 90, "O-alkyl-C",
      ifelse(
        normalized_df$ppm > 90 & normalized_df$ppm <= 110, "Di-O-alkyl C",
        ifelse(
          normalized_df$ppm > 110 & normalized_df$ppm <= 145, "H- C- sub. aromatic C", # da studiare se chiamare questo picco Aromatic o Phenolic
          ifelse(
            normalized_df$ppm > 145 & normalized_df$ppm <= 160, "O- sub. aromatic  C",
            ifelse(
              normalized_df$ppm > 160, "Carbonyl C",
              NA  # Valore di default, nel caso in cui nessuna condizione sia soddisfatta
            )
          )
        )
      )
    )
  )
)





# Invertire l'ordine delle righe
#inverted_df <- normalized_df[rev(seq_len(nrow(normalized_df))), ]
data <-  normalized_df

# #--------------------------------
# ######## Fig. 1 ##################
# #--------------------------------
# # Now Plotting of the single Spectra ## Fig. 1
# library(ggplot2)
# 
# # Loop per creare un grafico per ogni colonna (tranne la prima)
# plots <-  list()
# # Calcola i limiti comuni per l'asse y
# y_min <- min(data[,-c(40,41)])  # Esclude la prima colonna
# y_max <- max(data[,-c(40,41)])
# 
# for (col in names(data)[-c(40,41)]) {
#   plot <- ggplot(data[,-c(40,41)], aes_string(x = data$ppm, y = col)) +  # Usa aes_string() per accettare nomi dinamici delle colonne
#     geom_line(linewidth = 0.1) +
#     annotate("text", 
#              x = mean(range(data$ppm, na.rm = TRUE)),  # Posiziona il titolo al centro sull'asse x
#              y = y_max,  # Posiziona il titolo poco sotto il massimo dell'asse y
#              label = col, 
#              size = 1.28) +  # Personalizza lo stile del titolo
#     xlab("") +
#     ylab("") +
#     scale_x_reverse() +
#     theme_classic() +
#     theme(axis.title.y = element_blank(),  # Rimuove il titolo dell'asse y
#           axis.text.y = element_blank(),  # Rimuove i valori dell'asse y
#           axis.text= element_blank(),
#           axis.line.x = element_blank(),
#           axis.line.y = element_blank(),
#           axis.ticks = element_blank()
#     )
#   
#   #print(plot)
#   
#   # Aggiungi il plot alla lista
#   plots[[col]] <- plot
#   
#   # Salva il plot con il nome della colonna
#   #ggsave(filename = paste0(col, "spettro.svg"), plot = plot, width = 6, height = 4)
#   #ggsave(filename = paste0(col, "spettro.png"), plot = plot, width = 6, height = 4)
# }
# library(gridExtra)
# # Salvataggio in PDF
# pdf("grid_plot.pdf")
# do.call(grid.arrange, c(plots, ncol = 3))
# dev.off()
# # Salvataggio in PNG
# png("grid_plot.png", width = 16,height=22, units = "cm", res=400)
# do.call(grid.arrange, c(plots, ncol = 3))
# dev.off()
# 
# # Salvataggio in SVG
# svg("grid_plot.svg", width = 16)
# do.call(grid.arrange, c(plots, ncol = 3))
# dev.off()

#### Analysis Grouping for the Molecular Classes ####
# Summing all the rows on the bases of the column molecular classes
library(dplyr)
# Check if there are NA 'MolecularClasses'
any_na <- any(is.na(data$MolecularClasses))
rows_with_na <- which(is.na(data$MolecularClasses))
rowsNA <- data[c(rows_with_na),] 
##### Sum over the Classes
summed_data <- data %>%
  mutate(MolecularClasses = factor(MolecularClasses, levels = unique(MolecularClasses))) %>%  # Mantieni l'ordine originale
  group_by(MolecularClasses) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%  # Somma tutte le colonne ignorando i valori NA
  arrange(MolecularClasses) %>%
  as.data.frame()
summed_data <- summed_data[,-41] #remove the last column
print(class(summed_data))

#----------------------------------------------------------------
############################################# FIG. 2 #############
#----------------------------------------------------------------
library(tidyr)
# Creazione del data frame con classi molecolari e colori associati
  Colore = c(
    "red3",   # Rosso
    "#808080",   # Grigio
    "#000000",   # Nero
    "#FFA500",   # Arancione
    "#FFFF00",   # Giallo
    "lightgreen",   # Giallo chiaro
    "brown"    # Marrone
  )

  colorsMacro <- c("black",
                   "grey",
                   "white")
# Lista per salvare i plot
hists <- list()

# Ottieni i nomi delle colonne da plottare, escludendo 'MolecularClasses'
columns_to_plot <- names(summed_data)[names(summed_data) != "MolecularClasses"]
### Ordina le colonne in base a O-alkyl C
df <- summed_data[,-1]

df_ordinato <- df[, order(-unlist(df[6, ]))]

summed_data1 <- cbind(summed_data[,1], df_ordinato)
library(writexl)

write_xlsx(summed_data1, ("dataNMRregionsFood.xlsx"))
colnames(summed_data1)[1] <- "MolecularClasses"
# # Ciclo per creare un bar plot per ogni colonna
# for (col in columns_to_plot) {
#   plot <- ggplot(summed_data1, aes(x = "", y = .data[[col]], fill = MolecularClasses)) +
#     xlab(col)+
#     geom_bar(stat = "identity", color="black",position="fill",width=0.3) +
#     scale_fill_manual(values = Colore) +  # Usa la mappa di colori definita
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1),
#          axis.title.y = element_blank(),  # Rimuove il titolo dell'asse y
#                 axis.text.y = element_blank(),  # Rimuove i valori dell'asse y
#                 
#                 axis.line.x = element_blank(),
#                 axis.line.y = element_blank(),
#                 axis.ticks = element_blank(),
#                 legend.box = element_blank()
#           )+
#     scale_y_continuous(labels = scales::percent)
#   # Aggiungi il plot alla lista
#   hists[[col]] <- plot
#   # # Salva il plot con il nome della colonna
#   ggsave(filename = paste0(col, "histo.svg"), plot = plot, width = 6, height = 4)
#    ggsave(filename = paste0(col, "histo.png"), plot = plot, width = 6, height = 4)
# }
library(gridExtra)
# # Stampa i plot (opzionale)
# for (p in hists) {
#   print(p)
# }
# do.call(grid.arrange, c(hists, ncol = 3))

# Salvataggio in PDF
pdf("histogrid.pdf")
#do.call(grid.arrange, c(hists, ncol = 3))
dev.off()
# Salvataggio in PNG
#png("histogrid.png", width = 16,height=20, units = "cm", res=118.11)
#do.call(grid.arrange, c(hists, ncol = 3))
#dev.off()

# Salvataggio in SVG
#svg("histogrid.svg", width = 16)
#do.call(grid.arrange, c(hists, ncol = 3))
#dev.off()

#------------------------------------------------
###############Multivariate Analysis#############
#------------------------------------------------
# PCA over of the food categories over the relative abundance of the molecular classes ## Fig. 3 
# Trasposing the dataset "Summed_data" to have the molecular classes on the columns

datat <- t(summed_data[,-1]) #  rimuovo la prima colonna che rappresenta i nomi delle classi

# Conversione in data frame rimuovendo le righe indesiderate 
datat1 <- as.data.frame(datat[, ])
# Converte tutte le celle in valori numerici
#datat1 <- as.data.frame(lapply(datat, function(x) as.numeric(as.character(x))))

# Imposta i nomi delle colonne utilizzando i valori della colonna 'MolecularClasses' di summed_data
colnames(datat1) <- summed_data$MolecularClasses
#rownames(datat1) <- colnames(summed_data[-c(1, 43)])

# Controllo dei tipi per assicurarti che ogni cella sia numerica
class(datat1)

df <- datat1[,]

library(factoextra)
# Dissimilarity Matrix ##########
# Normalizza le righe del dataframe alla loro norma (se vuoi calcolare distanza tra righe)
normalizza <- function(x) x / sqrt(sum(x^2))
df_norm <- t(apply(df, 1, normalizza))  # Righe normalizzate

# Calcola la distanza euclidea tra righe normalizzate (cioè: distanza della corda)
d_corda <- dist(df_norm, method = "euclidean")



d <- dist(df, method = "euclidean")
m <- as.matrix(d_corda)
### Cluster Method #########
## Gerarchical
hc5 <- hclust(d_corda, method = "ward.D2" )
plot(hc5)
hc5$order
dend <-  as.dendrogram(hc5)
#cut the tree in groups
sub_grp <- cutree(hc5, k = 6)
length(sub_grp)
table(sub_grp, hc5$labels )

# Calcola l'ordine dei gruppi
order_sub_grp <- order(sub_grp)
hc5$order <- order_sub_grp
hc5
dendro <- plot(hc5,cex = 0.5)

prova <- as.data.frame(cbind(order_sub_grp,df))
prova2 <- as.data.frame(cbind(sub_grp,df))

plot(hc5, labels = NULL, hang = 0.1, check = TRUE,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height")


# # Salvataggio in PDF
# pdf("dendro.pdf", width = 8, height = 6)
# plot(hc5, labels = NULL, hang = 0.1, main = "Cluster Dendrogram", ylab = "Height")
# dev.off()
# 
# # Salvataggio in PNG
# png("dendro.png", width = 16, height = 20, units = "cm", res = 118.11)
# plot(hc5, labels = NULL, hang = 0.1, main = "Cluster Dendrogram", ylab = "Height")
# dev.off()
# 
# Salvataggio in SVG
svg("dendro.svg", width = 8, height = 6)
plot(hc5, labels = NULL, hang = 0.1, main = "Cluster Dendrogram", ylab = "Height")
dev.off()
### Non Gerarchico
###How much cluster in kmean
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.

fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


### K-MEans
# Compute k-means

km.res <- kmeans(df, 6, nstart = 25)
# Print the results
print(km.res)
aggregate(df, by=list(cluster=km.res$cluster), mean)
dd <- cbind(df, cluster = km.res$cluster)
head(dd)

clustercompare <- as.data.frame(cbind(rownames(dd),  prova2$sub_grp, dd$cluster))
colnames(clustercompare) <- c("food", "Ward", "kmean")

##### PCA ##########
df_centred <- df
# Eseguire la PCA
pca_result <- prcomp(df, center = TRUE, scale. = TRUE)

# Risultati della PCA
summary(pca_result)

# Grafico delle varianze spiegate
plot(pca_result, type = "l")

# Biplot per visualizzare variabili e osservazioni
biplot(pca_result, main = "Biplot PCA")



#### PCA
library("FactoMineR")
res.pca <- PCA(df,  graph = TRUE)
# Extract eigenvalues/variances
get_eig(res.pca)

# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
# Extract the results for variables
var <- get_pca_var(res.pca)
var
# Coordinates of variables
var$coord

# Contribution of variables
var$contrib
# Graph of variables: default plot
fviz_pca_var(res.pca, col.var = "black")
# Control variable colors using their contributions
library(factoextra)

# Plot delle dimensioni 2 e 4 della PCA
fviz_pca_var(res.pca, 
             axes = c(1, 2),  # Seleziona le dimensioni 2 e 4
             col.var = "contrib",  # Colora le variabili in base al contributo
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  # Palette di colori
             repel = TRUE  # Evita sovrapposizioni di testo
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)
