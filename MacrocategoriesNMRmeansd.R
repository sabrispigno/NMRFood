library(readxl)
library(dplyr)
library(svglite)
library(ggplot2)
CollectionNMRFood <- read_excel("C:/Users/spigno/OneDrive - Università di Napoli Federico II/Dottorato/spettri NMR/NMRFood/CollectionNMRFood.xlsx",
                                sheet = "datidef")[,-40]


#### Formatting the Table ####
# Filter keeping just the rows including the ppm from 0 to 200

CollectionNMRFood_filtered <- CollectionNMRFood %>%
  filter((ppm > 0 & ppm < 200))




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
# loop across all numeric columns
normalized_df[] <- lapply(normalized_df, function(col) {
  if (is.numeric(col)) {
    col[col < 0] <- 0
  }
  col
})
normalized_df <- normalized_df %>%
  mutate(across(where(is.numeric), ~ ifelse(. < 0, 0, .)))
# Make sure it stays a data.frame
normalized_df <- as.data.frame(normalized_df)


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
          normalized_df$ppm > 110 & normalized_df$ppm <= 140, "H- C- sub. aromatic C", # da studiare se chiamare questo picco Aromatic o Phenolic
          ifelse(
            normalized_df$ppm > 140 & normalized_df$ppm <= 160, "O- sub. aromatic  C",
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



library(tidyr)
long <- normalized_df %>% 
  pivot_longer(
    cols = `Apple`:`Biscuit`, 
    names_to = "foods",
    values_to = "value"
  )

library(dplyr)
library(dplyr)
library(stringr)

Macrocategories <- long %>%
  mutate(
    MacroCategories = case_when(
      foods %in% c("Bread", "WholeWheatBread", "Pasta", "WholeWheatPasta", "CousCous", 
                   "Quinoa", "Rice", "Seitan") ~ "Cereals",
      foods %in% c("Apple", "Banana", "Orange", "Tomato", "Broccoli", "Potato", "Carrot", 
                   "Almond", "Walnut", "MushroomPleurotus") ~ "Fruits, vegetables and mushrooms",
      foods %in% c("Lentil", "Chickpea", "Tofu", "Tempeh") ~ "Legumes",
      #foods %in% c("Seitan", "Tofu", "Tempeh") ~ "Fermented cereals and legumes",
      #foods %in% c("Tofu", "Tempeh") ~ "Fermented legumes",
      foods %in% c("Chicken", "Pig", "Bresaola", "Salami", "Ham","Cod", "Mussel", "Shrimp", "Salmon") ~ "Meats",
      foods %in% c("CowCheese", "SheepCheese", "GoatCheese", "Albumen", "Yolk") ~ "Animal products",

      # foods %in% c("OilEVO") ~ "Oil",
      foods %in% c("Chocolate", "Biscuit") ~ "Processed Food",
      TRUE ~ NA_character_
    )
  )

Clusters <- long %>%
  mutate(
    sub_grp = case_when(
      foods %in% c("Apple", "Banana", "MushroomPleurotus", "Chickpea", "WholeWheatBread", "CousCous", "Orange", "Carrot") ~ 1,
      foods %in% c( "Potato", "Bread", "Pasta", "WholeWheatPasta", "Rice", "Biscuit") ~ 2,
      foods %in% c("Broccoli", "Tomato", "Lentil", "Quinoa") ~ 3,
      foods %in% c("Almond", "Walnut", "Mussel", "Tempeh", "Chocolate") ~ 4,
      foods %in% c("Tofu", "Seitan", "Albumen") ~ 5,
      foods %in% c("CowCheese", "SheepCheese", "GoatCheese", "Yolk", "Cod", "Shrimp", 
                   "Salmon", "Chicken", "Pig", "Bresaola", "Salami", "Ham") ~ 6,
      TRUE ~ NA_real_
    )
  )

girafa <-  cbind(Macrocategories,Clusters[,"sub_grp"])
library(writexl)
write_xlsx(girafa, "girafa.xlsx")
summary_Macro <- Macrocategories %>% 
  group_by(MacroCategories, ppm) %>% 
  summarise( mean_intensity = mean(value, na.rm = TRUE), 
             sd_intensity = sd(value, na.rm = TRUE), .groups = "drop" )

# 2) Plot
p <- ggplot(summary_Macro, aes(x = ppm, y = mean_intensity)) +
  geom_ribbon(aes(ymin = mean_intensity - sd_intensity,
                  ymax = mean_intensity + sd_intensity),
              fill = "grey", alpha = 0.3) +   # ombra grigia
  geom_line(colour = "black", linewidth = 0.3) +  # linea nera
  scale_x_reverse() +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    #strip.text = element_blank(),       # elimina il titolo sopra ogni facet
    strip.background = element_blank()  # elimina lo sfondo del titolo
  ) +
  facet_wrap(~ MacroCategories, ncol = 3, scales = "free_y")

p
# 3) Save (optional)
ggsave("macrocategory_mean_sd.pdf", p, width = 10, height = 8)
ggsave("macrocategory_mean_sd.png", p, width = 10, height = 8, dpi = 300)
ggsave("macrocategory_mean_sd.svg", p, width = 10, height = 8, dpi = 300)


summary_Clusters <- Clusters %>% 
  group_by(sub_grp, ppm) %>% 
  summarise( mean_intensity = mean(value, na.rm = TRUE), 
             sd_intensity = sd(value, na.rm = TRUE), .groups = "drop" )

# 2) Plot
pClusters <- ggplot(summary_Clusters, aes(x = ppm, y = mean_intensity)) +
  geom_ribbon(aes(ymin = mean_intensity - sd_intensity,
                  ymax = mean_intensity + sd_intensity),
              fill = "grey", alpha = 0.3) +   # ombra grigia
  geom_line(colour = "black", linewidth = 0.3) +  # linea nera
  scale_x_reverse() +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    #strip.text = element_blank(),       # elimina il titolo sopra ogni facet
    strip.background = element_blank()  # elimina lo sfondo del titolo
  ) +
  facet_wrap(~ sub_grp, ncol = 3, scales = "free_y")

pClusters
# 3) Save (optional)
ggsave("clusters_mean_sd.pdf", pClusters, width = 10, height = 8)
ggsave("clusters_mean_sd.png", pClusters, width = 10, height = 8, dpi = 300)
ggsave("clusters_mean_sd.svg", pClusters, width = 10, height = 8, dpi = 300)


library(writexl)
write_xlsx(summary_Macro, "summarystatesMacrocategory.xlsx")
write_xlsx(summary_Clusters, "summarystatesClusters.xlsx")
# Mean intensity across all ppm for each group
global_macro <- Macrocategories %>%
  group_by(MacroCategories,MolecularClasses) %>%
  summarise(mean_allppm = mean(value, na.rm = TRUE),
            sd_allppm   = sd(value, na.rm = TRUE))

global_cluster <- Clusters %>%
  group_by(sub_grp, MolecularClasses) %>%
  summarise(mean_allppm = mean(value, na.rm = TRUE),
            sd_allppm   = sd(value, na.rm = TRUE))



summed_data <- normalized_df %>%
  mutate(MolecularClasses = factor(MolecularClasses, levels = unique(MolecularClasses))) %>%  # Mantieni l'ordine originale
  group_by(MolecularClasses) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%  # Somma tutte le colonne ignorando i valori NA
  arrange(MolecularClasses) %>%
  as.data.frame()
summed_data <- summed_data[,-41] #remove the last column


Macrowide <-  
  Macrocategories%>%
  select(-c("ppm"))%>%
  group_by(MolecularClasses, foods, MacroCategories)%>%
  summarise(MeanFoodMolecular = sum(value, na.rm = TRUE))%>% #Sum all the ppm inside the molecular class per each food
  group_by(MolecularClasses,MacroCategories)%>% # selct just the Macrocategories
  summarise(MolecularClasses_mean = mean(MeanFoodMolecular, na.rm = TRUE))%>% #mean of the averaged molecular class in each food but per all the categories
  pivot_wider(
    names_from = MacroCategories,
    values_from = MolecularClasses_mean
  )
  
Clusterwide <-  
  Clusters%>%
  select(-c("ppm"))%>%
  group_by(MolecularClasses, foods, sub_grp)%>%
  summarise(MeanFoodMolecular = sum(value, na.rm = TRUE))%>% #Sum all the ppm inside the molecular class per each food
  group_by(MolecularClasses, sub_grp)%>% # selct just the Macrocategories
  summarise(MolecularClasses_mean = mean(MeanFoodMolecular, na.rm = TRUE))%>% #mean of the averaged molecular class in each food but per all the categories
  pivot_wider(
    names_from = sub_grp,
    values_from = MolecularClasses_mean
  )
write_xlsx(Macrowide, "Macrowide.xlsx")
write_xlsx(Clusterwide, "Clusterwide.xlsx")
