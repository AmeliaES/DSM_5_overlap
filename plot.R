# Use data from suppliementary of Forbes et al. 2023 to create a similar figure restricted to SCZ, MDD and BD
# Paper: DOI: https://doi.org/10.1017/S0033291723002544
# Data: https://osf.io/w3kcn
# --------------------------
# Load libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(igraph)
library(ggraph)
library(tidyr)
library(tidygraph)
set.seed(1)
# --------------------------------
# Load in data
data <- read.csv("data/Forbes_supp.csv")

# Filter to chapters 2,3,4 (ie. chapters for SCZ, BD, MDD)
graph_data <- data %>%
  dplyr::filter(str_detect(DSM.5.Chapter, "Ch2\ |Ch3\ |Ch4\ ")) %>%
   dplyr::select(DSM_Chapter = DSM.5.Chapter,
                Disorder = DSM.5.Disorder,
                "Symptom_ID" = Unique.symptom.number..index.for.duplicate.symptoms..sort.by.this.column.to.see.the.groups.of.symptoms.that.have.been.coded.as.redundant.,
                "Description" = Constituent.symptom..wording.from.DSM.5.) %>%
  add_count(Symptom_ID)


# ------------------------------------------
# Create an empty adjacency matrix
adj_matrix <- matrix(0, nrow = nrow(graph_data), ncol = nrow(graph_data))

# Find IDs for symptoms that overlap
IDs3 <- graph_data %>%
  group_by(Symptom_ID) %>%
  summarise(n_groups = n_distinct(DSM_Chapter)) %>%
  filter(n_groups == 3) %>%
  pull(Symptom_ID) %>%
  unique()

IDs2 <- graph_data %>%
  group_by(Symptom_ID) %>%
  summarise(n_groups = n_distinct(DSM_Chapter)) %>%
  filter(n_groups == 2) %>%
  pull(Symptom_ID) %>%
  unique()

for (i in 1:nrow(graph_data)){
  if(graph_data$Symptom_ID[i] %in% IDs3){
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 20, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 20, 0)
  }else if(graph_data$Symptom_ID[i] %in% IDs2){
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 10, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 10, 0)
  }else{
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 1, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 1, 0)
  }
}

for (i in 1:nrow(graph_data)){
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, graph_data$n[i], 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, graph_data$n[i], 0)
}


# Check it keeps nodes with 0 edges
sapply(1:nrow(adj_matrix), function(i){
  sum(adj_matrix[i,]) == 0
})

# Convert to a tidy graph object
graph <- as_tbl_graph(adj_matrix, directed = FALSE) %>%
  mutate(DSM5_Chapter = factor(graph_data$DSM_Chapter)) %>%
  mutate(Disorder = factor(graph_data$Disorder)) %>%
  mutate(Symptom_ID = graph_data$Symptom_ID) %>%
  mutate(overlap = ifelse(Symptom_ID %in% IDs, TRUE, FALSE)) 

unique(graph_data$DSM_Chapter)
max(graph_data$n)

png("plots/chapters.png", height = 3000, width = 3000, res = 500)
ggraph(graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(colour = weight), alpha = 0.3, show.legend = F) + # 
  geom_node_point(size = 2, aes(color = DSM5_Chapter)) +
  theme_graph() +
  scale_edge_color_gradientn(colours = c('#756f72',  '#dbb75a', '#ab356a')) +
  scale_color_manual(values = c("Ch2 - Schizophrenia Spectrum and Other Psychotic Disorders" = "#c6a9cc",
                                "Ch3 - Bipolar and Related Disorders" = "#cee8a9",
                                "Ch4 - Depressive Disorders" = "#8ebfe8")) +
  guides(color="none")
dev.off()


ggraph(graph, layout = 'hive', axis = DSM5_Chapter) + 
  geom_edge_arc(aes(colour = weight), alpha = 0.3, show.legend = F) + # 
  geom_node_point(size = 2, aes(color = DSM5_Chapter)) +
  theme_graph() +
  scale_edge_color_gradientn(colours = c('#756f72',  '#dbb75a', '#ab356a')) +
  scale_color_manual(values = c("Ch2 - Schizophrenia Spectrum and Other Psychotic Disorders" = "#c6a9cc",
                                "Ch3 - Bipolar and Related Disorders" = "#cee8a9",
                                "Ch4 - Depressive Disorders" = "#8ebfe8")) +
  guides(color="none")

# --------------------------------
# for disorders, not chapters
graph_data <- graph_data %>%
dplyr::filter(Disorder %in% c("Schizophrenia","Bipolar I Disorder", "Bipolar II Disorder" , "Major Depressive Disorder")) 

adj_matrix <- matrix(0, nrow = nrow(graph_data), ncol = nrow(graph_data))

# Find IDs for symptoms that overlap
IDs3 <- graph_data %>%
  group_by(Symptom_ID) %>%
  summarise(n_groups = n_distinct(DSM_Chapter)) %>%
  filter(n_groups == 3) %>%
  pull(Symptom_ID) %>%
  unique()

IDs2 <- graph_data %>%
  group_by(Symptom_ID) %>%
  summarise(n_groups = n_distinct(DSM_Chapter)) %>%
  filter(n_groups == 2) %>%
  pull(Symptom_ID) %>%
  unique()

for (i in 1:nrow(graph_data)){
  if(graph_data$Symptom_ID[i] %in% IDs3){
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 20, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 20, 0)
  }else if(graph_data$Symptom_ID[i] %in% IDs2){
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 10, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 10, 0)
  }else{
    adj_matrix[i,] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 1, 0)
    adj_matrix[,i] <- ifelse(graph_data$Symptom_ID[i] == graph_data$Symptom_ID, 1, 0)
  }
}

# Check it keeps nodes with 0 edges
sapply(1:nrow(adj_matrix), function(i){
  sum(adj_matrix[i,]) == 0
})

# Convert to a tidy graph object
graph <- as_tbl_graph(adj_matrix, directed = FALSE) %>%
  mutate(DSM5_Chapter = factor(graph_data$DSM_Chapter)) %>%
  mutate(Disorder = factor(graph_data$Disorder)) %>%
  mutate(Symptom_ID = graph_data$Symptom_ID) %>%
  mutate(overlap = ifelse(Symptom_ID %in% IDs, TRUE, FALSE))

png("plots/disorders.png", height = 3000, width = 3000, res = 500)
ggraph(graph, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(aes(colour = weight), alpha = 0.6, show.legend = F) + 
  geom_node_point(size = 2, aes(color = Disorder)) +
  theme_graph() +
  scale_edge_color_gradientn(colours = c('#756f72',  '#dbb75a', '#ab356a')) +
  scale_color_manual(values = c("Schizophrenia" = "#c6a9cc",
                                "Bipolar I Disorder" = "#cee8a9",
                                "Bipolar II Disorder" = "#698a63",
                                "Major Depressive Disorder" = "#8ebfe8")) +
  guides(color="none")
dev.off()



