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
devtools::install_github("garthtarr/edgebundleR")
library(edgebundleR)

# --------------------------------
# Load in data
data <- read.csv("data/Forbes_supp.csv")

# Filter to chapters 2,3,4 (ie. chapters for SCZ, BD, MDD)
graph_data <- data %>%
  dplyr::filter(str_detect(DSM.5.Chapter, "Ch2\ ")) %>%
  # dplyr::filter(str_detect(DSM.5.Chapter, "Ch2\ |Ch3\ |Ch4\ ")) %>%
  dplyr::select(DSM_Chapter = DSM.5.Chapter,
                "Symptom_ID" = Unique.symptom.number..index.for.duplicate.symptoms..sort.by.this.column.to.see.the.groups.of.symptoms.that.have.been.coded.as.redundant.) %>%
  dplyr::mutate(Symptom_ID = as.character(Symptom_ID)) %>%
  group_by(Symptom_ID) %>%
  add_count(name = "symptom_id_count") %>%
  ungroup() %>%
  dplyr::mutate(Unique_ID = 1:nrow(.)) 

head(graph_data)

# ---------------
# Create hierarchical network dataframe:

# Origin on top, then DSM-5 chapters, then symptoms
d1 <- data.frame(from="origin", to=unique(graph_data$DSM_Chapter))
d2 <- graph_data %>%
  dplyr::select(from = DSM_Chapter, to = Unique_ID)
hierarchy <- as.data.frame(rbind(d1, d2))
head(hierarchy)

# create a vertices data.frame. One line per object of our hierarchy, giving features of nodes.
vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))) ) 

# Let's add a column with the group of each name. It will be useful later to color points
vertices$group  <-  hierarchy$from[ match( vertices$name, hierarchy$to ) ]
vertices$value <- as.character(c(rep(1,4), graph_data$symptom_id_count))
vertices$value <- as.character(c(rep(1,2), graph_data$symptom_id_count))

# ---------------
# Create a graph object with the igraph library
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
# This is a network object, you visualize it as a network like shown in the network section!

ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() +
  theme_void()

# ---------------
# Create connections for overlapping symptoms
# Where Unique_ID equals i, then a new "to" column should be the variable "to":
to <- sapply(1:nrow(graph_data), function(i){
  row <- graph_data[graph_data$Unique_ID == i,]
  symID <- row$Symptom_ID
  to <- graph_data[graph_data$Symptom_ID == symID & graph_data$Unique_ID > i,][1,]$Unique_ID
  to
})

graph_data$to <- to

# ---------------
# Create connections dataframe:
connect <- graph_data %>%
  dplyr::select(from = Unique_ID, to) %>%
  mutate(from = as.character(from),
         to = as.character(to))  %>%
  drop_na() 
head(connect)


# ---------------
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = connect$from, to = connect$to), alpha=0.7, colour="grey", tension = 1) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=group, size=value, alpha=0.2)) +
  # scale_color_manual(values = c("Ch2 - Schizophrenia Spectrum and Other Psychotic Disorders"= "#e2c9f2",
  #                               "Ch3 - Bipolar and Related Disorders" = "#ceedaf", 
  #                               "Ch4 - Depressive Disorders" = "#9dc8e3"))+
  theme_void()+
  theme(legend.position = "none")+
  geom_node_text(aes(x = x*1.1, y=y*1.1, filter = leaf, label=name), size=5, alpha=1) 
  

# ---------------
# why is there an edge from 3 that stops in the middle of the plot?
vertices[vertices$name == "4",]

graph_data %>%
  filter(Unique_ID == "3")

graph_data %>%
  filter(symptom_id_count == 1)

# --------------------------------
edgebundle(mygraph)




