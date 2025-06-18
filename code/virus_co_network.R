# Load required libraries
library(tidyverse)
library(reshape2)

# Read the RPM table
rpm_data <- read.csv("RPM_table.csv", row.names = 1)

# Function to create edge table
create_edge_table <- function(rpm_matrix) {
  # Convert to presence/absence matrix (1 if RPM > 0, 0 otherwise)
  presence_matrix <- rpm_matrix > 0
  
  # Initialize empty edge list
  edges <- data.frame(
    node1 = character(),
    node2 = character(),
    Degree = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Get virus names
  viruses <- rownames(rpm_matrix)
  
  # Calculate co-infections
  for(i in 1:(length(viruses)-1)) {
    for(j in (i+1):length(viruses)) {
      virus1 <- viruses[i]
      virus2 <- viruses[j]
      
      # Find samples where both viruses are present
      coinfection_samples <- which(presence_matrix[virus1,] & presence_matrix[virus2,])
      
      if(length(coinfection_samples) > 0) {
        edges <- rbind(edges, data.frame(
          node1 = virus1,
          node2 = virus2,
          Degree = length(coinfection_samples),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Sort by degree in descending order
  edges <- edges[order(-edges$Degree),]
  
  return(edges)
}

# Function to create node table
create_node_table <- function(rpm_matrix) {
  # Convert to presence/absence matrix
  presence_matrix <- rpm_matrix > 0
  
  # Calculate metrics for each virus
  nodes <- data.frame(
    shared.name = rownames(rpm_matrix),
    name = rownames(rpm_matrix),
    stringsAsFactors = FALSE
  )
  
  # Calculate frequency (number of samples where virus is present)
  nodes$frequency <- rowSums(presence_matrix)
  
  # Calculate infection (number of samples with RPM > 0)
  nodes$infection <- rowSums(rpm_matrix > 0)
  
  # Calculate average (mean RPM when present)
  nodes$average <- sapply(1:nrow(rpm_matrix), function(i) {
    positive_samples <- rpm_matrix[i, rpm_matrix[i,] > 0]
    if(length(positive_samples) > 0) {
      mean(positive_samples)
    } else {
      0
    }
  })
  
  return(nodes)
}

# Create edge and node tables
edge_table <- create_edge_table(rpm_data)
node_table <- create_node_table(rpm_data)

# Write tables to CSV
write.csv(edge_table, "edge_table_generated.csv", row.names = FALSE, quote = FALSE)
write.csv(node_table, "node_table_generated.csv", row.names = FALSE, quote = FALSE)

# Print summary
cat("Edge table summary:\n")
print(summary(edge_table))
cat("\nNode table summary:\n")
print(summary(node_table))













# Load required libraries
library(igraph)
library(RColorBrewer)


# Function to create and plot network
create_virus_network <- function(edge_file, node_file, remove_isolated = FALSE, output_file = "virus_network_circular.pdf") {
  # Read the edge and node tables
  edges <- read.csv(edge_file)
  nodes <- read.csv(node_file)
  
  # Create virus family classifications
  virus_families <- list(
    Picornaviridae = c("Aichivirus_A", "Hunnivirus_A", "Picornaviridae_sp.", "Posavirus"),
    Dicistroviridae = c("Aparavirus_sp.", "Aphis_glycines_virus", "Apis_dicistrovirus_3", "Dicistroviridae_sp."),
    Arenaviridae = c("Arenavirus_sp.", "Cardamones_virus", "Mammarenavirus"),
    Arteriviridae = c("Arteriviridae_sp.", "new_Arteriviridae1", "new_Arteriviridae2", "new_Arteriviridae3", "new_Arteriviridae4"),
    Coronaviridae = c("Bamboo_rat_coronavirus", "Betacoronavirus_1"),
    Artoviridae = c("Hubei_rhabdo-like_virus_6"),
    Orthomyxoviridae = c("Influenza_A_virus_H9N2"),
    Astroviridae = c("Mamastrovirus_3", "Porcine_astrovirus", "Rat_astrovirus"),
    Aliusviridae = c("new_Aliusviridae1"),
    Rhabdoviridae = c("new_Rhabdovirus_1"),
    Partitiviridae = c("new_Partitiviridae", "Partitiviridae_sp."),
    Paramyxoviridae = c("Paramyxoviridae_sp.", "Rattus_tanezumi_jeilongvirus"),
    Picobirnaviridae = c("Picobirnavirus_sp."),
    Parvoviridae = c("Porcine_bocavirus", "Rodent_parvovirus", "Soybean_thrips_denso-like_virus"),
    Circoviridae = c("Porcine_circovirus"),
    Caliciviridae = c("Porcine_sapovirus", "Sapovirus_GVII"),
    Tobaniviridae = c("Porcine_torovirus"),
    Sedoreoviridae = c("Rotavirus_A"),
    Chuviridae = c("Wenzhou_rodent_chuvirus_1"),
    Phenuiviridae = c("Wenzhou_rodent_phenuivirus_1"),
    Nodaviridae = c("Boolarra_virus", "Melnik_nodavirus")
  )
  
  # Create an extended color palette by combining multiple color palettes
  set1_colors <- brewer.pal(9, "Set1")
  set2_colors <- brewer.pal(8, "Set2")
  set3_colors <- brewer.pal(12, "Set3")
  
  # Combine all colors and ensure we have enough
  family_colors <- c(set1_colors, set2_colors, set3_colors)
  family_colors <- family_colors[1:length(virus_families)]
  names(family_colors) <- names(virus_families)
  
  # Function to get virus family
  get_virus_family <- function(virus_name) {
    for(family in names(virus_families)) {
      if(virus_name %in% virus_families[[family]]) {
        return(family)
      }
    }
    return("Unknown")
  }
  
  # Add family information to nodes
  nodes$family <- sapply(nodes$name, get_virus_family)
  nodes$color <- family_colors[nodes$family]
  
  # Create graph
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  # Remove isolated vertices if requested
  if(remove_isolated) {
    isolated_vertices <- which(degree(g) == 0)
    if(length(isolated_vertices) > 0) {
      g <- delete.vertices(g, isolated_vertices)
      cat("Removed", length(isolated_vertices), "isolated nodes\n")
    }
  }
  
  # Set visual properties
  V(g)$size <- sqrt(V(g)$frequency) * 4  # Scale node sizes based on frequency
  V(g)$color <- nodes$color[match(V(g)$name, nodes$name)]  # Update colors after possible vertex removal
  E(g)$width <- E(g)$Degree/2  # Scale edge widths based on co-infection degree
  E(g)$color <- adjustcolor("gray", alpha.f = 0.5)  # Set edge color to light gray
  
  # Create circular layout
  layout <- layout_in_circle(g)
  
  # Create PDF
  pdf(output_file, width = 15, height = 15)  # Increased size for better visibility
  
  # Set margins
  par(mar = c(1,1,1,1))
  
  # Main plot
  plot(g,
       layout = layout,
       vertex.label.dist = 0,        # Place labels at center of nodes
       vertex.label.degree = 0,      # Horizontal labels
       vertex.label.color = "black",
       vertex.label.cex = 0.8,       # Smaller label size
       vertex.frame.color = "gray",
       vertex.label.family = "sans",
       edge.curved = 0.2,            # Curved edges
       margin = c(0,0,0,0))         # Remove plot margins
  
  # Add legend with multiple columns to save space
  legend("bottomright", 
         legend = names(virus_families),
         fill = family_colors,
         title = "Virus Families",
         cex = 1,
         ncol = 2,  # Display legend in 2 columns
         bty = "n")
  
  dev.off()
  
  # Print network statistics
  cat("\nNetwork Statistics:\n")
  cat("Number of nodes:", vcount(g), "\n")
  cat("Number of edges:", ecount(g), "\n")
  cat("Network density:", edge_density(g), "\n")
  cat("Average degree:", mean(degree(g)), "\n")
  
  return(g)
}

# Create edge and node tables
edge_table <- create_edge_table(rpm_data)
node_table <- create_node_table(rpm_data)

# Write tables to CSV
write.csv(edge_table, "edge_table_generated.csv", row.names = FALSE, quote = FALSE)
write.csv(node_table, "node_table_generated.csv", row.names = FALSE, quote = FALSE)

# Create networks with and without isolated nodes
g1 <- create_virus_network(
  edge_file = "edge_table_generated.csv",
  node_file = "node_table_generated.csv",
  remove_isolated = FALSE,
  output_file = "virus_network_with_isolated.pdf"
)

g2 <- create_virus_network(
  edge_file = "edge_table_generated.csv",
  node_file = "node_table_generated.csv",
  remove_isolated = TRUE,
  output_file = "virus_network_without_isolated.pdf"
)

