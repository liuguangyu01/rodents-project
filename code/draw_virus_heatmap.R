# Install and load the necessary packages
# install.packages("randomcoloR")  # Uncomment if not already installed
# install.packages("circlize")  # Uncomment if not already installed
library(randomcoloR)
library(circlize)
library(ComplexHeatmap)
library(tidyverse)

setwd("C:\\Users\\79011\\OneDrive\\桌面\\muse\\R\\heatmap- 副本 (2)")

# Load and preprocess data
vtable = read_csv("virus_table_filtered.csv") %>%
  select_if(function(x) { ifelse(is.character(x), T, sum(x) > 0) })  # drop empty columns

virus_name_arr = colnames(select(vtable, -lib_id))
lib_id_arr = vtable$lib_id

vmeta = read_csv("virus_meta.csv") %>%
  filter(virus_name %in% virus_name_arr)

lib_meta = read_csv("lib_metadata.csv")

vmat = t(select(vtable, all_of(virus_name_arr)))
colnames(vmat) = vtable$lib_id

# Reordering
vmeta_ord = vmeta %>% filter(virus_name %in% virus_name_arr) %>% arrange(virus_family)  # 确保vmeta_ord和vmat列匹配
meta_ord = arrange(lib_meta, order)
vmat_ord = log10(vmat[vmeta_ord$virus_name, meta_ord$lib_id] + 1)

# Define unique categories for colors
genus = sort(c("Mus", "Rattus", "Bandicota", "Apodemus"))  # Example genus list, adjust as necessary
site = c("Pig farm", "Urban")
family = unique(vmeta_ord$virus_family)
species = sort(unique(meta_ord$species))
domain = unique(vmeta_ord$domain)  # Extract unique domain values

# Generate colors for each category
total_elements = length(genus) + length(site) + length(family) + length(species) + length(domain)
all_colors = distinctColorPalette(total_elements)

# Assign colors to each category
genus_color = all_colors[1:length(genus)]
site_color = all_colors[(length(genus) + 1):(length(genus) + length(site))]
family_color = all_colors[(length(genus) + length(site) + 1):(length(genus) + length(site) + length(family))]
species_color = all_colors[(length(genus) + length(site) + length(family) + 1):(length(genus) + length(site) + length(family) + length(species))]
domain_color = all_colors[(length(genus) + length(site) + length(family) + length(species) + 1):total_elements]

# RPM color function
RPM_col_fun = colorRamp2(c(0, ceiling(max(vmat_ord))), c("#F4F2ED", "#DD1332"))

# Heatmap annotations
col_ano = columnAnnotation(
  sample_site = anno_simple(meta_ord$site, col = setNames(site_color, site), height = unit(0.3, "cm")),
  host_genus = anno_simple(meta_ord$genus, col = setNames(genus_color, genus), height = unit(0.3, "cm")),
  host_species = anno_simple(meta_ord$species, col = setNames(species_color, species), height = unit(0.3, "cm")),
  gap = unit(1, "mm")
)

row_ano = rowAnnotation(
  domain = anno_simple(vmeta_ord$domain, col = setNames(domain_color, domain), width = unit(0.3, "cm")),
  virus_family = anno_simple(vmeta_ord$virus_family, col = setNames(family_color, family), width = unit(0.3, "cm"))
)

# Draw heatmap with domain and site ordering, and grouping by species within each site
pdf(file = "virus_heatmap.pdf", width = 12, height = 8)
ht = Heatmap(vmat_ord,
             cluster_columns = F,
             cluster_rows = F,
             
             # Custom row and column ordering
             row_order = order(vmeta_ord$domain, vmeta_ord$virus_family, vmeta_ord$virus_name),
             column_order = order(meta_ord$site, meta_ord$species),
             
             # Column split by site, to keep samples from the same site together
             column_split = factor(meta_ord$site, levels = site, ordered = T),
             
             # Row split by domain to keep viruses of the same domain together
             row_split = factor(vmeta_ord$domain, levels = unique(vmeta_ord$domain), ordered = T),
             
             col = RPM_col_fun,
             top_annotation = col_ano,
             left_annotation = row_ano,
             column_labels = rep("", ncol(vmat_ord)),
             row_names_gp = gpar(fontsize = 10),
             column_names_gp = gpar(fontsize = 6)
)

# Legends
l1 = Legend(labels = genus, title = "Host genus", legend_gp = gpar(fill = genus_color))
l2 = Legend(labels = species, title = "Host species", legend_gp = gpar(fill = species_color))
l3 = Legend(labels = site, title = "Sample site", legend_gp = gpar(fill = site_color))
l4 = Legend(labels = family, title = "Virus family", legend_gp = gpar(fill = family_color))
l5 = Legend(labels = unique(vmeta_ord$domain), title = "Domain", legend_gp = gpar(fill = domain_color))  # Add domain legend

# Draw heatmap with legends
draw(ht, annotation_legend_list = list(l1, l2, l3, l4, l5))
dev.off()
