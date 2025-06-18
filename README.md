# Code and Data for the Manuscript: Rodent–Pathogen Profiling in Swine Farms and Urban Environments: Metatranscriptomic Zoonotic Risk Insights

This repository contains the data and R scripts used for the analyses and figure generation in the manuscript titled "Rodent–Pathogen Profiling in Swine Farms and Urban Environments: Metatranscriptomic Zoonotic Risk Insights".

## Repository Structure

- `/code`: This directory contains all the R scripts used for analysis and visualization.
- `/data`: This directory contains all the necessary input data files required to run the scripts in the `/code` directory.

## Dependencies

The analyses were performed in R (version 4.2 or later). The following key R packages are required to run the scripts:

- `tidyverse`
- `ComplexHeatmap`
- `circlize`
- `ggplot2`
- `sf`
- `igraph`
- `networkD3`
- `vegan`
- `pctax`

Please ensure these packages are installed before running the scripts.

## How to Reproduce

Each major analysis or figure can be reproduced by running the corresponding script in the `/code` directory. The scripts are designed to read the necessary input files from the `/data` directory.

- **Heatmap Figure**: To generate the main heatmap, run `/code/heatmap/draw_virus_heatmap.R`.
- **Sampling Map Figure**: To generate the geographical sampling map, run `/code/map/Map.R`.
- **Prevalence Plots**: To analyze and plot viral prevalence, run `/code/prevalence/prevalence.R`.
- **Co-occurrence Network**: To generate the viral co-occurrence network, run `/code/net/virus_co_network.R`.
- **Alpha and Beta Diversity**: For diversity analyses, run the scripts located in `/code/αβ/`.
- **Sankey Diagram**: To generate the Sankey diagram, run `/code/sankey/sankey_networkD3_v2.R`.
- **Venn Diagram**: To generate the Venn diagram of shared viruses, run `/code/venn/virus_network_venn.R`.

## Contact

For any questions regarding the code or data, please contact the author at 15503796797@stu.scau.edu.cn
