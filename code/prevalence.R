# Load required libraries
library(tidyverse)
library(readr)
library(ggplot2)

# Read CSV files
virus_meta <- read_csv("virus_meta.csv")
virus_table <- read_csv("virus_table_filtered.csv") 
lib_metadata <- read_csv("lib_metadata.csv")

# Process data
virus_table_binary <- virus_table %>%
  mutate(across(-lib_id, ~ifelse(. > 0, 1, 0)))

# 计算流行率并准备输出数据
prevalence_data <- virus_table_binary %>%
  pivot_longer(cols = -lib_id, names_to = "virus", values_to = "positive") %>%
  left_join(lib_metadata, by = "lib_id") %>%
  left_join(virus_meta, by = c("virus" = "virus_name")) %>%
  group_by(virus_family, species) %>%  # 按病毒科和宿主种分组
  summarise(
    positive_samples = sum(positive),   # 阳性样本数
    total_samples = n(),                # 总样本数
    prevalence = mean(positive) * 100,  # 流行率
    .groups = "drop"
  ) %>%
  arrange(virus_family, species) %>%    # 按病毒科和宿主种排序
  mutate(
    prevalence = round(prevalence, 2)   # 将流行率四舍五入到两位小数
  )

# 保存流行率数据到CSV文件
write_csv(prevalence_data, "virus_prevalence_by_species.csv")

# Get all unique virus families
all_families <- unique(prevalence_data$virus_family)

# Create a consistent color palette for virus families
family_colors <- setNames(scales::hue_pal()(length(all_families)), all_families)

# Create the plot
ggplot(prevalence_data, aes(x = prevalence, y = virus_family, color = virus_family)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~ species, scales = "free_x", ncol = 7) +  # 修改为按species分面
  scale_color_manual(values = family_colors) +
  scale_x_continuous(limits = c(0, NA), breaks = function(x) pretty(x, n = 3)) +
  labs(
    x = "Prevalence of virus family in different animal species (%)", 
    y = "Virus family",
    title = "Viral Prevalence Across Different Host Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_rect(color = "black", fill = "lightgray", size = 0.5),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Save the plot
ggsave("virus_prevalence_by_species.pdf", width = 20, height = 12, dpi = 300)

# 创建一个总结表,显示每个病毒科的总体流行率
summary_data <- prevalence_data %>%
  group_by(virus_family) %>%
  summarise(
    unique_species = n_distinct(species),            # 不同宿主种数
    mean_prevalence = mean(prevalence, na.rm = TRUE), # 平均流行率
    min_prevalence = min(prevalence, na.rm = TRUE),   # 最低流行率
    max_prevalence = max(prevalence, na.rm = TRUE),   # 最高流行率
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%  # 所有数值四舍五入到两位小数
  arrange(desc(mean_prevalence))  # 按平均流行率降序排列

# 保存总结数据
write_csv(summary_data, "virus_family_prevalence_summary.csv")

# Print summary to console
cat("\nVirus Family Prevalence Summary:\n")
print(summary_data)
