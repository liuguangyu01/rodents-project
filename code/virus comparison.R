# 设置工作目录
setwd('D:/zhuomian/muse/R/aβ多样性/virus comparison')
rm(list = ls())  # 去除环境变量

# 加载必要的库
library(vegan)
library(picante)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library('gridExtra')
library('cowplot')

# 读取数据
group_info <- read.table('group.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df <- read.csv('heatmap_data.csv', header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

# 确保数据列名正确
print(names(df))

# 合并数据和分组信息
df1 <- merge(df, group_info, by.x = "lib_id", by.y = "lib_id")

# 检查是否有NA
print(sum(is.na(df1)))

# 设定0值为NA
df1[df1 == 0] <- NA

# 转换数值列为log10(RPM + 1)
numeric_columns <- sapply(df1, is.numeric)
df1[numeric_columns] <- log10(df1[numeric_columns] + 1)

# 设置颜色
mycol <- c("#F9764B", "#15514A")

# 确保数据框中包含这些列
virus_names <- c("Betacoronavirus_1", "Mamastrovirus_3", "Influenza_A_virus_H9N2", 
                 "Rotavirus_A", "Porcine_torovirus", "Porcine_sapovirus", 
                 "Porcine_circovirus", "Porcine_bocavirus", "Porcine_astrovirus")

# 检查列是否存在
missing_cols <- virus_names[!virus_names %in% names(df1)]
if(length(missing_cols) > 0) {
  print(paste("警告：以下列不存在:", paste(missing_cols, collapse=", ")))
}

# 转换数据格式为long格式，方便绘图
df_long <- tidyr::pivot_longer(
  df1,
  cols = virus_names[virus_names %in% names(df1)],
  names_to = "virus",
  values_to = "value"
)

# 处理NA值
df_long <- df_long[!is.na(df_long$value), ]

# 设置病毒显示顺序
df_long$virus <- factor(df_long$virus, levels = virus_names)

# 创建多个箱线图
p <- ggplot(df_long, aes(x = group, y = value, color = group)) +
  geom_boxplot(outlier.shape = NA, fill = "transparent") +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  facet_wrap(~ virus, ncol = 5, scales = "free_y") +
  scale_color_manual(values = mycol) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", size = 0.8),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black", hjust = 1),
    axis.title.y = element_text(size = 10, face = "italic"),
    strip.text = element_text(size = 10, face = "italic"),
    legend.position = "none"
  ) +
  labs(y = "log10(RPM + 1)")

# 保存图形
ggsave("Alpha.pdf", p, width = 10, height = 6)

# 同时打印到屏幕
print(p)

cat("图表已成功生成并保存为Alpha.pdf\n")

