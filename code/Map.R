library(ggplot2)
library(dplyr)
library(sf)
library(ggforce)
library(ggnewscale)
library(cowplot)
library(gridExtra)

# 读取地理空间数据和省份数据
china_provinces <- st_read("YN.json")    #云南省导入YN.json
province_data <- read.csv("admixture.csv", stringsAsFactors = FALSE)
province_totals <- read.csv("total.csv", stringsAsFactors = FALSE)

# 确保数值型数据
province_data <- province_data %>%
  mutate(
    longitude = as.numeric(as.character(longitude)),
    latitude = as.numeric(as.character(latitude)),
    value = as.numeric(as.character(value))
  )

# 合并省份总数到地理空间数据中
china_provinces <- china_provinces %>%
  left_join(province_totals, by = c("name" = "city"))

# 按经纬度分组，计算每个采样点的总数
sampling_point_totals <- province_data %>%
  group_by(longitude, latitude) %>%
  summarise(total = sum(value), .groups = "drop")

# 计算最大采样点的总数
max_sampling_total <- max(sampling_point_totals$total, na.rm = TRUE)


# 使用dplyr来计算每个类别的起始和结束角度（用于饼图）
province_data <- province_data %>%
  group_by(city, longitude, latitude) %>%
  arrange(city, category) %>%
  mutate(
    group_total = sum(value),
    end_angle = cumsum(value / group_total) * 2 * pi,
    start_angle = lag(end_angle, default = 0)
  ) %>%
  ungroup() %>%
  left_join(sampling_point_totals, by = c("longitude", "latitude")) %>%
  # 修改radius计算方式
  mutate(radius = 0.15 + (total / max_sampling_total) * 0.3)  # 调整基础大小和缩放因子

# 主地图绘制
gg <- ggplot() +
  geom_sf(data = china_provinces, aes(fill = total), color = "white") +
  scale_fill_gradient(
    low = "#EBD3BC", 
    high = "#EC923E", 
    na.value = "#DFDFDF", 
    name = "total",
    limits = c(0, 100),  # 设置固定的范围，这里以0-100为例
    breaks = seq(0, 100, by = 50)  # 设置刻度，每20个单位一个刻度
  ) +
  new_scale_fill() +
  geom_arc_bar(data = province_data, 
               aes(x0 = longitude, y0 = latitude, 
                   r0 = 0, r = radius,
                   start = start_angle, end = end_angle, 
                   fill = category),
               color = "black", linewidth = 0.1) +
  scale_fill_manual(
    name = "Species",
    values = c(
      "Apodemus chevrieri" = "#EC7189",
      "Rattus rattus alexandrinus" = "#A9B1D9",
      "Mus cookii" = "#F4DA48",
      "Mus domesticus" = "#7B5CA4",
      "Rattus tanezumi" = "#20ADDE",
      "Rattus nitidus" = "#076A71",
      "Rattus norvegicus" = "#E62233",
      "Rattus losea" = "#FFFF99",
      "Bandicota indica" = "#DCB9D7"
    )
  ) +
############################
  #coord_sf(
    #xlim = c(109.5, 117.5),    # 广东省经度范围
    #ylim = c(20, 26),         # 广东省纬度范围
    #expand = FALSE
  #) +
##########################
  coord_sf(
    xlim = c(97.5, 106.2),  # 云南省经度范围
    ylim = c(21.1, 29.2),   # 云南省纬度范围
    expand = FALSE
  ) +
#########################
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold")
  )

# 相应调整图例中点的大小计算
sample_sizes <- c(5, 15, max_sampling_total)
legend_data <- data.frame(
  size = sample_sizes,
  x = rep(0, length(sample_sizes)),
  y = seq(0.8, -0.8, length.out = length(sample_sizes))
)

# 调整图例点的大小以匹配主图
size_legend <- ggplot(legend_data, aes(x, y)) +
  geom_point(aes(size = size), shape = 21, fill = "white", color = "black", stroke = 0.5) +
  scale_size_area(max_size = 8, limits = c(0, max(sample_sizes))) +  # 调整最大大小
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1), clip = "off") +
  annotate("text", x = rep(0.6, 3), 
           y = seq(0.8, -0.8, length.out = length(sample_sizes)), 
           label = paste("n =", sample_sizes), 
           hjust = 0, size = 3) +
  labs(title = "Sample size")

# 组合主图和图例
final_plot <- grid.arrange(gg, size_legend, ncol = 2, widths = c(5, 1))

# 保存图片
ggsave("yunnan_sampling_map.pdf", final_plot, width = 12, height = 8, units = "in")
