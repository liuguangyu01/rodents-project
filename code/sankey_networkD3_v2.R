# 加载必要的包
library(networkD3)
library(tidyverse)
library(readxl)
library(htmlwidgets)
library(webshot)

# 读取数据
data <- read.table("data2.txt", 
                   header = TRUE, 
                   sep = "\t", 
                   stringsAsFactors = FALSE)

# 定义每一层节点的顺序 - 移除 class_order 和 order_order
kingdom_order <- unique(data$Kingdom)
phylum_order <- unique(data$Phylum)
family_order <- unique(data$Family)
genus_order <- unique(data$Genus)
species_order <- unique(data$Species[data$Species != ""])

# 创建节点数据并按自定义顺序排序 - 移除 class_order 和 order_order
nodes <- data.frame(
  name = c(kingdom_order, phylum_order, family_order, genus_order, species_order)
)

# 创建连接数据，跳过Class和Order层级
links <- bind_rows(
  # Kingdom to Phylum
  data %>% 
    group_by(Kingdom, Phylum) %>%
    summarise(value = sum(counts), .groups = 'drop') %>%
    rename(source = Kingdom, target = Phylum),
  
  # Phylum to Family（直接从Phylum连接到Family）
  data %>% 
    group_by(Phylum, Family) %>%
    summarise(value = sum(counts), .groups = 'drop') %>%
    rename(source = Phylum, target = Family),
  
  # Family to Genus
  data %>% 
    group_by(Family, Genus) %>%
    summarise(value = sum(counts), .groups = 'drop') %>%
    rename(source = Family, target = Genus),
  
  # Genus to Species
  data %>% 
    filter(!is.na(Species) & Species != "") %>%  
    group_by(Genus, Species) %>%
    summarise(value = sum(counts), .groups = 'drop') %>%
    rename(source = Genus, target = Species)
)

# 将source和target转换为节点索引
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# 创建随机颜色生成函数
generateRandomColor <- function() {
  # 使用rgb生成随机颜色，确保颜色不会太浅（最大值设为0.9）
  r <- runif(1, 0.2, 0.9)
  g <- runif(1, 0.2, 0.9)
  b <- runif(1, 0.2, 0.9)
  
  # 转换为16进制颜色代码
  return(rgb(r, g, b))
}

# 计算需要的颜色数量
n_colors_needed <- length(nodes$name)

# 生成随机颜色列表
set.seed(123)  # 设置随机种子以保证颜色的可重复性
custom_colors <- sapply(1:n_colors_needed, function(x) generateRandomColor())

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])',
                       paste(custom_colors, collapse = '", "'))

# 创建颜色比例字符串
colourScale <- sprintf('d3.scaleOrdinal().range(["%s"])',
                       paste(custom_colors, collapse = '", "'))

# 创建桑基图
p <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  fontSize = 8,
  nodeWidth = 20,
  height = 900,
  width = 1400,
  sinksRight = FALSE,
  iterations = 5,
  nodePadding = 5,
  colourScale = colourScale
)

# 渲染增强
p1 <- htmlwidgets::onRender(p, '
  function(el, x) {
    var svg = d3.select(el).select("svg");

    // 创建一个合法的ID, 防止名称中有空格或者-等特殊字符
    function createValidID(name) {
      if (!name) {
        return "unknown"; // 如果名称是null或undefined，返回一个默认值
      }
      // 将非字母数字字符替换为下划线，包括空格和-
      return name.replace(/[^a-zA-Z0-9-]/g, "_");
    }
    
    // 收集要删除的节点名称（NA）
    var nodesToRemove = [];
    svg.selectAll(".node").each(function(d) {
      if (!d.name || d.name === "NA") {
        nodesToRemove.push(d.name);
      }
    });

    // 删除标签为NA的节点及其直接相关的链接
    nodesToRemove.forEach(function(nodeName) {
      // 找到并删除与该节点相关的所有直接链接
      svg.selectAll(".link").filter(function(link) {
        return link.target.name === nodeName;
      }).remove();
      
      // 删除节点
      svg.selectAll(".node").filter(function(node) {
        return node.name === nodeName;
      }).remove();
    });

    // 创建渐变色
    svg.selectAll(".link").each(function(d) {
      // 使用 source.name 和 target.name 作为渐变ID的一部分
      var gradientID = "gradient-" + createValidID(d.source.name) + "-" + createValidID(d.target.name);

      // 创建渐变
      var gradient = svg.append("defs")
        .append("linearGradient")
        .attr("id", gradientID)
        .attr("gradientUnits", "userSpaceOnUse")
        .attr("x1", d.source.x + d.source.dx / 2)
        .attr("y1", d.source.y + d.source.dy / 2)
        .attr("x2", d.target.x + d.target.dx / 2)
        .attr("y2", d.target.y + d.target.dy / 2);

      // 起始颜色（源节点颜色）
      var sourceColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.source.name; }).select("rect").style("fill");
      var targetColor = d3.select(el).selectAll(".node").filter(function(node) { return node.name === d.target.name; }).select("rect").style("fill");

      gradient.append("stop")
        .attr("offset", "0%")
        .attr("stop-color", sourceColor);

      // 终止颜色（目标节点颜色）
      gradient.append("stop")
        .attr("offset", "100%")
        .attr("stop-color", targetColor);

      // 设置链接的颜色为渐变色
      d3.select(this).style("stroke", "url(#" + gradientID + ")");
    }); 
    
    // 修改节点矩形框的颜色
    svg.selectAll(".node rect")
      .style("stroke", "black") // 设置矩形框的边框颜色
      .style("stroke-width", "1.5px"); // 设置矩形框的边框粗细
  }
')

# 显示和保存
p1
saveNetwork(p1, "sankey2.html")
webshot("sankey2.html", "sankey2.pdf")
