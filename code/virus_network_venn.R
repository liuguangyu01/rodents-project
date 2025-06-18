# Load required libraries
library(tidyr)
library(dplyr)

# Read the data
data <- read.csv("RPM_table.csv", row.names = 1, check.names = FALSE)

# Add sample numbers to make unique column names while preserving genus information
genera <- colnames(data)
sample_names <- paste0("sample_", seq_along(genera))
names(genera) <- sample_names
colnames(data) <- sample_names

# Convert data to long format and filter RPM >= 1
long_data <- data %>%
  # Add virus names as a column
  tibble::rownames_to_column("virus") %>%
  # Convert to long format
  pivot_longer(cols = -virus,
               names_to = "sample",
               values_to = "RPM") %>%
  # Add genus information
  mutate(genus = genera[sample]) %>%
  # Filter for true positives (RPM >= 1)
  filter(RPM >= 1)

# Function to process each genus
process_genus <- function(genus, data) {
  # Get unique viruses present in this genus
  genus_viruses <- data %>%
    filter(genus == !!genus) %>%
    distinct(virus) %>%
    arrange(virus) %>%
    pull(virus)
  
  # Skip if no viruses found
  if(length(genus_viruses) == 0) {
    cat("No viruses found for genus:", genus, "\n")
    return()
  }
  
  # Save to file
  filename <- paste0(genus, "_viruses.txt")
  
  # Write viruses list to file
  writeLines(genus_viruses, filename)
  
  cat("Created file for", genus, "\n")
}

# Process each genus
for(genus in unique(genera)) {
  process_genus(genus, long_data)
}

cat("Analysis complete! Files have been created for each genus.\n")



library(VennDiagram)  # 引入VennDiagram包，用于绘制文氏图

# 定义输出文件路径
outFile <- "intersectGenes.txt"  # 输出交集基因文件的路径
outPic <- "venn.pdf"  # 输出图片文件的路径


# 获取目录下所有文本文件
files <- dir(pattern = "\\.txt$")  # 使用正则表达式提取TXT结尾的文件
geneList <- list()

# 读取所有txt文件中的基因信息，保存到geneList
for (inputFile in files) {
  if (inputFile == outFile) {
    next  # 跳过输出文件
  }
  rt <- read.table(inputFile, header = FALSE)  # 读取文件
  geneNames <- as.vector(rt[, 1])  # 提取基因名
  geneNames <- gsub("^ | $", "", geneNames)  # 去掉基因首尾的空格
  uniqGene <- unique(geneNames)  # 去重
  header <- unlist(strsplit(inputFile, "\\.|\\-"))[1]  # 提取文件名作为标识
  geneList[[header]] <- uniqGene
  uniqLength <- length(uniqGene)
  print(paste(header, uniqLength, sep = " "))  # 打印每个文件的基因数量
}

# 绘制文氏图
venn.plot <- venn.diagram(geneList, filename = NULL, fill = rainbow(length(geneList)))
pdf(file = outPic, width = 5, height = 5)
grid.draw(venn.plot)
dev.off()

# 保存交集基因
intersectGenes <- Reduce(intersect, geneList)
write.table(intersectGenes, file = outFile, sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)

