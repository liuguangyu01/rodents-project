library(pctax)
library(pcutils)
library(ggplot2)
###示例数据 
###data(otutab,package = "pcutils")
###   https://rdrr.io/cran/pctax/man/b_analyse.html


otutab <- read.table("otutab.txt", header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)
metadata <- read.table("metadata.txt", header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)
taxonomy <- read.table("taxonomy.txt", header = TRUE, sep = "\t", row.names = 1, check.names = FALSE)


# 1. 数据预处理
# 移除全为0的行
otutab <- otutab[rowSums(otutab) > 0, ]


# 2. 提取分组信息
groups <- metadata$Group

# 3. 设置颜色
custom_colors <- c(
  "Pig farm" = "#F9764B",    # 红色
  "Urban areas" = "#15514A"   # 金色
)


#α-diversity analysis
a_diversity(otutab)->a_res
plot(a_res,"Group",metadata,mode = 2)

##env
plot(a_res,"env1",metadata)





#β-diversity analysis 1    PCA
b_analyse(otutab,method = "pca",
          norm = TRUE,
          distance = "jaccard",    ###"bray" or "jaccard"
          group = groups)->b_res
#plot(b_res,"Group",metadata,bi = T,bi_text_size=3,rate=0.2,sample_label=F,stat_ellipse=T)
plot(b_res,"Group",metadata,mode=1,bi = F,
     pal = custom_colors,
     #Group2 = "Site",
     sample_label=F,
     stat_ellipse=T,
     margin = TRUE,            # 添加边缘图
     box_margin = F       # 使用密度图
)

#β-diversity analysis 2    pcoa
otutab <- otutab[rowSums(otutab) > 0, colSums(otutab) > 0]


b_analyse(otutab,method = "pcoa",
          norm = TRUE,
          distance = "jaccard")->b_res    ###nmds or pcoa
#plot(b_res,"Group",metadata,bi = T,bi_text_size=3,rate=0.2,sample_label=F,stat_ellipse=T)
plot(b_res,"Group",metadata,mode=1,bi = T,
     pal = custom_colors,
     sample_label=F,
     stat_ellipse=T,
     margin = TRUE,            # 添加边缘图
     box_margin = FALSE       # 使用密度图
     
)








#Phylogenetic tree1
ann_tree(taxonomy,otutab)->tree
easy_tree(tree,add_abundance=T) 

#Phylogenetic tree2
ann_tree(taxonomy[,c("Kingdom","Class","Order","Family","Genus")],otutab)->tr
easy_tree(tr,add_abundance = T,highlight = "Class") 

#Phylogenetic tree3
ann_tree(taxonomy, otutab) -> tree
easy_tree(tree, add_abundance = FALSE)
easy_tree(tree, add_tiplab = FALSE) -> p
some_tax <- table(taxonomy$Phylum) %>%
  sort(decreasing = TRUE) %>%
  head(5) %>%
  names()
add_strip(p, some_tax)







##Clustering analysis
library(ReporterScore)
pcutils::hebing(otutab, metadata$Group) -> otu_group
cm_test_k(otu_group, filter_var = 0.8)
cm_res <- c_means(otu_group, k_num = 3, filter_var = 0.8)
plot(cm_res, 0.8)
plot(cm_res, 0.8, mode = 2)

#Community assembly
ncm(otutab)->ncm_res
plot(ncm_res)


##RDA
env=metadata[,6:10]
myRDA(otutab,env)->phy.rda
RDA_plot(phy.rda,"Group",metadata)

#Differential analysis
diff_da(otutab, group_df = metadata["Group"], method = "deseq2") -> res
volcano_p(res, mode = 1)
volcano_p(res,mode=2)

library(ggplot2)
kwtest(otutab[1:5, ], metadata["Group"], method = "kruskal.test") -> kw_res
text_df <- data.frame(row.names = kw_res$tax, label = kw_res$p.signif)
multi_bar(otutab[1:5, ], metadata["Group"], mode = 1, text_df = text_df, text_angle = 0) +
  scale_fill_brewer(palette = "Set1")

####Lefse
library(microeco)
data(dataset)
t1 <- trans_diff$new(dataset = dataset, method = "lefse", group = "Group")
t1$plot_diff_bar(use_number = 1:20)
t1$plot_diff_cladogram(use_taxa_num = 100, use_feature_num = 30, select_show_labels = NULL)

###RandomFores
suijisenlin(otutab, metadata["Group"]) -> rf_res
rf_res$imp
