#We will assume that you have basic knowledge of the tidyverse, data loading, data cleaning, etc. 

library(tidyverse)
setwd("C:/Users/Debajyoti Saha/Documents/GitHub/Sta-R-tistics/")

#Slide 5: 

#Example 1: boxplot
#Read in mouse data
mouse_protein <- read_csv(file = "datasets/MouseProtein_Kaggle/Data_Cortex_Nuclear.csv")

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype)) + geom_boxplot() + ggtitle("CREB expression")

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Genotype)) + geom_boxplot() + theme_classic()

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Treatment)) + geom_boxplot() + theme_classic()

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Genotype)) + geom_boxplot() + theme_classic() + 
  facet_wrap(~Treatment)

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Genotype)) + geom_boxplot() + theme_classic() + 
  facet_wrap(~Treatment) + geom_jitter(position = ) 

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Genotype, color= Treatment)) + geom_boxplot() + 
  theme_classic() + 
  facet_wrap(~Treatment) + geom_jitter() +
  scale_fill_brewer(type = "seq", palette = 1)

ggplot(data = mouse_protein, aes(y = CREB_N, x = Genotype, fill = Genotype, color= Treatment)) + geom_boxplot() + 
  theme_classic() + 
  facet_wrap(~Treatment) + geom_jitter() +
  scale_color_manual(values = c("grey", "lightblue")) +
  scale_fill_brewer(type = "seq", palette = 1)

#Slide 6:

#Example 2: Violin plot
#Read in mental health in tech data
mental_health_tech <- read_csv("datasets/MentalHealthTech_Kaggle/survey.csv") %>% 
  filter(Age < 100 & Gender %in% c("Male", "Female") & Country %in% c("United Kingdom", "United States"))

ggplot(data = mental_health_tech, aes(y= Age, x=Country, fill= Gender)) +
  geom_violin(position = position_dodge(1)) + 
  stat_summary(fun = median, geom="point", size=2, color="black", position = position_dodge(1)) + 
  theme_bw() + scale_fill_brewer(type = "qual", palette = 5)

ggplot(data = mental_health_tech, aes(y= Age, x=Country, fill= Gender)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  theme_bw() + scale_fill_brewer(type = "qual", palette = 5) 

ggplot(data = mental_health_tech, aes(y= Age, x=Country, fill = Gender)) +
  geom_violin() + theme_bw() + scale_fill_brewer(type = "qual", palette = 5) +
  facet_wrap(~Gender) +
  geom_boxplot(alpha=0.2, width = 0.1, color = "#000000")  

ggplot(data = mental_health_tech, aes(y= Age, x=Country, fill = Gender)) +
  geom_violin() + theme_bw() + scale_fill_brewer(type = "qual", palette = 5) +
  facet_wrap(~Gender) +
  geom_boxplot(alpha=0.2, width = 0.1, color = "#000000") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

#slide 7:

#Example 3: density plot
#read in the Billboard Hot Weekly charts data

billboard_weekly <- read_tsv(file = "datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv") %>% drop_na()

ggplot(data = billboard_weekly, aes(x= danceability)) + geom_density() + theme_light()

billboard_weekly %>% group_by(Performer) %>% summarise(number = n()) %>% 
    filter(number > 50) %>% select(Performer) %>% unlist() -> performers_to_plot

billboard_weekly_subset <- billboard_weekly %>% filter(Performer %in% performers_to_plot)

ggplot(data = billboard_weekly_subset, aes(x= danceability, color = Performer, fill = Performer, alpha=0.3)) + 
  geom_density() + theme_light()

ggplot(data = billboard_weekly_subset, aes(x= danceability, color = Performer, fill = Performer, alpha=0.3)) + 
  geom_histogram(aes(y = ..density..), position = "identity") +
  geom_density() + theme_light()  + facet_grid(~Performer) + 
  theme(text =  element_text(size = 12), axis.text = element_text(angle = 45))

#Slide 8:
library(rstatix)
library(ggpubr)

#Helpful post for p-values
#https://www.datanovia.com/en/blog/how-to-add-p-values-onto-a-grouped-ggplot-using-the-ggpubr-r-package/

#Example 4: bar plots with error bars
std_err <- function(x) {
  sd(x)/sqrt(length(x))  
}

mouse_protein %>% drop_na %>% select(Genotype, Treatment, IL1B_N, pCFOS_N) %>%
  pivot_longer(cols = c("IL1B_N", "pCFOS_N"), names_to = "Gene", values_to = "Exp") %>% 
  group_by(Gene, Genotype, Treatment) %>%
  summarise_all(.funs = c(Avg = mean, se = std_err)) -> plot_proteins
  
ggplot(data = plot_proteins, aes(y = Avg, x = Gene, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge())

ggplot(data = plot_proteins, aes(y = Avg, x = Gene, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(~Genotype) + theme_bw()

ggplot(data = plot_proteins, aes(y = Avg, x = Gene, fill = Treatment)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Avg-se, ymax = Avg+se), width = 0.1, position = position_dodge(0.9)) +
  facet_grid(~Genotype)+ theme_bw() + scale_fill_brewer(type = "seq", palette = 7) -> bar_plot_fancy

bar_plot_fancy

#Slide 9:

#Example 5:
#bar plot with error bars and significance stars
#https://cran.r-project.org/web/packages/ggsignif/vignettes/intro.html

gene_plotter <- function(gene, plot_df) {
  plot_df %>% drop_na %>% select(all_of(c("Genotype", "Treatment", gene))) %>%
    group_by(Genotype, Treatment) %>%
    summarise_all(.funs = c(Avg = mean, se = std_err)) -> plot_proteins
  
  t.test_formula <- as.formula(paste0(gene, "~Treatment"))
  
  plot_df %>% drop_na %>% select(all_of(c("Genotype", "Treatment", gene))) %>%
    group_by(Genotype) %>% t_test(t.test_formula) %>% adjust_pvalue(method = "bonferroni") %>%
    add_significance("p.adj") -> stat_test
  
  stat_test <- add_x_position(stat_test, group = "Treatment", x = "Genotype", dodge = 0)
  
  p <- ggplot(data = plot_proteins, aes(y = Avg, fill = Treatment, x = Genotype)) + 
    geom_bar(stat = "identity", position = position_dodge()) + scale_fill_brewer(type = "seq", palette = 6) +
    geom_signif(y_position = max(plot_proteins$Avg)+ 0.2*max(plot_proteins$Avg),
                annotation = stat_test$p.adj.signif, 
                xmin = stat_test$xmin, xmax = stat_test$xmin, extend_line = 0.1) +
    geom_errorbar(aes(ymin = Avg-se, ymax = Avg+se), width = 0.1, position = position_dodge(0.9)) +
    theme_bw() + ggtitle(gene)
}

p1 <- gene_plotter("IL1B_N", mouse_protein)
p2 <- gene_plotter("pCFOS_N", mouse_protein)

class(p1)

library(patchwork)

p1+p2

p1/p2

p1+p2 + plot_layout(ncol = 1, guides = "collect")

#Slide 10

# Example 6: 
#Plotting in loops

for(gene_to_plot in names(mouse_protein)[2:7]) {
  #gene_plotter(gene_to_plot, mouse_protein)
  print(gene_plotter(gene_to_plot, mouse_protein))
}

plots_list <- lapply(names(mouse_protein)[2:7], gene_plotter, plot_df = mouse_protein)

plots_list

reduce(plots_list, `+`) -> plots_patch

class(plots_patch)

plots_patch + plot_layout(ncol = 3, guides = "collect")

#Slide 11:

#Example 7:
#Plotting correlations

library(psych)

billboard_weekly %>% select_if(is.numeric) -> billboard_weekly_numeric

corr_res <- corr.test(billboard_weekly_numeric)

corrplot::corrplot(corr_res$r, p.mat = corr_res$p, method = "ellipse")

corrplot::corrplot(corr_res$r, p.mat = corr_res$p, method = "number", 
                   insig = "blank", sig.level = 0.05, tl.col = "black", mar = c(2,2,2,2)) -> billboard_corr_plot

#a little extra
corrplot::corrplot(corr_res$r, p.mat = corr_res$p, method = "number", 
                   insig = "blank", sig.level = 0.05, tl.col = "black", mar = c(2,2,2,2)) -> billboard_corr_plot

billboard_corr_plot

print(billboard_corr_plot)

class(billboard_corr_plot)

corrplot::corrplot(corr_res$r, p.mat = corr_res$p, method = "number", 
                   insig = "blank", sig.level = 0.05, tl.col = "black", mar = c(2,2,2,2))
recordPlot() -> billboard_corr_plot

billboard_corr_plot

#Slide 12:

#Example 8:
#Make some scatter plots

ggplot(billboard_weekly, aes(x = energy, y = loudness)) + 
  geom_point() + 
  theme_classic() -> scatter_plot_1

scatter_plot_1

ggplot(billboard_weekly_subset, aes(x = energy, y = loudness, color = Performer, label = Song)) + 
  geom_label() + 
  theme_classic()

ggplot(billboard_weekly_subset, aes(x = energy, y = loudness, color = Performer, shape = Performer)) + 
  geom_point() + 
  geom_smooth(method="gam", se = FALSE, fullrange=FALSE, level=0)+
  theme_classic()

ggplot(billboard_weekly_subset, aes(x = energy, y = loudness, color = Performer, shape = Performer)) + 
  geom_point() + 
  geom_smooth(method="lm", se=TRUE, fullrange= TRUE, level=0.95, linetype = "dashed", fill = "purple", alpha = 0.1)+
  theme_classic() -> scatter_plot_2

scatter_plot_2

#Slide 13:

#Example 9:
#Make a heatmap
#https://jokergoo.github.io/ComplexHeatmap-reference/book/

#BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

billboard_weekly %>% group_by(Performer) %>% summarise(number = n()) %>% 
  filter(number > 30) %>% select(Performer) %>% unlist() -> performers_to_plot

billboard_weekly %>% filter(Performer %in% performers_to_plot) %>% 
  group_by(Performer) %>% 
  select_if(is.numeric) %>% summarise_all(mean) %>%
  column_to_rownames("Performer") %>% as.matrix() %>% t() -> billboard_means

col_row_names <- dimnames(billboard_means)

billboard_means

Heatmap(billboard_means)

billboard_means %>% apply(1, scale) -> billboard_means

billboard_means
rowSums(billboard_means)
colSums(billboard_means)

billboard_means <- t(billboard_means)
rowSums(billboard_means)
colSums(billboard_means)

dimnames(billboard_means) <- col_row_names

Heatmap(billboard_means)

billboard_weekly %>% filter(Performer %in% performers_to_plot) %>% 
  group_by(Performer) %>% summarise(Songs = n()) %>% select(Songs) %>% unlist() -> Songs_per_performer

row_annot <- rowAnnotation(Type = c("Technical", rep("Musical", 11), rep("Technical", 2)), 
                           col = list(Type = c("Technical" = "lightgreen", "Musical" = "yellow"))) 
col_annot <- HeatmapAnnotation(Songs = anno_barplot(Songs_per_performer, heigh = unit(1.5, "cm")), annotation_name_side = "left")

Heatmap(billboard_means, right_annotation = row_annot, bottom_annotation = col_annot)
Heatmap(billboard_means, right_annotation = row_annot, bottom_annotation = col_annot) -> nice_heatmap

class(nice_heatmap)
#By the way, if you want to make hitmaps in a loop and plot them later, you may want to look at the "draw" function

#Slide 14:
#UpSet plots!

library(UpSetR)

danceable_songs <- billboard_weekly %>% filter(danceability > median(danceability)) %>% select(Song) %>% unlist()
loud_songs <- billboard_weekly %>% filter(loudness > median(loudness)) %>% select(Song) %>% unlist()
explicit_songs <- billboard_weekly %>% filter(spotify_track_explicit == TRUE) %>% select(Song) %>% unlist()

upset_list <- list(danceable = danceable_songs, loud = loud_songs, explicit = explicit_songs)

upset(fromList(upset_list))

#Slide 15:
#Saving plots well

#WARNING: Never save your workspace with "save.image" if it contains ggplot objects. 
#They take up a HUGE amount of space when saved to RDS!
#ggsave is also an option

png(file = "C:/Users/Debajyoti Saha/Documents/R_workshop_plot_1.png", height = 600, width = 800)
  scatter_plot_1
dev.off()

pdf(file = "C:/Users/Debajyoti Saha/Documents/R_workshop_plot_2.pdf", height = 8, width = 10, onefile = TRUE)
  scatter_plot_1
  nice_heatmap
dev.off()

pdf(file = "C:/Users/Debajyoti Saha/Documents/R_workshop_plot_3.pdf", height = 8, width = 10, onefile = TRUE)
  scatter_plot_1 %>% ggrastr::rasterize()
  nice_heatmap
dev.off()

svg(file = "C:/Users/Debajyoti Saha/Documents/R_workshop_plot_4.svg", height = 8, width = 10)
  plots_patch + plot_layout(ncol = 3, guides = "collect")
dev.off()

#Slide 16:
# plotting brains 

# install.packages("ggseg")
library(ggseg)

plot(dk) # plot regions of an atlas e,g. desikin killiani 
plot(aseg) # example volumetric atlas

# the key to using ggplot or any of their tools is to format your data correctly, 
# the rest the package will take care of:

# plot only a subset of the ROIS 
someData <- tibble(region = c("transverse temporal", "insula",
                 "precentral","superior parietal"), p = sample(seq(0,.5,.001), 4))
 ggplot(someData) +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = p)) 
 
 # plot only a subset of the ROIS for only the right side!
 someData <- tibble(region = c("transverse temporal", "insula",
                               "precentral","superior parietal"), p = sample(seq(0,.5,.001), 4), hemi= rep('right',4))
 ggplot(someData) +
   geom_brain(atlas = dk, 
              position = position_brain(hemi ~ side),
              aes(fill = p)) + theme_void() # void theme makes it look pretty!


 # let us plot an example of an entire cortex
someData=as.data.frame(dk)
someData$p= sample(seq(0,.5,.001), dim(someData)[1])
ggplot(someData) +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = p)) + theme_void() 

# change colour bar
ggplot(someData) +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = p)) + theme_void() + viridis::scale_fill_viridis(option = 'magma',limits=c(0, 1 ))

# divergent colour bar
ggplot(someData) +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = p)) + theme_void() + scale_fill_gradient2(low = "#67309A", mid = "#FCF7F4", high = "#EC7A48", midpoint = median(someData$p))


#Slide 17:

#Plotting PCA results
#https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

library(ggfortify)

mouse_protein %>% drop_na -> mouse_protein

mouse_protein %>% column_to_rownames("MouseID") %>% select_if(is.numeric) %>% 
  as.matrix() -> mouse_protein_numeric

mouse_protein %>% as.data.frame() -> mouse_protein

mouse_protein_numeric %>% prcomp(center = TRUE, scale. = TRUE) -> mouse_protein_prcomp

mouse_protein_prcomp

plot(y = mouse_protein_prcomp$sdev, x = 1:77)

categories <- c("Behavior", "Genotype", "Treatment", "class") 
types <- c("loading", "frame")

plots_list <- list()

for(plot_type in types) {
  for(category in categories) {
    plots_list[[paste(plot_type, "_", category)]] <- 
      autoplot(mouse_protein_princomp, data = mouse_protein, colour = category, 
                              loadings = plot_type == "loading", frame = plot_type == "frame", frame_type = "norm")
  }
}

(reduce(plots_list[1:4], `+`) * theme_bw()) + plot_layout(ncol = 2)
(reduce(plots_list[5:8], `+`) * theme_bw()) + plot_layout(ncol = 2)

#a little extra

#remotes::install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot::ggbiplot(mouse_protein_prcomp, choices = 1:2)
ggbiplot::ggbiplot(mouse_protein_princomp, choices = 4:5)

#Exercise 1: 
# Make any combination of plots  of your choice with any data of your choice 
# using a loop or apply function and collect the plots in a patchwork 
#(You can look up "datasets" in the help section for some options)
# save your plots to a file in any format of your choice
#For brownie points use the ggdogs, gglime, ggbernie, or ggcats packages 
