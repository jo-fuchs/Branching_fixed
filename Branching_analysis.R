####Desription######################################################################################################
### Neuron branching analysis part 2
###
###   Goals:
###   - primary summary statistics for each neuron (Numbers and lengths of all categories)
###   - secondary statistic for each neuron (Branches / length, Mean lengths, Total lengths, Axon/Dendrite Ratio)
###   - Create summary Figures
###
###   requires data created through: ImageJ plugin NeuronJ - tracing measurements exported as .txt
###
###   Version 1.1 (08.08.2020)
###   Joachim Fuchs & Vanessa Schulmann
###
##################################################################################################################


#### 1. Load packages, source functions ##########################################################################

library(ggplot2)
library(ggsci)
library(tidyr)
source("SummarizeNeurons.R")


####  2. Analysis  ################################################################################################


# define Directory of Line profiles
folderdir <- dirname(file.choose(new = FALSE))

## Run functions
SummarizeNeurons(folderdir)



## add identifiers, set order of factors
# load data
resultsdir <- file.path(folderdir, "Results")
Axon_results <- read.table(file.path(resultsdir, "Axon_results.txt"), sep = "\t", header = TRUE)
Dendrite_results <- read.table(file.path(resultsdir, "Dendrite_results.txt"), sep = "\t", header = TRUE)
Misclass <- read.table(file.path(resultsdir, "Misclassified.txt"), sep = "\t", header = TRUE)

## merge with data from other folders if required
Total <- merge(Axon_results, Dendrite_results)


## unblinding (if necessary)
# library(readxl)
# Blinding <- read_excel()
#
# Total<-merge(Total, Blinding)

# set order
# Total$Condition<-factor(Total$Condition, levels=c("WT", "KO"))
  


#### 3. Plots ######################################################################################################


plot_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.title = element_text(size = rel(1.15)),
    axis.text.x = element_blank(), axis.ticks.y = element_line(color = "black"), #
    strip.background = element_rect(fill = "grey90", color = NA), # Facet header color
    plot.title = element_text(hjust = 0.5, size = rel(1.3), face = "bold")
  )



# total axon length
axon_plot <- ggplot(Total, aes(x = " ", y = total_axon)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Total Axon length") +
  xlab("") +
  ylab("Total axon length [µm]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
ggsave("Total_axon_length.jpg",
  plot = axon_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Total_axon_length.pdf",
  plot = axon_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# primary axon length
axon_plot <- ggplot(Total, aes(x = " ", y = primary_axon)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Primary Axon length") +
  xlab("") +
  ylab("Primary axon length [µm]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
ggsave("Primary_axon_length.jpg",
  plot = axon_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Primary_axon_length.pdf",
  plot = axon_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Axon branch density
axon_plot <- ggplot(Total, aes(x = " ", y = Total_branches_by_total_length)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Axon branch density") +
  xlab("") +
  ylab("Total branches / total axon length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


axon_plot
ggsave("Branch_density.jpg",
  plot = axon_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Branch_density.pdf",
  plot = axon_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Dendrite length
dend_plot <- ggplot(Total, aes(x = " ", y = total_dendrite)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Total dendrite length") +
  xlab("") +
  ylab("Total dendrite length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
ggsave("Dendrite_length.jpg",
  plot = dend_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Dendrite_length.pdf",
  plot = dend_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Dendrite number
dend_plot <- ggplot(Total, aes(x = " ", y = Number_dendrites)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Dendrites per cell") +
  xlab("") +
  ylab("Dendrites per cell\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
ggsave("Dendrite_number.jpg",
  plot = dend_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Dendrite_number.pdf",
  plot = dend_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Dendrite branches
dend_plot <- ggplot(Total, aes(x = " ", y = Branches_per_dendrite)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Branches per dendrite") +
  xlab("") +
  ylab("Branches per dendrite\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
ggsave("Dendrite_branches.jpg",
  plot = dend_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Dendrite_branches.pdf",
  plot = dend_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Total neurite length
dend_plot <- ggplot(Total, aes(x = " ", y = total_neurite_length)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Total neurite length") +
  xlab("") +
  ylab("Total neurite length [µm]\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
ggsave("Neurite_length.jpg",
  plot = dend_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Neurite_length.pdf",
  plot = dend_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)



# Axon-dendrite ratio
dend_plot <- ggplot(Total, aes(x = " ", y = axon_by_dendrite)) +
  geom_jitter(aes(col = Condition), size = 2, width = 0.3, alpha = 0.5) +
  facet_grid(~Condition) +
  stat_summary(fun.y = mean, fun.ymin = mean, fun.ymax = mean, geom = "crossbar", width = 0.5) + ## Mean line (Boxplot with all borders=mean)
  stat_summary(fun.data = "mean_se", geom = "errorbar", color = "black", width = 0.3) + ## error bars (mean_se = SEM)
  ylim(0, NA) +

  # Axes and Title
  ggtitle("Axon / Dendrite ratio") +
  xlab("") +
  ylab("Total axon length / Total dendrite length\n") +
  plot_theme +
  scale_color_startrek() +
  scale_fill_startrek()


dend_plot
ggsave("Axon_dendrite.jpg",
  plot = dend_plot, device = "jpeg", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
)
ggsave("Axon_dendrite.pdf",
  plot = dend_plot, device = "pdf", path = resultsdir,
  scale = 1, width = 15, height = 15, units = "cm"
) 
        