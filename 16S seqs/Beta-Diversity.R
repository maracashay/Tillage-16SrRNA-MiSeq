pruned.otu <- data.frame(ps.4@otu_table)
map.pruned <- data.frame(ps.4@sam_data)
# transform via center-log ratio
ps_clr <- microbiome::transform(ps.4, "clr")   
# calculate distance matrix from CLR transformed ASV table with Euclidean distance
clr_dist_matrix <- phyloseq::distance(ps_clr, method = "euclidean") 
#ADONIS test
bc.perm.core <- adonis(clr_dist_matrix ~ Crop * Type * Till, data = map.pruned)
bc.perm.core

pairwise.perm.manova(clr_dist_matrix, map.pruned$Crop_type ,nperm=100)
pairwise.perm.manova(clr_dist_matrix, map.pruned$Till ,nperm=100)


# Plotting PCoA# 
p1 <- plot_ordination(ps_clr, gpca, "samples", color = "Date")
plot_ordination(ps_clr, gpca, "samples", color = "Crop")

man.shapes = c(15, 16)
colvecC <- c("gray30"  "#56B4E9")

pdf("PCoA-CLR-Compartment-Crop.pdf")
plot(p1.df$Axis.1, p1.df$Axis.2,
     xlab = paste("PCoA 1 (6%)", sep = ""),
     ylab = paste("PCoA 2 (5%)", sep = ""),pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(p1.df$Axis.1, p1.df$Axis.2, pch = man.shapes[p1.df$Type], cex = 2, bg = "gray", col = colvecC[p1.df$Crop])
dev.off()

# Run the above code for Corn and Soybean years- but instead of 'Crop', use 'Stage' as an effect
