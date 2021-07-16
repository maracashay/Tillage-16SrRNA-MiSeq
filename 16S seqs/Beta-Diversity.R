#### perform PERMANOVA on full data (crop and soybean) ####
library(vegan)
ps.bc <- vegdist(otu_table(ps.rel), method = "bray")
bc.permanova <- adonis(ps.bc ~ Crop * Till * Type, data = metadata, nperm= 999)
bc.permanova

library(RVAideMemoire)
pairwise.perm.manova(bc_rt_highcover_ns_abun, map.pruned$Till, nperm = 999)

#### Plotting diversity ordination ####

gpca <- ordinate(ps.rel, "PCoA")

p2 <- plot_ordination(ps.rel, gpca, "taxa")
p1 <- plot_ordination(ps.rel, gpca, "samples", color = "Crop")
p1

p2.df <- data.frame(p2$data)
p2.mapping <- data.frame(p2$mapping)
p1.df <- data.frame(p1$data)

man.shapes = c(0,15,1,16,2,17)

plot(p1.df$Axis.1, p1.df$Axis.2,
     xlab = paste("PCoA 1 (11.5%)", sep = ""),
     ylab = paste("PCoA 2 (9..6%)", sep = ""),pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(p1.df$Axis.1, p1.df$Axis.2, pch = man.shapes[map.pruned$Crop_Type], cex = 2, bg = "gray", col =c("brown3", "brown4", "coral")[map.pruned$Till])
legend("topleft", legend = levels(map.pruned$Crop_Type), col =c("brown3", "brown4", "coral")[map.pruned$Till],pch = man.shapes[map.pruned$Crop_Type], cex = 0.7 )

#### PERMANOVA on either Corn or Soybean ####

ps.soy <- subset_samples(ps.rel, Crop =="Soy")
soy.meta <- data.frame(sample_data(ps.soy))
dist.soy <- vegdist(otu_table(ps.soy), method = "bray")
adonis(dist.soy ~ Stage * Till * Type, data= soy.meta

pairwise.perm.manova(dist.soy, soy.meta$Crop_Type, nperm = 999)

#### plotting ordination for soybean composition ###
gpca <- ordinate(ps.soy, "PCoA")

p2 <- plot_ordination(ps.soy, gpca, "taxa", color = "Class")
p1 <- plot_ordination(ps.soy, gpca, "samples", color = "Stage")

p1.df <- data.frame(p1$data)
p2.df <- data.frame(p2$data)
soy.meta$Stage_Type <- as.factor(paste(soy.meta$Type, soy.meta$Stage))

plot(p1.df$Axis.1, p1.df$Axis.2,
     xlab = paste("PCoA 1 (18.6%)", sep = ""),
     ylab = paste("PCoA 2 (11.8%)", sep = ""),pch = 16, cex = 2.0, type = "n", cex.lab = 1.5, cex.axis = 1.2, axes = FALSE)
axis(side = 1, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
axis(side = 2, labels = T, lwd.ticks = 2, cex.axis = 1.2, las = 1)
abline(h = 0, v = 0, lty = 3)
box(lwd = 2)
points(p1.df$Axis.1, p1.df$Axis.2, pch = man.shapes.2[map.soy$Stage], cex = 2, bg = "gray", col =c("darkolivegreen3","darkolivegreen4")[map.soy$Type])
legend("topleft", legend = levels(map.soy$Stage_Type), col =c("darkolivegreen3","darkolivegreen4")[map.soy$Type],pch = man.shapes.2[map.soy$Stage], cex = 0.7 )
