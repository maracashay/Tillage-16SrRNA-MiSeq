#### Rarefy data based on value lower than lowest sequence count acceptable####
library(phyloseq)
set.seed(500)

ps.rare<-rarefy_even_depth(ps.3, sample.size = 3000)

#### Estimate Alpha-Diversity ####
alpha.div<-estimate_richness(ps.rare, measures=c("Shannon", "Observed"))
alpha.div
map.rare <- data.frame(sample_data(ps.rare))
map.rare$Shannon <- paste(alpha.div$Shannon)

map.rare$Observed <- paste(alpha.div$Observed)

map.rare$Shannon <- as.numeric(map.rare$Shannon)

library(microbiome)
even <- evenness(ps.rare, 'pielou')
map.rare$Evenness <- even

# perform linear mixed effect, repeated measures modeling ####
library(lme4)
library(lmerTest)
library(bestNormalize)
map.rare$Observed <- as.numeric(map.rare$Observed)
be <- bestNormalize::bestNormalize(map.rare$Observed)
be1 <- predict(be)
be
map.rare$Observed.norm <- be1
shannon.model <- lmer(Observed.norm ~ Crop*Till*Type  + (1|Block) + (1|Crop/Date), data = map.rare)
plot(shannon.model)
library(car)
leveneTest(residuals(shannon.model) ~ map.rare$Till*map.rare$Type*map.rare$Crop)
qqnorm(resid(shannon.model))
anova(shannon.model)

#### post-hoc test using emmeans ####
library(emmeans)
emmeans(shannon.model, pairwise ~ Crop | Till)

aggregate(Observed ~ Crop + Type, map.rare, FUN = mean)
aggregate(Observed ~ Crop + Type, map.rare, FUN = st.err)

#### plot data ####
map.rare$Date <- factor(map.rare$Date,levels = c("6.14.18", "7.3.18", "7.13.18", "7.7.19", "7.23.19", "7.31.19"))
library(ggplot2)
ggplot(map.corn.rare, aes(x=Stage, y=Shannon, fill=Till)) + theme_classic() + ylab("Shannon Diversity") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())  + scale_fill_manual(breaks = c("CD", "MP", "NT"), values = c("brown3", "brown4", "coral")) +
  theme(legend.position="none")  +theme(axis.title.x = element_blank(),axis.title.y = element_text(color="black", size=16, face="bold")) + theme(axis.text = element_text(face = "bold", color = "black", size = 16)) + theme(legend.text = element_text(face = "bold", color = "black", size = 16)) + theme(legend.title=element_text(size=16, face = "bold"))  +stat_summary(geom = "bar", fun  = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, lwd =1)

