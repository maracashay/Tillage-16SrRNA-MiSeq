library(lme4)
library(lmerTest)
library(bestNormalize)
bc <- bestNormalize(qpcr$nirK.g)
bc1 <- predict(bc)
bc
qpcr$nirKg.bc <- bc1

nirK.mod <- lmer(nirKg.bc ~ Crop *Tillage * Type + (1|Stage/Crop) + (1|Block), qpcr)

qqnorm(resid(nirK.mod))
plot(nirK.mod)
leveneTest(residuals(nirK.mod) ~ qpcr$Till*qpcr$Type*qpcr$Crop)

anova(nirK.mod)

#### Model ~ Corn only ####
qpcr.c <- qpcr[which(qpcr$Crop == "Corn"), ]
nirK.corn <- lmer(nirKg.bc~ Stage*Tillage*Type + (1|Block), qpcr.c)
qqnorm(resid(nirK.corn))
plot(nirK.corn)
leveneTest(residuals(nirK.mod) ~ qpcr.c$Till*qpcr.c$Type*qpcr.c$Crop)

anova(nirK.corn)
emmeans(nirK.corn, pairwise ~ Stage | Type)
emmeans(nirK.corn, pairwise ~ Type | Stage)

#### Plotting Data ####

ggplot(qpcr.c, aes(x=Stage, y=nirK.g, fill=Stage)) + theme_classic() + ylab("nirK") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + scale_fill_manual(breaks = c("C.V3", "C.V5", "C.V8"), values = c("#E69F00", "#0072B2","#CC79A7")) +
  theme(legend.position="none")  +theme(axis.title.x = element_blank(),axis.title.y = element_text(color="black", size=16, face="bold")) + theme(axis.text = element_text(face = "bold", color = "black", size = 16)) + theme(legend.text = element_text(face = "bold", color = "black", size = 16)) + theme(legend.title=element_text(size=16, face = "bold"))  +stat_summary(geom = "bar", fun  = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, lwd =1)
