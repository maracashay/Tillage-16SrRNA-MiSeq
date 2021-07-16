##### family-level phyloseq object ####
# I use taxa_level from the microbiomeSeq package
ps.fam <- taxa_level(ps.rel, 'Family')

ps.till <- subset_samples(ps.fam, Till == "NT" | Till == "CD")
ps.till.bulk <- subset_samples(ps.fam, Type == "Bulk")

ps.till2 <- subset_samples(ps.fam, Till == "NT" | Till == "MP")
ps.till2.bulk <- subset_samples(ps.fam, Type == "Bulk")

ps.till3 <- subset_samples(ps.fam, Till == "CD" | Till == "MP")
ps.till3.bulk <- subset_samples(ps.fam, Type == "Bulk")


ps.phl.sc.rhiz@sam_data$Crop <- factor(ps.phl.sc.rhiz@sam_data$Crop,levels = c("Soy", "Corn"))

ex5 <- differentialTest(formula = ~ Till + Block,
                        phi.formula = ~ Till + Block, formula_null = ~Block,
                        phi.formula_null = ~ Block,
                        data = ps.phl.till3.bulk,
                        test = "Wald", boot = FALSE,
                        fdr_cutoff = 0.05)
plot(ex5)
