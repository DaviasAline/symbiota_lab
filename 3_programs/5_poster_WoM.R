# Figures pour poster WoM 
# 27/09/2023
# A. Davias 

# Chargement des packages ----
library(tidyverse)
library(readxl)
source("~/5. R projects/symbiota_lab/3_programs/1_functions.R", echo=TRUE)

# Description des données phyla 3M ----
phyla_3M <-
  read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
  select(CHILD_ID, 2:10)%>%
  mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
  select(ident = CHILD_ID, 
         Firmicutes = `k__Bacteria;p__Firmicutes`, 
         Bacteroidetes =  `k__Bacteria;p__Bacteroidetes`, 
         Proteobacteria = "k__Bacteria;p__Proteobacteria", 
         Actinobacteria = "k__Bacteria;p__Actinobacteria", 
         Cyanobacteria = "k__Bacteria;p__Cyanobacteria", 
         Fusobacteria  = "k__Bacteria;p__Fusobacteria", 
         TM7 = "k__Bacteria;p__TM7", 
         Tenericutes  = "k__Bacteria;p__Tenericutes", 
         Verrucomicrobia = "k__Bacteria;p__Verrucomicrobia") %>%
  mutate_if(is.numeric, ~ . * 100)
phyla_3M_vec <- colnames(phyla_3M[2:10])

add <- bdd_filtered %>% select(ident, BAYLEYQ4_3_12, sIgA_3m, Clusters_Hein_3M) 

phyla_3M <- left_join(phyla_3M, add, by = "ident")%>%
  filter(!is.na(BAYLEYQ4_3_12) & !is.na(sIgA_3m)& !is.na(Clusters_Hein_3M)) %>%
  select(-BAYLEYQ4_3_12, - sIgA_3m, -Clusters_Hein_3M)


bar_order1 <- phyla_3M %>% select(ident, Firmicutes)
bar_order2 <- phyla_3M %>% select(ident, Bacteroidetes)
phyla_3M.long <- gather(phyla_3M, Variable, value, -ident)                 # = pivot_longer()
phyla_3M.long <- left_join(phyla_3M.long, bar_order1, by= "ident")
phyla_3M.long <- left_join(phyla_3M.long, bar_order2, by= "ident")
phyla_3M.long <- phyla_3M.long %>% 
  rename(Firmicute_order = Firmicutes, 
         Bacteroides_order = Bacteroidetes)

phyla_3M.long$Variable <- fct_relevel(
  phyla_3M.long$Variable,
  "Verrucomicrobia", "Tenericutes" , "TM7", "Fusobacteria"  , "Cyanobacteria","Actinobacteria" ,  "Proteobacteria" , "Bacteroidetes"  , "Firmicutes")

barplot.phyla_3M <- phyla_3M.long %>%
  ggplot() +
  aes(x = reorder(ident, -Firmicute_order), 
      y = value, 
      fill = Variable) + 
  geom_col(position = position_fill(), 
           width=1) +
  labs(
    x = "",
    #title = "Distribution of the phyla in the gut microbiota of the children from the CHILD cohort study at 3 months of age",
    y = "Relative abundance (%)") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(
    name = "", 
    # labels =  c("ch_feces_rel_p1_Y1" = "Firmicutes",
    #             "ch_feces_rel_p2_Y1" = "Actinobacteria",
    #             "ch_feces_rel_p3_Y1" = "Bacteroidetes",
    #             "ch_feces_rel_p4_Y1" = "Proteobacteria",
    #             "ch_feces_rel_p5_Y1" = "Verrucomicrobia",
    #             "ch_feces_rel_p10_Y1" = "Tenericutes",
    #             "ch_feces_rel_p6_Y1" = "Candidatus Saccharibacteria",
    #             "ch_feces_rel_p7_Y1" = "Cyanobacteria Chloroplast",
    #             "ch_feces_rel_p9_Y1" = "Fusobacteria",
    #             "ch_feces_rel_p8_Y1" = "Euryarchaeota"), 
    palette = "Paired", 
    direction = - 1)+
  scale_x_discrete(breaks = NULL)  +
  theme_classic() +
  theme(
    text = element_text(family = "serif"),       # use  windowsFonts() to see how to set fonts
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA), 
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_blank(),
    
    axis.title.y = element_text(size=25, face = "bold"),
    axis.text.y = element_text(size = 25), 
    
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.text = element_text(size = 25), 
    legend.key.size = unit(0.5, "cm"), 
    title = element_text(size = 20))+
  guides(fill = guide_legend(nrow = 3))

barplot.phyla_3M
ggsave("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/10. Symbiota lab project/figures/barplot_phyla_3M.tiff", 
       plot = barplot.phyla_3M,
       device = "tiff",
       units = "mm",
       width = 240, 
       height = 170,
       dpi = 300,
       limitsize = FALSE)



# Description des données genres 3M ---- 
# genera_3M <-
#   read_excel("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/5. R projects/symbiota_lab/0_source_data/3months_Taxonomic Abundances L2-L6.xlsx") %>%
#   select(CHILD_ID, 124:255) %>%
#   mutate(CHILD_ID = as.factor(CHILD_ID)) %>%
#   select(ident = CHILD_ID, 
#         "Unamed genus (Enterobacteriaceae family)" = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__",           
#         Bacteroides = "k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__Bacteroidaceae;g__Bacteroides",                 
#         Bifidobacterium = "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Bifidobacteriales;f__Bifidobacteriaceae;g__Bifidobacterium", 
#         Veillonella = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Veillonellaceae;g__Veillonella",                    
#         Streptococcus = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Streptococcaceae;g__Streptococcus",                  
#         "Unamed genus (Clostridiaceae family)" = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Clostridiaceae;g__",                                
#         "Unamed genus (Lachnospiraceae family)" = "k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__Lachnospiraceae;g__",                               
#         Citrobacter = "k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Enterobacteriales;f__Enterobacteriaceae;g__Citrobacter",
#         Actinomyces = "k__Bacteria;p__Actinobacteria;c__Actinobacteria;o__Actinomycetales;f__Actinomycetaceae;g__Actinomyces",         
#         Enterococcus = "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Enterococcaceae;g__Enterococcus") %>%
#   mutate_if(is.numeric, ~ . * 100)
# genera_3M_vec <- colnames(genera_3M[2:11])
# 
# genera_3M <- left_join(genera_3M, add, by = "ident")%>%
#   filter(!is.na(BAYLEYQ4_3_12) & !is.na(sIgA_3m)& !is.na(Clusters_Hein_3M)) %>%
#   select(-BAYLEYQ4_3_12, - sIgA_3m, -Clusters_Hein_3M)
# 
# genera_3M.long <- gather(genera_3M, Variable, value, -ident) 
# genera_3M.long <- genera_3M.long %>% 
#   mutate(
#     ident = as.factor(ident), 
#     Variable = fct_relevel(
#       Variable,
#       "Bacteroides", "Unamed genus (Enterobacteriaceae family)", "Veillonella", "Bifidobacterium", "Unamed genus (Lachnospiraceae family)", 
#       "Streptococcus",  "Unamed genus (Clostridiaceae family)","Actinomyces","Enterococcus",  "Citrobacter" ))
# 
# boxplot.genera_3M <- genera_3M.long %>%
#   ggplot() +
#   aes(x = "", 
#       y = value, 
#       fill = Variable) +
#   geom_boxplot(shape = "circle") +
#   scale_fill_brewer(
#     palette = "Set3", 
#     direction = 1) +
#   labs(
#     x = "",
#     #title = "Distribution of the 10 most abundant genera in the gut microbiota of the children from the CHILD cohort study at 3 months of age",
#     y = "Relative abundance (%)", 
#     fill = ""
#   ) +
#   scale_y_continuous()+
#   theme_classic() +
#   theme(
#     text = element_text(family = "serif"), 
#     panel.background = element_rect(fill = "transparent", colour = NA),
#     plot.background = element_rect(fill = "transparent", colour = NA),
#     legend.background = element_rect(fill = "transparent", colour = NA), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(), 
#     axis.ticks.x = element_blank(),
#     axis.line.x = element_blank(),
#     axis.title = element_text(size = 20, face = "bold"),
#     axis.text.y = element_text(size = 20),
#     legend.text = element_text(size = 20, face = "italic"), 
#     legend.position = "right", 
#     legend.box = "vertical", 
#     legend.key.size = unit(0.5, "cm"), 
#     title = element_text(size = 20))
# 
# boxplot.genera_3M
# ggsave("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/10. Symbiota lab project/figures/boxplot_genera_3M.tiff", 
#        plot = boxplot.genera_3M, 
#        device = "tiff",
#        units = "mm",
#        width = 200, 
#        height = 200,
#        dpi = 300,
#        limitsize = FALSE)


# Description des données metabolites ----
## boxplot ----
bdd_metabolites_long <- bdd_filtered %>% 
  select(ident, sIgA_3m_cat_CutOff5, 
         metabolite_Acetate, 
         metabolite_Lactate,
         metabolite_Propionate, 
         metabolite_Butyrate, 
         metabolite_Formate,
         metabolite_Taurine, 
         metabolite_Valerate) 

var_lab(bdd_metabolites_long$metabolite_Acetate) <- NULL
var_lab(bdd_metabolites_long$metabolite_Propionate) <- NULL
var_lab(bdd_metabolites_long$metabolite_Butyrate) <- NULL
var_lab(bdd_metabolites_long$metabolite_Taurine) <- NULL
var_lab(bdd_metabolites_long$metabolite_Lactate) <- NULL
var_lab(bdd_metabolites_long$metabolite_Formate) <- NULL
var_lab(bdd_metabolites_long$metabolite_Valerate) <- NULL

bdd_metabolites_long <- bdd_metabolites_long %>%
  rename("Acetate" = metabolite_Acetate, 
         "Propio- \n nate" = metabolite_Propionate, 
         "Butyrate" = metabolite_Butyrate, 
         "Taurine" = metabolite_Taurine, 
         "Lactate" = metabolite_Lactate, 
         "Formate" = metabolite_Formate, 
         "Valerate" = metabolite_Valerate) %>%
  pivot_longer(cols = c(-sIgA_3m_cat_CutOff5, -ident)) %>%
  mutate(
    name = fct_relevel(name,
                       "Acetate", "Lactate", "Propio- \n nate", "Butyrate", "Formate",
                       "Valerate", "Taurine"))
bdd_metabolites_long$sIgA_3m_cat_CutOff5 <- bdd_metabolites_long$sIgA_3m_cat_CutOff5 %>%
  fct_recode(
    "≥5 ng/dL" = ">=5 ng/dL")

boxplot_metabolites_3M <- 
  ggplot(bdd_metabolites_long) +
  aes(x = "", y = value, fill = sIgA_3m_cat_CutOff5) +
  geom_boxplot() +
  scale_fill_manual(
    values = c(`<5 ng/dL` = "#F8766D",
               `≥5 ng/dL` = "#5EB13F")
  ) +
  labs(
    y = "Metabolites concentrations at 3 months",
    x = "",
    fill = "sIgA level at 3 months"
  ) +
  theme_bw() +
  facet_wrap(~name, scales = "free", ncol = 7) + 
  theme(
    axis.ticks.x = element_blank(), 
    text = element_text(family = "serif"),
    axis.title.y = element_text(size = 25, face = "bold"),
    axis.text.y = element_text(size = 25), 
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size = 20), 
    axis.title = element_text(size = 20), 
    legend.title = element_text(size = 25), 
    legend.text = element_text(size = 25), 
    legend.position = "bottom", 
    #panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),  # enleve tout le fond sauf legend 
    legend.background = element_rect(fill = "transparent", colour = NA))
boxplot_metabolites_3M

ggsave("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/10. Symbiota lab project/figures/boxplot_metabolites_3M.tiff", 
       plot = boxplot_metabolites_3M, 
       device = "tiff",
       units = "mm",
       width = 250, 
       height = 170,
       dpi = 300,
       limitsize = FALSE)

## histogram ----
histogram_metabolites_3M <- bdd_filtered %>% 
  rename("Acetate" = metabolite_Acetate, 
         "Propionate" = metabolite_Propionate, 
         "Butyrate" = metabolite_Butyrate, 
         "Tryptophan" = metabolite_Tryptophan, 
         "Taurine" = metabolite_Taurine, 
         "Lactate" = metabolite_Lactate, 
         "Formate" = metabolite_Formate) %>%
  histogram(vars = c("Propionate", 
                     "Butyrate", 
                     "Tryptophan", 
                     "Taurine",
                     "Lactate",
                     "Formate"), 
            order_var = c("Propionate", 
                          "Butyrate", 
                          "Tryptophan", 
                          "Taurine",
                          "Lactate",
                          "Formate"), 
            ncol = 3)


histogram_metabolites_3M_high <- bdd_filtered %>% 
  filter(sIgA_3m_cat_CutOff5 == ">=7 ng/dL") %>%
  rename("Acetate" = metabolite_Acetate, 
         "Propionate" = metabolite_Propionate, 
         "Butyrate" = metabolite_Butyrate, 
         "Tryptophan" = metabolite_Tryptophan, 
         "Taurine" = metabolite_Taurine, 
         "Lactate" = metabolite_Lactate, 
         "Formate" = metabolite_Formate) %>%
  histogram(vars = c("Propionate", 
                     "Butyrate", 
                     "Tryptophan", 
                     "Taurine",
                     "Lactate",
                     "Formate"), 
            order_var = c("Propionate", 
                          "Butyrate", 
                          "Tryptophan", 
                          "Taurine",
                          "Lactate",
                          "Formate"), 
            ncol = 3)

histogram_metabolites_3M_low <- bdd_filtered %>% 
  filter(sIgA_3m_cat_CutOff5 == "<7 ng/dL") %>%
  rename("Acetate" = metabolite_Acetate, 
         "Propionate" = metabolite_Propionate, 
         "Butyrate" = metabolite_Butyrate, 
         "Tryptophan" = metabolite_Tryptophan, 
         "Taurine" = metabolite_Taurine, 
         "Lactate" = metabolite_Lactate, 
         "Formate" = metabolite_Formate) %>%
  histogram(vars = c("Propionate", 
                     "Butyrate", 
                     "Tryptophan", 
                     "Taurine",
                     "Lactate",
                     "Formate"), 
            order_var = c("Propionate", 
                          "Butyrate", 
                          "Tryptophan", 
                          "Taurine",
                          "Lactate",
                          "Formate"), 
            ncol = 3)


histogram_metabolites_3M
ggsave("C:/Users/Aline/OneDrive - etu.univ-grenoble-alpes.fr/Documents/10. Symbiota lab project/figures/histogram_metabolites_3M.tiff", 
       plot = histogram_metabolites_3M, 
       device = "tiff",
       units = "mm",
       width = 250, 
       height = 145,
       dpi = 300,
       limitsize = FALSE)

