library(tidyverse)
library(ggridges)

Data <- read.csv("_SharedFolder_article-turnout-lifestyles/data/CleanData_Lifestyle_merged2022-04-04.csv") %>%
  select(c(X.1, immigrant, op_potentialG_Lib, op_potentialG_Cons, op_potentialG_Ndp, op_potentialG_BQ, op_potentialG_PV,
           op_potentialG_CAQ, op_potentialG_PLQ, op_potentialG_PQ, op_potentialG_QS))


# Essayer de changer les esti de couleurs

fills1 <- c("op_potentialG_CAQ" = "#00FFFF",
            "op_potentialG_PLQ" = "#FF0024",
            "op_potentialG_PQ" = "#099FFF",
            "op_potentialG_QS" = "#FF6600")

fills2 <- c("op_potentialG_Lib" = "#FF0024", 
              "op_potentialG_Cons" = "#099FFF", 
              "op_potentialG_Ndp" = "#FF6600", 
              "op_potentialG_BQ" = "#00FFFF", 
              "op_potentialG_PV" = "#12B848")

# Noms pour les facettes
partyNamesProv <- c("op_potentialG_CAQ" = "CAQ", 
                    "op_potentialG_PLQ" = "PLQ", 
                    "op_potentialG_PQ" = "PQ", 
                    "op_potentialG_QS" = "QS")

partyNamesFed <- c("op_potentialG_Lib" = "PLC", 
                   "op_potentialG_Cons" = "PCC", 
                   "op_potentialG_Ndp" = "NPD", 
                   "op_potentialG_BQ" = "BQ", 
                   "op_potentialG_PV" = "PVC")



# Appui au provincial

ProvData <- Data %>%
  select(X.1, immigrant, 8:11) %>%
  na.omit() %>%
  pivot_longer(., 3:6,
               names_to = "parti",
               values_to = "potGrowth") %>%
  mutate(immigrant = factor(immigrant))

ggplot(ProvData, aes(x = potGrowth, y = immigrant, fill = parti)) +
  geom_density_ridges(
    aes(fill = parti, color = parti),
    alpha = 0.6,
    size = 1,
    #calc_ecdf = TRUE,
    #quantile_lines=TRUE,
    bandwidth=0.1,
    show.legend = F) +
  scale_y_discrete(labels=c("\nImmigrants", "\nNon immigrants")) +
  scale_x_continuous(name="\nPotentiel d'appui envers le parti\n", limits=c(0, 1), breaks=c(0.1, 0.9), 
                     labels=c("\nFaible", "\nÉlevé")) + 
  scale_fill_manual(values = fills1) +
  scale_colour_manual(values = fills1) +
  facet_wrap(~parti, nrow = 1, labeller = as_labeller(partyNamesProv)) +
  theme_bw() +
  ylab("") +
  xlab("Potentiel d'appui envers le parti") +
  ggtitle("\nDistribution du potentiel d'appui selon le parti provincial\n") +
  theme(title=element_text(size=25, face="bold"), 
        axis.text.x = element_text(size = 18, color="black",  face = "bold"),
        axis.text.y = element_text(size = 18, color="black", face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "black", color = "black"),
        strip.text = element_text(color = "grey"),
        plot.title = element_text(hjust = 0.5)) 


ggsave("_SharedFolder_article-turnout-lifestyles/graphs/plotProvData.pdf", width = 24, height = 12)

# Appui au fédéral 

FedData <- Data %>%
  select(X.1, immigrant, 3:7) %>%
  na.omit() %>%
  pivot_longer(., 3:7,
               names_to = "parti",
               values_to = "potGrowth") %>%
  mutate(immigrant = factor(immigrant))

ggplot(FedData, aes(x = potGrowth, y = immigrant, color = parti)) +
  geom_density_ridges(
    aes(fill = parti, color = parti),
    alpha = 0.6,
    size = 1,
    #calc_ecdf = TRUE,
    #quantile_lines=TRUE,
    bandwidth=0.1,
    show.legend = F) +
  scale_y_discrete(labels=c("\nImmigrants", "\nNon immigrants")) +
  scale_x_continuous(name="\nPotentiel d'appui envers le parti\n", limits=c(0, 1), breaks=c(0.1, 0.9), 
                     labels=c("\nFaible", "\nÉlevé")) + 
  scale_fill_manual(values = fills2) +
  scale_colour_manual(values = fills2) +
  facet_wrap(~parti, nrow = 1, labeller = as_labeller(partyNamesFed)) +
  theme_bw() +
  ylab("") +
  xlab("Potentiel d'appui envers le parti") +
  ggtitle("\nDistribution du potentiel d'appui selon le parti provincial\n") +
  theme(title=element_text(size=25, face="bold"), 
        axis.text.x = element_text(size = 18, color="black",  face = "bold"),
        axis.text.y = element_text(size = 18, color="black", face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "black", color = "black"),
        strip.text = element_text(color = "grey"),
        plot.title = element_text(hjust = 0.5)) 

ggsave(file = "_SharedFolder_article-turnout-lifestyles/graphs/plotFedData.pdf", width = 24, height = 12)