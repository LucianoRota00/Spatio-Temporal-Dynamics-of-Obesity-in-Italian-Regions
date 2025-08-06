# Load required packages
library(sf)
library(tidyverse)

setwd("C:\\Users\\lucia\\Desktop\\STBetaBayes_Obesity_Italy\\data_and_models")
load('psi_mean')
psi_mean_numeric

# Posterior mean values for each Italian region
posterior_means <- data.frame(
  region = c("Piemonte", "Valle d'Aosta", "Lombardia", "Liguria", "Emilia-Romagna",
             "Trentino-Alto Adige", "Veneto", "Friuli-Venezia Giulia", "Toscana", "Umbria",
             "Marche", "Lazio", "Abruzzo", "Molise", "Campania", "Puglia", 
             "Basilicata", "Calabria", "Sicilia", "Sardegna"),
  psi_mean = psi_mean_numeric
)

# Load Italy map with regions
#italy <- ne_states(country = "Italy", returnclass = "sf")
shape = st_read("C:\\Users\\lucia\\Desktop\\Tirocinio\\codici\\italia_maps",
                layer = 'Reg01012023_g_WGS84')

shape$DEN_REG[!shape$DEN_REG %in% posterior_means$region] = 'Friuli-Venezia Giulia'

min_psi = min(posterior_means$psi_mean)
max_psi = max(posterior_means$psi_mean)

shape %>% left_join(posterior_means, by = join_by(DEN_REG == region)) %>% 
  ggplot()+
  geom_sf(aes(fill = psi_mean))+
  #ggtitle("Map of the Posterior Mean of Spatial Random Effects") +
  theme_bw()+
  scale_fill_gradient(low = 'yellow', high = 'red',
                      limits= c(min_psi, max_psi))+
  theme(legend.position = 'right', 
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = 'bold', size = 13, hjust = 0.5))+
  labs(fill = "Posterior Means")
