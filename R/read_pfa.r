# =========================================
# read_pfa.r
#
# Basic file reader for PFA data
#
# 14/04/2021 First coding
# =========================================

library(tidyverse)
library(lubridate)
require(RJSONIO)  # for finding dropbox folder

source("r/theme_publication.r")
source("r/lowcase.r")
source("r/get_dropbox.r")
source("r/loadRDataObject.r")
source("r/encode_zchords.r")

dropboxdir <- file.path(get_dropbox(), "SPFA-PFA_PandoraData")

#load generic data
load(file.path(dropboxdir,"rdata","world.df.RData"))
load(file.path(dropboxdir,"rdata","fao.df.RData"))

# load PFA data
h <- loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_haul_extraction_for_pandora.RData"))
l <- loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_length_extraction_for_pandora.RData"))
b <- loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_bio_extraction_for_pandora.RData"))

# Test: plot by haul
invisible(gc())
xrange <- range(h$shootlon, na.rm=TRUE)
yrange <- c(min(h$shootlat, na.rm=TRUE),64)

tt <-
  h %>% 
  mutate(year = year(date)) %>% 
  group_by(year, species) %>% 
  summarise(n = n())

h %>% 
  mutate(year = year(date)) %>% 
  
  ggplot(aes(shootlon, shootlat)) + 
  theme_publication() +
  theme(panel.border     = element_rect(colour="black" , size=0.2),
        panel.grid.major = element_blank(),
        strip.background = element_rect(colour="black", size =0.2),
        plot.margin      = unit(c(0,0,0,0),"cm"),
        plot.title       = element_text(vjust=0, size=10, hjust=0),
        axis.text        = element_text(size=6),
        axis.title       = element_blank(),
        legend.key.width = unit(0.5, "cm")) +
  
  # coord_quickmap() +
  coord_quickmap(xlim=xrange , ylim=yrange) +
  geom_polygon(data=world.df, aes(long, lat, group=group), fill = "gray90") +
  geom_point(aes(size=catch, colour=species), alpha = 0.4, shape=20) +
  geom_text(data=tt, aes(label = paste("n=", n)), colour="black",
            x=-Inf, y=Inf, hjust=0, vjust=1, size=3) +
  scale_size(range = c(0.01,5)) +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = NULL, y = NULL, size = "t/haul", title="Catch by species") +
  # facet_wrap(~ month, ncol=6)
  facet_grid(year ~ species)


# Example of LF by lat/long and year
l %>% 
  filter(!is.na(shootlon), !is.na(shootlat)) %>% 
  filter(shootlat <= 62, shootlat >= 54) %>% 
  filter(shootlon >= -2, shootlon <= 2) %>% 
  filter(species == "her") %>% 
  filter(year(date) >= 2018) %>% 
  mutate(year = year(date)) %>% 
  mutate(hid = paste0(tid, haul)) %>% 
  mutate(rect   = encode_zchords(shootlon, shootlat,dx = 0.5,dy = 0.5)) %>% 
  separate(rect, c("shootlon", "shootlat"), sep = ":", convert = TRUE, remove = FALSE) %>% 
  mutate(shootlat = factor(shootlat, levels = rev(sort(unique(tmp$shootlat))))) %>% 
  
  ggplot(aes(length, prop, group=hid)) + 
  theme_publication() +
  theme(panel.border     = element_rect(colour="black" , size=0.2),
        panel.grid.major = element_blank(),
        panel.spacing    = unit(0.5,"mm"),
        strip.background = element_rect(colour="black", size =0.2),
        plot.margin      = unit(c(0,0,0,0),"cm"),
        plot.title       = element_text(vjust=0, size=10),
        axis.text        = element_text(size=6),
        axis.title       = element_blank(), 
        legend.key.width = unit(0.5, "cm")) +
  geom_line(aes(colour=factor(year)), alpha = 0.4) +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = NULL, y = NULL, size = "t/haul", title="Herring LF by long/lat") +
  # facet_wrap(~ month, ncol=6)
  facet_grid(shootlat ~ shootlon)
