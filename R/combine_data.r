# =========================================
# combine_data.r
#
# Basic file reader for PFA data
#
# 14/04/2021 First coding
# 21/04/2021 Making superficial changes
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

load(file.path(dropboxdir,"rdata","icesrectangles.df.RData"))
icesrectangles.df <-
  icesrectangles.df %>% 
  group_by(rect) %>% 
  filter(row_number()==1)

# load haul data
h1 <- 
  loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_haul_extraction_for_pandora.RData")) %>% 
  mutate(
    source="pfa",
    year    = year(date),
    month   = month(date),
    week    = week(date),
    hid    = paste("pfa", tid, stringr::str_pad(haul, pad="0", width=3), sep="_")
  ) %>% 
  rename(
    datetime = haul_time
  )

h2 <- 
  loadRData(file.path(dropboxdir, "SPFA data","spfa_ss_haul.RData")) %>% 
  rename(
    shootlon = londd, 
    shootlat = latdd, 
    date     = dte, 
    vid      = vssl, 
    catch    = est_catch, 
    species  = sp
  ) %>% 
  mutate(
    source  = "spfa", 
    species = tolower(species),
    tid     = paste(vid, stringr::str_pad(trip, pad="0", width=3), sep="_"),
    hid     = paste("spfa", un_hl_id),
    month   = month(date),
    week    = week(date)
  )

# janitor::compare_df_cols(h1, h2)
# combine
h <- 
  bind_rows(h1, h2) %>% 
  dplyr::select(
    vid, tid, hid, date, datetime, year, month, week, 
    shootlat, shootlon, rect,
    source, species, catch
  )


# read length data
l1 <- 
  loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_length_extraction_for_pandora.RData")) %>% 
  mutate(
    source="pfa",
    year    = year(date),
    month   = month(date),
    week    = week(date),
    hid    = paste("pfa", tid, stringr::str_pad(haul, pad="0", width=3), sep="_")
  ) %>% 
  rename(
    datetime = haul_time
  ) %>% 
  ungroup()

l2 <- 
  loadRData(file.path(dropboxdir, "SPFA data","spfa_ss_ltfreq.RData")) %>% 
  rename(
    length = Length_cm, 
    date     = dte, 
    vid      = vssl, 
    species  = sp
  ) %>% 
  mutate(
    source  = "spfa", 
    species = tolower(species),
    tid     = paste(vid, stringr::str_pad(trip, pad="0", width=3), sep="_"),
    hid     = paste("spfa", un_hl_id),
    month   = month(date),
    week    = week(date)
  ) %>% 
  left_join(dplyr::select(h2, hid, shootlat, shootlon)) %>% 
  ungroup()

# janitor::compare_df_cols(l1, l2)
l <- 
  bind_rows(l1, l2) %>% 
  dplyr::select(
    vid, tid, hid, date, datetime, year, month, week, 
    shootlat, shootlon, rect,
    source, species,
    length, count
  )


# bio data - TO BE DONE
b1 <- 
  loadRData(file.path(dropboxdir, "PFA data","pfa_20210413_bio_extraction_for_pandora.RData"))
b2 <- 
  loadRData(file.path(dropboxdir, "SPFA data","spfa_ss_bio.RData"))



# Test: plot by haul
invisible(gc())
xrange <- range(h$shootlon, na.rm=TRUE)
yrange <- c(min(h$shootlat, na.rm=TRUE),64)
yrange <- c(50,62)

tt <-
  h %>% 
  filter(species %in% c("her","mac","whb")) %>% 
  filter(year %in% 2018:2020) %>% 
  filter(shootlon >= xrange[1] & shootlon <= xrange[2]) %>% 
  filter(shootlat >= yrange[1] & shootlat <= yrange[2]) %>% 
  group_by(year, species, source) %>% 
  summarise(n = n())

h %>% 
  filter(species %in% c("her","mac","whb")) %>% 
  filter(year %in% 2018:2020) %>% 
  filter(shootlon >= xrange[1] & shootlon <= xrange[2]) %>% 
  filter(shootlat >= yrange[1] & shootlat <= yrange[2]) %>% 
  
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
  geom_point(aes(size=catch, colour=source), alpha = 0.4, shape=20) +
  geom_text(data=filter(tt, source=="pfa"), 
            aes(label = paste("n=", n), colour=source),
            x=-Inf, y=Inf, hjust=0, vjust=1, size=3) +
  geom_text(data=filter(tt, source=="spfa"), 
            aes(label = paste("n=", n), colour=source),
            x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  scale_size(range = c(0.01,5)) +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = NULL, y = NULL, size = "t/haul", title="Catch by species") +
  # facet_wrap(~ month, ncol=6)
  facet_grid(year ~ species)



myspecies <- "her"

# rectangle/week combinations with both PFA and SPFA length observations
t <-
  l %>% 
  mutate(year_rect_week = paste(year, rect, week, sep="_")) %>% 
  filter(species == myspecies) %>% 
  group_by(source, species, year_rect_week) %>%
  summarise(count = sum(count, na.rm=TRUE)) %>% 
  group_by(species, year_rect_week) %>%
  mutate(nsources = n()) %>% 
  filter(nsources == 2) 

tt <-
  t %>% 
  group_by(source, species, year_rect_week) %>%
  summarise(n = as.integer(sum(count, na.rm=TRUE)))

# Example of LF by lat/long and year
l %>% 
  
  # filter(!is.na(shootlon), !is.na(shootlat)) %>% 
  mutate(year_rect_week = paste(year, rect, week, sep="_")) %>% 
  filter(year_rect_week %in% t$year_rect_week) %>% 
  
  left_join(dplyr::select(icesrectangles.df, rect, lat, long), 
            by="rect") %>% 
  filter(species == myspecies) %>% 
  filter(year >= 2018) %>% 
  
  # filter(year_rect_week == "2018_48E9_30") %>% 
  # View()
# mutate(rect   = encode_zchords(shootlon, shootlat,dx = 0.5,dy = 0.5)) %>%
  # separate(rect, c("shootlon", "shootlat"), sep = ":", convert = TRUE, remove = FALSE) %>% 
  
  mutate(length = floor(length)) %>% 
  
  # summarize by source, year_rect_week
  group_by(year_rect_week, rect, lat, long, source, species, length) %>% 
  summarise(count=sum(count, na.rm=TRUE)) %>% 
  group_by(year_rect_week, rect, lat, long, source, species) %>% 
  mutate(prop=count/sum(count, na.rm=TRUE)) %>% 
  
  ggplot(aes(length, prop)) + 
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
  geom_line(aes(colour=source), alpha = 0.4) +
  geom_text(data=filter(tt, source=="pfa"), 
            aes(label = paste("n=", n), colour=source),
            x=-Inf, y=Inf, hjust=0, vjust=1, size=3) +
  geom_text(data=filter(tt, source=="spfa"), 
            aes(label = paste("n=", n), colour=source),
            x=Inf, y=Inf, hjust=1, vjust=1, size=3) +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = NULL, y = NULL, size = "t/haul", title=myspecies) +
  # facet_wrap(~ month, ncol=6)
  facet_wrap(~ year_rect_week)


# Detailed look at specific combinations of species, year, rectange, week

# myspecies <- "her"; myyear_rect_week <- "2018_48E9_30"
myspecies <- "whb"; myyear_rect_week <- "2020_35D5_11"

t <-
  l %>% 
  mutate(year_rect_week = paste(year, rect, week, sep="_")) %>% 
  filter(species == myspecies) %>% 
  filter(year_rect_week == myyear_rect_week) %>%
  
  mutate(length = floor(length)) %>% 
  
  group_by(source, species, year_rect_week, hid, length) %>%
  summarise(count = sum(count, na.rm=TRUE)) %>% 
  
  group_by(source, species, year_rect_week, hid) %>%
  mutate(prop = count/sum(count, na.rm=TRUE)) 

tt <-
  t %>% 
  group_by(source, species, year_rect_week) %>% 
  summarise(nhaul = n_distinct(hid))

t %>% 
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
  geom_line(aes(colour=source), alpha = 0.4) +
  geom_text(data=filter(tt, source=="pfa"), 
            aes(label = paste("nhauls=", nhaul), colour=source),
            x=-Inf, y=Inf, hjust=0, vjust=1, size=3, inherit.aes = FALSE) +
  geom_text(data=filter(tt, source=="spfa"), 
            aes(label = paste("nhauls=", nhaul), colour=source),
            x=Inf, y=Inf, hjust=1, vjust=1, size=3, inherit.aes = FALSE) +
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = NULL, y = NULL, title=paste0(myspecies, myyear_rect_week)) +
  # facet_wrap(~ month, ncol=6)
  facet_wrap(~ source)
