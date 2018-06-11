library(data.table)
library(tidyverse)
library(gganimate)
library(here)

MMMM <- fread("http://data.ssb.no/api/v0/dataset/131613.csv?lang=no")

# Clean and tidy the data a bit, and remove superfluous information
immdata <- MMMM %>%
    setNames(nm = c("sex","age","immigrant","variants","year","type","population")) %>%
    select(-type, -variants) %>%   ## Just one variant in this table. All data population counts
    mutate(age = as.numeric(str_sub(age,start=1, end=3))) %>%
    mutate(sex = ifelse(str_sub(sex,1,3)=="1 M", "Male","Female")) %>%
    filter(str_sub(immigrant,1,2)!="00") %>%
    mutate(immcat = case_when(str_sub(immigrant,1,2)=="01" ~ "1G Western",
                              str_sub(immigrant,1,2)=="02" ~ "2G Western",
                              str_sub(immigrant,1,2)=="03" ~ "1G East EU",
                              str_sub(immigrant,1,2)=="04" ~ "2G East EU",
                              str_sub(immigrant,1,2)=="05" ~ "1G Nonwest",
                              str_sub(immigrant,1,2)=="06" ~ "2G Nonwest",
                              str_sub(immigrant,1,2)=="99" ~ "Natives+++")) %>%
    select(year, sex ,age, immigrant, everything()) %>%
    arrange(year,sex,age,immigrant) %>%
    group_by(year,sex,age) %>%
    #    arrange(year,sex,age,immigrant) %>%
    mutate(slutt = cumsum(population)) %>%
    mutate(start = slutt-population)
head(immdata,20)

pyears <- seq(2016,2100,5)
pyramid <- ggplot(mapping = aes(x=age,fill=immcat,frame=year)) + 
    geom_ribbon(data=filter(immdata,sex=="Male", year %in% pyears), mapping=aes(ymin=start, ymax=slutt)) + 
    geom_ribbon(data=filter(immdata,sex=="Female", year %in% pyears), mapping=aes(ymin=-1*start, ymax=-1*slutt)) + 
    scale_fill_brewer(palette = "Pastel1") +
    scale_x_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100),expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0),limits=c(-53000,53000), breaks = seq(-50000, 50000, 10000), 
                       labels = paste0(as.character(c(5:0, 1:5)), "k")) +
    coord_flip() +
    labs(title = "Norway's population by national background", x = "", y = "", fill="") + 
    theme(legend.position="right", legend.title = element_blank(), legend.text = element_text(size=10))
gganimate(pyramid, title_frame = T, filename = here("./projections.gif"), ani.width=800, ani.height=800, extra.opts="-delay 50")


# A pyramid for the period 1846-2016.     
historical <- fread("http://data.ssb.no/api/v0/dataset/59322.csv?lang=no")

histdata <- historical %>%
    setNames(nm = c("age","sex","year","unit","population")) %>%
    mutate(age = as.numeric(str_sub(age,start=1, end=3))) %>%
    mutate(sex = as.factor(as.numeric(str_sub(sex,1,1)))) %>%
    select(-unit) %>%
    arrange(year)

pyears <- sort(unique(histdata$year))
histpyra <- ggplot(histdata, aes(x=age, y=population, fill=sex, frame=year)) + 
    geom_ribbon(data=filter(histdata,sex==1, year %in% pyears), mapping=aes(ymin=0, ymax=population)) + 
    geom_ribbon(data=filter(histdata,sex==2, year %in% pyears), mapping=aes(ymin=-1*population, ymax=0)) + 
    scale_fill_brewer(palette = "Pastel1") +
    scale_x_continuous(limits=c(0,100),breaks=c(0,10,20,30,40,50,60,70,80,90,100),expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0),limits=c(-40000,40000), breaks = seq(-40000, 40000, 10000), 
                       labels = paste0(as.character(c(4:0, 1:4)), "k")) +
    coord_flip() +
    labs(title = "Norway's population in", x = "", y = "", fill="") + 
    theme(legend.position="none")
histpyra

histpyra_anim <- gganimate(histpyra, title_frame = T)
gganimate_save(histpyra_anim, filename=here("./historical-pyramid.gif"),loop = 0, fps = 4)
