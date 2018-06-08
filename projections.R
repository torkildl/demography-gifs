library(gapminder)
library(data.table)
library(tidyverse)
library(gganimate)

MMMM <- fread("http://data.ssb.no/api/v0/dataset/131613.csv?lang=en")

# Clean and tidy the data a bit, and remove superfluous information
immdata <- MMMM %>%
    setNames(nm = c("sex","age","immigrant","variants","year","type","population")) %>%
    select(-type, -variants) %>%   ## Just one variant in this table. All data population counts
    mutate(age = as.numeric(str_sub(age,start=1, end=3))) %>%
    mutate(sex = ifelse(sex=="1 Males", "Male","Female")) %>%
    filter(immigrant!="00 Total population") %>%
    mutate(populationminus = population*-1) %>%
    mutate(immcat = case_when(str_sub(immigrant,1,2)=="01" ~ "1G Western",
                              str_sub(immigrant,1,2)=="02" ~ "2G Western",
                              str_sub(immigrant,1,2)=="03" ~ "1G East EU",
                              str_sub(immigrant,1,2)=="04" ~ "2G East EU",
                              str_sub(immigrant,1,2)=="05" ~ "1G Nonwest",
                              str_sub(immigrant,1,2)=="06" ~ "2G Nonwest",
                              str_sub(immigrant,1,2)=="99" ~ "Natives+++"))
head(immdata)


pyramid <- ggplot(mapping = aes(x=age,fill=immcat, frame=immcat)) + 
    geom_bar(data=filter(immdata,sex=="Male"), mapping=aes(y=population),stat="identity") + 
    geom_bar(data=filter(immdata,sex=="Female"), mapping=aes(y=populationminus),stat="identity") + 
    scale_fill_brewer(palette = "Accent") +
    coord_flip() +
    theme(legend.position="bottom", legend.text = element_text(size=8))

gganimate(pyramid, convert = "C:/Progra~1/ImageMagick-7.0.7-Q16/convert.exe convert")

    




p <- ggplot(gapminder, aes(x=gdpPercap,y=lifeExp, size=pop, color=continent, frame=year)) + geom_point() + theme(legend.position="none")
gganimate(p, title_frame = T, convert="magick.exe")



nine_alternatives <- fread("http://data.ssb.no/api/v0/dataset/85456.csv?lang=no")
