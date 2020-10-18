#Test 2

#Creating a shiny app


library(rvest)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(textdata)
library(gridExtra)
library(shiny)
library(leaflet)
library(readxl)
library(ggimage)
library(rtweet)

library(shiny)
library(leaflet)
library(tidyverse)

library(geojsonio)
library(stringr)

pokemon.data <- read_csv(file.choose())
view(pokemon.data)

##Bulbosar test
bulbosar.url <- "https://bulbapedia.bulbagarden.net/wiki/Bulbasaur_(Pok%C3%A9mon)"

bulbosar.desc <- poke.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

bulbasar.img <- bulbosar.url %>%
  read_html() %>%
  html_nodes("img") %>%
  .[[3]] %>%
  html_attr('src')

view(bulbasar.img)

bulbasar.img


bulbosar.data <- data.frame(bulbosar.desc)

bulbosar.data.copy <- bulbosar.data

bulbosar.data.copy %>%
  head(1)

##getting image
Charizard.url <- "https://bulbapedia.bulbagarden.net/wiki/Charizard"
Charizard.img <- Charizard.url %>%
  read_html() %>%
  html_nodes('img') %>%
  view()

#https://assets.pokemon.com/assets/cms2/img/pokedex/full/006.png
#https://assets.pokemon.com/assets/cms2/img/pokedex/detail/001.png

#<img alt="Charizard" src="//cdn.bulbagarden.net/upload/thumb/7/7e/006Charizard.png/250px-006Charizard.png" width="250" height="250" srcset="//cdn.bulbagarden.net/upload/thumb/7/7e/006Charizard.png/375px-006Charizard.png 1.5x, //cdn.bulbagarden.net/upload/thumb/7/7e/006Charizard.png/500px-006Charizard.png 2x">

#How to get url for pokemon
poke.url <- paste0('https://en.wikipedia.org/wiki/',
                   'bulbosaur')

bulbosar.img.frame <- data.frame(bulbosar.img)

bulbosar.img %>%
  ggplot() +
  geom_image()


##Charizard
Charizard.url <- "https://bulbapedia.bulbagarden.net/wiki/Charizard"

Charizard.desc <- Charizard.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

Charizard.data <- data.frame(Charizard.desc)

Charizard.data.copy <- Charizard.data

desc <- Charizard.data.copy %>%
  head(1) %>%
  view()

Charizard.data.copy[1,1]

desc$Charizard.desc

pokemon.data %>%
  view()


##2
#HP, Attack, Defense, Sp. Atk, Sp. Def, and Speed

pokemon.data.copy <- pokemon.data

#repeat for each attribute
#create a bar graph of percentages, color tallest bar
cutoff <- pokemon.data.copy %>%
  filter(Name == "Bulbasaur") %>%
  select(HP)

numb.greater <- pokemon.data.copy %>%
  filter(HP >= cutoff$HP) %>%
  count('#')

floor((100 - (numb.greater$n / 800) * 100))


##3
random.dist <- floor(runif(800,
                     1,
                     51)) %>%
  view()

view(random.data)

states.data <- read_csv(file.choose())
view(states.data) 

states.data.copy <- states.data


pokemon.data.numbered <- pokemon.data.copy %>%
  mutate(state.num = random.dist[pokemon.data.copy$`#`])

pokemon.data.states <- left_join(pokemon.data.numbered,
                                 states.data.copy,
                                 by = c('state.num' = 'Num'))

color.map <- pokemon.data.states %>%
  group_by(State) %>%
  count()

states <- geojson_read(file.choose(),
                       what = "sp")

states %>%
  view()

states@data <- left_join(states@data,
                         color.map,
                         by = c("NAME" = "State"))

view(states@data)

bins1 <- c(0,
           5,
           10,
           15,
           20,
           25,
           30,
           Inf)
colors <- colorBin(bins = bins1,
                   palette = "Blues",
                   domain = states@data$n)
view(colors)

view(states@data)

states@data$is.selected.state

states %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(fillColor = ~colors(n),
              weight = 3,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = 1,
              label = ~n) %>%
  addPolygons(fillColor = "red",
              fill = ~is.selected.state,
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = .1,
              fillOpacity = .5,
              label = ~n) %>%
  setView(-96, 37.8, 3)

highlight.state <- pokemon.data.states %>%
  filter(Name == 'Bulbasaur') %>%
  select(State)

view(states)

states %>%
  leaflet() %>%
  addTiles %>%
  addPolygons(fillColor = ifelse(states@data$NAME == highlight.state$State,
                                 "red",
                                 ~colors(n)),
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = .7,
              label = ~n) %>%
  setView(-96, 37.8, 3)


states@data <- states@data %>%
  mutate(is.selected.state = ifelse(states@data$NAME == highlight.state$State,
                                    TRUE,
                                    FALSE))

##4

pokemon.state.list <- pokemon.data.states %>%
  filter(State == "Maryland") %>%
  select(Name)

count(pokemon.state.list)

pokemon.state.list[floor(runif(1,
                               1,
                               count(pokemon.state.list)$n)),]$Name

 









