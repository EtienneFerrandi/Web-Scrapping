# Web-Scrapping

This script aims at creating a database containing main informations concerning the past expositions of the musée de Lyon (https://www.mac-lyon.com/fr/agenda?pastExpo=true).

The informations of the data base have been previously got with a loop scrapping.

With the `gender` package, we classify each artist concerned with the past expositions in two categories, man and woman. 

```ruby
#### RECODAGE pour base propre ####
library(questionr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(tidyverse)

bdd <- read_delim("base_url_raw_verifie_utf8.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)

bdd <- bdd %>% mutate_if(is.character, funs(str_replace_all(.,"_", "é")))
bdd <-bdd %>% mutate_if(is.character, funs(str_replace_all(.,"!", "è")))

###Manipuler des dates ####
bdd_date <- separate(bdd, dates, sep="//", into=c("date_debut", "date_fin"))

#changer en format date
library(lubridate)

bdd_date$date_fin2 <- dmy(bdd_date$date_fin)
bdd_date$date_debut2 <- dmy(bdd_date$date_debut)

#Création variable de durée : 
bdd_date$duree <- bdd_date$date_fin2 - bdd_date$date_debut2
bdd_date$duree <- as.numeric(bdd_date$duree)

hist(bdd_date$duree, breaks = 50)

###Pour les artistes ####
#création bdd artiste_rec avec seulement les prénoms : 
bdd_date$artistes_rec <- forcats::fct_recode(bdd_date$artistes,
                                    "seul" = "Delphine Balley",
                                    "seul" = "Charlie Adlard",
                                    "seul" = "Maxwell Alexandre",
                                    "collectif"="Alain Séchas, Philippe Droguet, Krzysztof Wodiezko, Jean-Franeois Gavoty, Eduardo Paolozzi, Daniel Spoerri, Christian Boltanski, Aernout Mik",
                                    "collectif"="Laurie Anderson, George Brecht, Philip Corner, Molly Davies, Morton Feldman, Anna Halprin, Joe Jones, Allan Kaprow, Alvin Lucier, George Maciunas, Peter Moore, Nam June Paik, Terry Riley, David Tudor, Stephen Vitiello, La Monte Young et Marian Zazeela",
                                    "collectif"="Ophélie Demurger, Jean Doroszczuk, Adriane Emerit, Hugo Ferretto, Perrine Forest, Quentin Goujout, Juliette Guerin, Hélene Hulak, Michala Julenyové, Maintenance Tank x Nicolas Degrange, Pierre Masclef, Jean-Baptiste Perret, Sinem Sahin, Frédérique Vivet",
                                    "collectif"="Sara Bichéo, Chourouk Hriech, Celsian Langlois, Violaine Lochu, Lou Masduraud & Antoine Bellini, Hannelore Van Dijck",
                                    "seul" = "Tal Isaac Hadad",
                                    "seul"="Bernar Venet",
                                    "collectif"="Celia-Yunior, Susana Pilar, Jenny Feal, Amélie Giacomini et Laura Sellies, Igor Keltchewsky alias Abraham Murder, Anne Le Troter, Duniesky Marten, Laure Mary-Couégnias, Nathalie Muchamad, eléonore Pano-Zavaroni, Marion Robin, Ludvig Sahakyan, Thomas Teurlai, Victor Yudaev",
                                    "collectif"="Massinissa Selmani, Marius Dansou",
                                    "collectif"="Doug Aitken, Lara Almércegui, Laurie Anderson, Hans Arp, Renaud Auguste-Dormeuil, Davide Balula, Robert Barry, Laurent P. & Cyrille Berger, Dominique Blais, Céleste Boursier-Mougenot, Thierry Boutonnier, George Brecht, Robert Breer, Mathieu Briand, Marcel Broodthaers, Richard Buckminster Fuller, Alberto Burri, Alexander Calder, ChiméPom, Elizabeth S. Clark, Bruce Conner, Philip Corner, Julien Creuzet, Dadamaino, Julien Discrit, Lucio Fontana, Lars Fredrikson, Susanna Fritscher, Jochen Gerz, Marco Godinho, Brion Gysin, Hans Haacke, Anawana Haloba, Hao Jingfang & Wang Lingjie, Lee Mingwei, Ola Maciejewska, Heinz Mack, Jill Magid, Anna Maria Maiolino, Jén Manéuéka, Gordon Matta-Clark, David Medalla, Cildo Meireles, Ari Benjamin Meyers, Yuko Mohri, Peter Moore, Ernesto Neto, Rivane Neuenschwander, Camille Norment, Melik Ohanian, Damién Ortega, Fernando Ortega, Nam June Paik, Christodoulos Panayiotou, Lygia Pape, Ewa Partum, Pratchaya Phinthong, Otto Piene, Philippe Quesne, Terry Riley, Lotty Rosenfeld, Tomés Saraceno, Paolo Scheggi, Shimabuku, Mieko Shiomi, Daniel Stegmann Mangrané, Diana Thater, David Tudor, Dareo Villalba, Jorinde Voigt, Apichatpong Weerasethakul, Cerith Wyn Evans, La Monte Young, Héctor Zamora, Icaro Zorbar",
                                    "collectif"="Apichatpong Weerasethakul, Ryan Gander, Nathalie Djurberg, Guillaume Leblon, Julien Prévieux",                                   
                                    "seul"="Ben",
                                    "seul"="Lola Gonzalez",
                                    "collectif"="Carlos Puga, Patricia Martins, emilie Girard-Charest",
                                    "collectif"="Philippe Droguet, William Kentridge, Alain Pouillet, Henri Ughetto et Carmelo Zagari",                                   
                                    "collectif"="Gaelle Choisne, Ruth Cornelisse, Fabrice Croux, Adélaéde Feriot, Nicolas Garait-Leavenworth, Lola Gonzélez, Huang Yang, Maxime Lamarche, Leng Wen, Li Linlin, Lu Zhengyuan, Daniel Otero Torres, David Posth-Kohler, Qiu Yu, Johann Rivat, Wu Hong",
                                    "seul"="Jesés Rafael Soto",
                                    "collectif"="John Baldessari, Larry Bell, David Hockney, Ryan Trecartin,Lizzie Fitch, Alex Israel",
                                    "collectif"="ORLAN, Régine Chopinot, Benni Efrat, Mike Hentz, Dieter Appelt, Fabrizio Plessi, Marie-Christine Vernay",
                                    "seul"="Olivier Zabat",
                                    "seul"="Jan Fabre",
                                    "collectif"="Eduardo Basualdo, Cai Guo-Qiang, Ilya Kabakov, ORLAN, Jean-Luc Parant, Mel Ramos, Tavares Strachan",
                                    "collectif"="Lewis Trondheim, Franeois Ayroles, Jochen Gerner, Patrice Killoffer",
                                    "seul"="Yoko Ono",
                                    "seul"="Paulo Nimer Pjota",
                                    "collectif"="Wong Hoy Cheong, Eko Nugroho",
                                    "seul"="Krzysztof Wodiczko",
                                    "seul"="Jean-Luc Parant",
                                    "collectif"="13e Biennale de Lyon 2015",
                                    "seul"="Sunday Jack Akpan",
                                    "collectif"="Dan Graham, Yona Friedman, Stephen Vitiello, Hiroshi Sugimoto, Robert Filliou, Claudio Parmiggiani, Eko Nugroho, Bik Van Der Pol, Pied la biche",
                                    "collectif"="Ken Thaiday, Black Travelly, Zhuang Hui",
                                    "collectif"="Ange Leccia, Richard Baquié",
                                    "collectif"="Zbynek Baladran, Marina de Caro, Lucia Koch, Tracey Rose, Alexander Schellow",
                                    "collectif"="Lina Adam, Anida Yoeu ALI, Apotik Komik, BOO Junfeng, CHANG Yoong Chia, Chris CHONG Chan Fui , CHONG Kim Chiew , Louie CORDERO , Marisa DARASAVATH , Heri DONO",
                                    "seul"="Antoine Catala",
                                    "seul"= "Juliacks",
                                    "seul"="Erro")
## Recodage de bdd_date$artistes_rec
bdd_date$artistes_rec <- as.character(bdd_date$artistes_rec)
bdd_date$artistes_rec[bdd_date$artistes_rec == "Charley Case, Franco Fasoli , Jaz, Kid Kréol & Boogie, Addam Yekutieli aka Know Hope, Reko Rennie, Saner, Teck, Elliot Tupac, Wenna"] <- "collectif"
bdd_date$artistes_rec[bdd_date$artistes_rec == "Lina Adam, Mathilde Barrio Nuevo , Sophie Bonnet-Pourpet , Thibault Brunet , Jean-Alain Corre , Hasan & Husain Essop , Dan Finsel , André Fortino , Nikita Kadan , Chun Kaifeng , Karim Kal , Paula Krau"] <- "collectif"
bdd_date$artistes_rec[is.na(bdd_date$artistes_rec)] <- "collectif"

bdd_date$artistes_entier <- bdd_date$artistes
bdd_arti <- bdd_date  %>% 
  separate(artistes_entier, sep=" ", into=c("artistes_prenom", "null"))
bdd_arti$null <- NULL
bdd_arti$artistes_prenom [bdd_arti$artistes_rec == "collectif"] <- NA

#puis on attribue le genre en fonction du pr?nom
library(gender)
prenom <- gender(bdd_arti$artistes_prenom, method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"), countries = c("United States", "Canada", "United Kingdom", "France", "Italia"), years = c(1950, 2012))

prenom$name
prenom$gender

bdd_arti2 <- left_join(bdd_arti, select(prenom, name, gender), by= c("artistes_prenom"= "name"))

## Recodage de base_arti2$gender en base_arti2$artistes_genre
bdd_arti2$artistes_genre <- bdd_arti2$gender
bdd_arti2$artistes_genre[base_arti2$gender == "female"] <- "Femme"
bdd_arti2$artistes_genre[base_arti2$gender == "male"] <- "Homme"

bdd_arti2$gender <- NULL


###Pour les commissaires ####
#commissaire : pareil que artiste
bdd_comm <- bdd_arti2
#Recodage de bdd_date2$commissaires en bdd_date2$commissaires_rec
bdd_comm$commissaires_rec <- fct_recode(bdd_comm$commissaires,
                                         "seul" = "Agnès Violeau",
                                         "collectif" = "Isabelle Bertolotti, Nathalie Ergino, Melanie Pocock, Thierry Raspail, Bala Starr, Emmanuel Tibloux",
                                         "collectif" = "Julien Malland, Hervé Perdriolle",
                                         "seul" = "Ralph Rugoff",
                                         "seul" = "Thierry Raspail",
                                         "collectif" = "Thierry Raspail, Danielle Kvaran",
                                         "collectif" = "Thierry Raspail, Gunnar B. Kvaran, Nicolas Garait-Leavenworth")


bdd_comm$commissaires_entier <- bdd_comm$commissaires
bdd_comm <- separate(bdd_comm, commissaires_entier, sep=" ", into=c("commissaires_prenom", "null"))

bdd_comm$null <- NULL
bdd_comm$commissaires_prenom [bdd_comm$commissaires_rec == "collectif"] <- NA

prenom <- gender(bdd_comm$commissaires_prenom, method = c("ssa", "ipums", "napp", "kantrowitz", "genderize", "demo"), countries = c("United States", "Canada", "United Kingdom", "France", "Italia"), years = c(1950, 2012))
prenom$name
prenom$gender

table(essai$texte)
bdd_comm2 <- left_join(bdd_comm, select(prenom2, name, gender), by= c("commissaires_prenom"= "name"))

bdd_comm2$commissaires_genre <- bdd_comm2$gender
bdd_comm2$commissaires_genre[bdd_comm2$commissaires_prenom == "Agnès"] <- "Femme"
bdd_comm2$commissaires_genre[bdd_comm2$gender == "male"] <- "Homme"

bdd_comm2$gender <- NULL

###Ajout variable nom_musee et type_musee ####
base_lyon <- bdd_comm2

base_lyon$nom_musee <- "mac_lyon"
base_lyon$type_musee <- "public"
base_lyon$ID <- NULL

base_lyon <- base_lyon %>% select(nom_musee, type_musee,titre, texte, date_debut2, date_fin2, duree, artistes, artistes_rec, artistes_genre, commissaires, commissaires_rec, commissaires_genre)

library(xlsx)

write.xlsx(base_lyon, file ="base_maclyon.xlsx")
```
