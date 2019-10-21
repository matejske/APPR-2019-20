source("lib/libraries.r")
#source('uvoz/uvoz.r')
#===================
# Uvozimo zemljevid Sveta
# source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

#svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                        #"ne_50m_admin_0_countries", encoding = "utf-8") %>% fortify()

zace <- ggplot(skupno.plini, aes(x=Leto, y=skupne.emisije, col=Drzava)) + geom_point() + geom_line()


# Zemljevid sveta skrčimo na zemljevid Evrope
europe <- filter(svet, CONTINENT == "Europe")
europe <- filter(europe, long < 55 & long > -35 & lat > 30 & lat < 75)

europe <- filter(europe, NAME != "Jersey")
europe <- filter(europe, NAME != "Russia")

# Drzave v zemljevidu Evrope
drzave <- sort(unique(europe$NAME)) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

test <-ggplot() + geom_polygon(data = europe, aes(x=long, y=lat, group=group))

#Zemljevidi po izpušnih glede na leta
