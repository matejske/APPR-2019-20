source("lib/libraries.r")
#source('uvoz/uvoz.r')
#===================
#Graf ekoizdatkov Slovenije v letih 1998 - 2018==============================================
davki.slovenije <- eko.davki %>% filter(Drzava == "Slovenia")

graf.davki.slo <- ggplot(davki.slovenije, aes(x = Leto, y = Pobrani.davki)) + 
  geom_point() + geom_line()





#Zemljevid INDEKSA eko izdatkov/bdp 



#Zemljevid INDEKSA eko izdatkov/bdp 







# Uvozimo zemljevid Sveta=======================================================================
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
colnames(europe)[26] <- 'Drzava'
# Drzave v zemljevidu Evrope
drzave <- sort(unique(europe$NAME)) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

test <-ggplot() + geom_polygon(data = europe, aes(x=long, y=lat, group=group))

#DODAJ JANOS

#FUNKCIJA ZA SHINY

#plotly==================================================================
plotly.tabela <- inner_join(eko.davki,eko.potrosnja,by = c('Drzava','Leto')) 
plotly.tabela <- plotly.tabela %>% filter(Izdatki.za.ekologijio != '')%>% filter(Leto > 2009)
plotly.graf <- ggplot(data = plotly.tabela, aes(x=Izdatki.za.ekologijio, y=Pobrani.davki, color=Drzava)) + 
  geom_point(aes(frame=Leto, ids=Drzava)) + scale_x_continuous()
plotly.graf <- ggplotly(plotly.graf, dynamicTics=TRUE)




