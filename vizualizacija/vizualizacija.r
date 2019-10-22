source("lib/libraries.r")
#source('uvoz/uvoz.r')
#============================================================================================
#Graf ekoizdatkov Slovenije v letih 1998 - 2018==============================================
davki.slovenije <- eko.davki %>% filter(Drzava == "Slovenia")

graf.davki.slo <- ggplot(davki.slovenije, aes(x = Leto, y = Pobrani.davki)) + 
  geom_point() + geom_line()
plot(graf.davki.slo)


# Uvozimo zemljevid sveta=======================================================================
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

#svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", 
#                        "ne_50m_admin_0_countries", encoding = "utf-8") %>% fortify()

#zace <- ggplot(skupno.plini, aes(x=Leto, y=skupne.emisije, col=Drzava)) + geom_point() + geom_line()


#Zemljevid sveta skrčimo na zemljevid Evrope
europe <- svet %>% filter(CONTINENT == "Europe") %>% 
  filter(long < 55 & long > -25 & lat > 35 & lat < 72) %>%
  filter(NAME != "Jersey") %>%
  filter(NAME != "Russia")

colnames(europe)[26] <- 'Drzava'

#Drzave v zemljevidu "europe"
drzave <- sort(unique(europe$NAME)) 
drzave <- as.data.frame(drzave, stringsAsFactors=FALSE) 
names(drzave) <- "Drzava"

#zemljevid.evropa <-ggplot() + geom_polygon(data = europe, aes(x=long, y=lat, group=group))



#DODAJ info iz issuea
zemljevid.evropa <- ggplot() + geom_polygon(data=bdp %>% filter(Leto == 2017) %>% 
                          right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, 
                                                                         group=group, fill=BDP.E))


plot(zemljevid.evropa)


#Zemljevid INDEKSA eko izdatkov/bdp 



#Zemljevid INDEKSA eko izdatkov/bdp 



#FUNKCIJA ZA SHINY=================================================================================
zemljevid.leto <- function(cifra) {
  
  ggplot() + geom_polygon(data = bdp %>% filter(Leto == cifra) %>% 
                            right_join(europe, by=c("Drzava"="NAME")),
                          aes(x = long, y = lat, group = group, fill= Odstotki))+
    xlab("") + ylab("")  + theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
    scale_fill_gradient(low = '#ffb3b3', high='#660000')
  
}


#Plotly=============================================================================================
#Plotly: Pobrani davki in izmerjene vrednosti emisij
plotly.tabela2 <- inner_join(eko.davki, skupno.plini, by = c('Drzava','Leto')) %>% 
  filter(Leto >= "2008")
plotly.graf2 <- ggplot(data = plotly.tabela2, aes(x=Pobrani.davki, y=skupne.emisije, color=Drzava)) + 
  geom_point(aes(frame=Leto, ids=Drzava)) + scale_x_continuous()
plotly.graf2 <- ggplotly(plotly.graf2, dynamicTics=TRUE)
print(plotly.graf2)

