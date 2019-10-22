source("lib/libraries.r")
source('uvoz/uvoz.r')
#============================================================================================
#Graf ekoizdatkov Slovenije v letih 1998 - 2018==============================================
davki.slovenije <- eko.davki %>% filter(Drzava == "Slovenia")
graf.davki.slo <- ggplot(davki.slovenije, aes(x = Leto, y = Pobrani.davki)) + 
  geom_point() + geom_line()
plot(graf.davki.slo)


#Uvozimo zemljevid sveta=====================================================================
source("lib/uvozi.zemljevid.r") #Nastavi pravo datoteko

svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", 
                        "ne_50m_admin_0_countries", encoding = "utf-8") %>% fortify()


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

#Zemljevid evropskih drzav v letu 2017 (obarvane glede na velikost BDP)
zemljevid.bdp.2017 <- ggplot() + 
  geom_polygon(data = bdp %>% 
                 filter(Leto == 2017) %>% 
                 right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, group=group, fill=BDP.E)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank(), legend.position = 'none') + 
  labs(title = "Zemljevid evropskih držav", 
       subtitle = "BDP v letu 2017") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

plot(zemljevid.bdp.2017)

#Zemljevid INDEKSA eko izdatkov/bdp============================================================
zemljevid.izdatki.v.bdp.2018 <- ggplot() + 
  geom_polygon(data = ekoizdatki.v.bdp %>% 
                 filter(Leto == 2018) %>% 
                 right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, group=group, fill=ekoizdatki.v.bdp.stolpec)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank(), legend.position = 'none') + 
  labs(title = "Zemljevid indeksa izdatki za ekologijo glede na BDP",
       subtitle = "(evropske države v letu 2018)") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

plot(zemljevid.izdatki.v.bdp.2018)


#Cluster=====================================================================================
podobnosti <- dcast(bdp, Drzava~Leto, value.var = 'BDP.E')
priprava.plini <- skupno.plini %>% 
  filter(Leto > 2007) %>% 
  dcast(Drzava ~ Leto, value.var = 'skupne.emisije')
podobnosti <- right_join(podobnosti, priprava.plini, by=c('Drzava'))

podobnosti.a <- podobnosti[,-1]
fit <- hclust(dist(scale(podobnosti.a)))
skupine2 <- cutree(fit, 7)

cluster2 <- mutate(podobnosti, skupine2)

zemljevid_cluster <- ggplot() + 
  geom_polygon(data = right_join(cluster2[c(-2:-31)], europe, by=c('Drzava')), aes(x=long, y=lat, group = group, fill=factor(skupine2))) + 
  geom_line() +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), legend.position = 'none') + 
  # guides(fill=guide_colorbar(title='Skupine')) + 
  ggtitle('nekaj')

#FUNKCIJA ZA SHINY=================================================================================
zemljevid.leto <- function(cifra) {
  
  ggplot() + geom_polygon(data = bdp %>% filter(Leto == cifra) %>% 
                            right_join(europe, by=c("Drzava"="NAME")),
                          aes(x = long, y = lat, group = group, fill=BDP.E))+
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

