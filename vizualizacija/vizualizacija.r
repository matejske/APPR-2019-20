#=========================================================================================================
#Graf ekodavkov Slovenije v letih 1998 - 2018=============================================================
davki.slovenije <- eko.davki %>% filter(Drzava == "Slovenia")
graf.davki.slo <- ggplot(davki.slovenije, aes(x = Leto, y = Pobrani.davki)) + 
  geom_point(colour = "red") + 
  geom_line() + 
  theme_minimal() +
  ggtitle('Prihodki Slovenije s strani ekoloških davkov \n(1998-2018)') + 
  xlab("Leto") + 
  ylab("Pobrani davki v milijonih €") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
plot(graf.davki.slo)


#Uvozimo zemljevid sveta==================================================================================
source("lib/uvozi.zemljevid.r")
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


#Zemljevid evropskih drzav v letu 2016 (obarvane glede na velikost BDP)====================================
zemljevid.bdp.2016 <- ggplot() + 
  geom_polygon(data = bdp %>% 
                 filter(Leto == 2016) %>% 
                 transform(BDP.E = bdp$BDP.E/1000) %>%
                 right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, group=group, fill=BDP.E)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  labs(title = "Zemljevid evropskih držav", 
       subtitle = "BDP v letu 2016") +
  guides(fill=guide_colorbar("BDP v \nmilijardah €")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right")

plot(zemljevid.bdp.2016)

#transform(bdp.pc, bdp.pc.stolpec = round(((bdp.pc$BDP.E/bdp.pc$Stevilo.prebivalcev)*
#1000000), digits = 2))

#Zemljevid INDEKSA eko izdatkov/bdp=========================================================================
zemljevid.izdatki.v.bdp.2016 <- ggplot() + 
  geom_polygon(data = ekoizdatki.v.bdp %>% 
                 filter(Leto == 2016) %>% 
                 right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, group=group, fill=ekoizdatki.v.bdp.stolpec)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  labs(title = "Zemljevid indeksa izdatki za ekologijo glede na BDP",
       subtitle = "(evropske države v letu 2016)") +
  guides(fill=guide_colorbar("Vrednost \nindeksa")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right") 

plot(zemljevid.izdatki.v.bdp.2016)


#Cluster==================================================================================================
##Podobnosti med državami glede na letni BDP in izpuščene emisije
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
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
  guides(fill=guide_legend(title='Skupine')) + 
  ggtitle('Podobnosti med državami glede na letni BDP in izpuščene emisije') + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
print(zemljevid_cluster)

#Plotly=========================================================================================================
##Plotly: Pobrani davki in izmerjene vrednosti emisij
plotly.tabela <- inner_join(eko.davki, skupno.plini, by = c('Drzava','Leto'))
plotly.tabela <- plotly.tabela %>% 
  filter(Leto >= "2008") %>%
  transform(skupne.emisije = plotly.tabela$skupne.emisije / 1000000) %>%
  transform(Pobrani.davki = plotly.tabela$Pobrani.davki / 1000)
plotly.graf2 <- ggplot(data = plotly.tabela, aes(x=Pobrani.davki, y=skupne.emisije, color=Drzava)) +
  geom_point(aes(frame=Leto, ids=Drzava)) + 
  scale_x_continuous() +
  xlab("Pobrani davki v milijardah €") + 
  ylab("Vrednost vseh emisij v megatonah")
  
  ggtitle('Podobnosti med državami glede na letni BDP in izpuščene emisije')
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))
plotly.graf2 <- ggplotly(plotly.graf2, dynamicTics=TRUE)
print(plotly.graf2)


#FUNKCIJA ZA SHINY==============================================================================================
zemljevid.leto <- function(cifra) {
  
  ggplot() + geom_polygon(data = skupno.plini %>% 
                            filter(Leto == cifra) %>% 
                            transform(skupne.emisije = skupno.plini$skupne.emisije / 1000000) %>%
                            right_join(europe, by=c("Drzava"="NAME")),
                          aes(x = long, y = lat, group = group, fill=skupne.emisije)) + 
    xlab("") + ylab("")  + 
    guides(fill=guide_colorbar("Vrednost emisij v megatonah")) +
    theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
    scale_fill_gradient(low = '#ffb3b3', high='#660000')
  
}
