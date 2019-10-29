#=========================================================================================================
#Graf skupnih emisij v Sloveniji v letih 2008-2017
emisije.slo <- skupno.plini %>% 
  filter(Drzava == "Slovenia" & Leto >= 2008) %>%
  transform(skupne.emisije = round(skupne.emisije / 1000000, 4))

graf.emisije.slo <- ggplot(emisije.slo, aes(x = Leto, y = skupne.emisije)) + 
  geom_line(colour = "royalblue", size = 1.5) +
  geom_point(colour = "#000000", size = 2.5) + 
  theme_minimal() +
  ggtitle('Vrednosti letnih emisij Slovenije \n(seštevek panog v letih 1998-2017)') + 
  ylab("Vrednosti emisij v megatonah") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) + 
  scale_x_continuous("Leto", labels = as.character(emisije.slo$Leto), breaks = emisije.slo$Leto)
# plot(graf.emisije.slo)


#Graf ekodavkov Slovenije v letih 2008-2017=============================================================
davki.slovenije <- eko.davki %>% filter(Drzava == "Slovenia" & Leto >= 2008)

graf.davki.slo <- ggplot(davki.slovenije, aes(x = Leto, y = Pobrani.davki)) + 
  geom_line(colour = "royalblue", size = 1.5) +
  geom_point(colour = "#000000", size = 2.5) + 
  theme_minimal() +
  ggtitle('Prihodki Slovenije s strani ekoloških davkov \n(2008-2017)') + 
  ylab("Pobrani davki v milijonih €") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) + 
  scale_x_continuous("Leto", labels = as.character(davki.slovenije$Leto), breaks = davki.slovenije$Leto)
# plot(graf.davki.slo)


# Graf emisij držav glede na BDP v letu 2017 ==============================================================
emisije.v.bdp.2017 <- emisije.v.bdp %>% filter(Leto == 2017 & is.na(emisije.v.bdp.stolpec) == FALSE)

graf.emisije.v.bdp.2017 <- ggplot(emisije.v.bdp.2017, 
                                  aes(x = reorder(Drzava, emisije.v.bdp.stolpec),
                                      y = emisije.v.bdp.stolpec,
                                      fill=factor(ifelse(emisije.v.bdp.2017$Drzava=="Slovenia","T","F")))) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_fill_manual(name = "emisije.v.bdp.2017$Drzava", values=c("royalblue","red")) +
  xlab("Države") + 
  ylab("Skupne emisije / BDP\n(tone / mio €)") + 
  ggtitle("Indeks emisij glede na BDP \n(države EU v letu 2017)") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))

# plot(graf.emisije.v.bdp.2017)

# Uvozimo zemljevid sveta==================================================================================
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


#Zemljevid evropskih drzav v letu 2017 (obarvane glede na velikost BDP)====================================
zemljevid.bdp.2017 <- ggplot() + 
  geom_polygon(data = bdp %>% 
                 filter(Leto == 2017) %>% 
                 transform(BDP.E = BDP.E / 1000000) %>%
                 right_join(europe, by=c("Drzava"="NAME")), aes(x=long, y=lat, group=group, fill=BDP.E)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  scale_fill_gradient(high = "#001933", low="#cce5ff") +
  labs(title = "Zemljevid držav EU", 
       subtitle = "BDP v letu 2017") +
  guides(fill=guide_colorbar("BDP v \nbilijonih €")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right")

# plot(zemljevid.bdp.2017)



#Zemljevid INDEKSA eko izdatkov glede na bdp================================================================
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
        legend.position = "right") +
  scale_fill_gradient(high = "#B2FF66", low="#193300")

# plot(zemljevid.izdatki.v.bdp.2016)


#Razvrščanje (Cluster) ==================================================================================================
##Podobnosti med državami glede na letni BDP in izpuščene emisije
podobnosti <- dcast(bdp, Drzava~Leto, value.var = 'BDP.E')
priprava.plini <- skupno.plini %>% 
  filter(Leto > 2007) %>% 
  dcast(Drzava ~ Leto, value.var = 'skupne.emisije')
podobnosti <- right_join(podobnosti, priprava.plini, by=c('Drzava'))

podobnosti.a <- podobnosti[,-1]
fit <- hclust(dist(scale(podobnosti.a)))
skupine2 <- cutree(fit, 5)

cluster2 <- mutate(podobnosti, skupine2)

zemljevid.cluster <- ggplot() + 
  geom_polygon(data = right_join(cluster2[c(-2:-31)], europe, by=c('Drzava')), aes(x=long, y=lat, group = group, fill=factor(skupine2))) + 
  geom_line() +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) + 
  guides(fill=guide_legend(title='Skupine')) + 
  ggtitle('Razvrščanje držav glede na letni BDP \nin izpuščene emisije') + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
#print(zemljevid.cluster)

#Plotly=========================================================================================================
##Plotly: Pobrani davki in izmerjene vrednosti emisij
plotly.tabela <- inner_join(eko.davki, skupno.plini, by = c('Drzava','Leto'))
plotly.tabela <- plotly.tabela %>% 
  filter(Leto >= "2008") %>%
  transform(skupne.emisije = skupne.emisije / 1000000) %>%
  transform(Pobrani.davki = Pobrani.davki / 1000)

plotly.graf2 <- ggplot(data = plotly.tabela, aes(x=Pobrani.davki, y=skupne.emisije, color=Drzava)) +
  geom_point(aes(frame=Leto)) + 
  scale_x_continuous() +
  xlab("Pobrani davki v milijardah €") + 
  ylab("Vrednost vseh emisij v megatonah") + 
  theme(legend.position = "none")

plotly.graf2 <- ggplotly(plotly.graf2, dynamicTics=TRUE, width = 900)
# print(plotly.graf2)


#FUNKCIJA ZA SHINY==============================================================================================
zemljevid.leto <- function(letnica, sektor="Total - all NACE activities") {
  
  ggplot() + geom_polygon(data = plini.sektorji %>% 
                            filter(Leto == letnica & Sector.gospodarstva == sektor) %>%
                            right_join(europe, by=c("Drzava"="NAME")) %>%
                            transform(skupne.emisije = round(skupne.emisije / 1000000, 4)),
                          aes(x = long, y = lat, group = group, fill=skupne.emisije)) + 
    xlab("") + ylab("")  + 
    guides(fill=guide_colorbar("Vrednost emisij \nv megatonah")) +
    theme(axis.title=element_blank(), 
          axis.text=element_blank(), 
          axis.ticks=element_blank(), 
          panel.background = element_blank()) + 
    scale_fill_gradient(low = "#32CD32", high="#8b4513")
  
}
