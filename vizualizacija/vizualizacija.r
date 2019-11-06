#1. Graf skupnih emisij v Sloveniji v letih 2008-2017 ==================================================
## Izracun ----
emisije.slo <- emisije %>% 
  filter(Drzava == "Slovenia" & Leto >= 2008 & Sector.gospodarstva == "Total - all NACE activities") %>%
  transform(skupne.emisije = round(skupne.emisije / 1000000, 4))

## Graf ----
graf.emisije.slo <- ggplot(data=emisije.slo,
                           aes(x=Leto, y=skupne.emisije)) + 
  geom_line(colour="royalblue", size=1.5) +
  geom_point(colour = "#000000", size=2.5) + 
  theme_minimal() +
  ggtitle('Vrednosti letnih emisij Slovenije \n(seštevek panog v letih 1998-2017)') + 
  ylab("Vrednosti emisij v megatonah") +
  theme(plot.title=element_text(hjust=0.5, size=15, face="bold")) + 
  scale_x_continuous("Leto", labels=as.character(emisije.slo$Leto), breaks=emisije.slo$Leto)

# plot(graf.emisije.slo)


#2. Graf ekodavkov Slovenije v letih 2008-2017=============================================================
## Izracun ----
davki.slovenije <- eko.davki %>% 
  filter(Drzava == "Slovenia" & Leto >= 2008)

## Graf ----
graf.davki.slo <- ggplot(davki.slovenije, aes(x=Leto, y=Pobrani.davki)) + 
  geom_line(colour="royalblue", size=1.5) +
  geom_point(colour="#000000", size=2.5) + 
  theme_minimal() +
  ggtitle('Prihodki Slovenije s strani ekoloških davkov \n(2008-2017)') + 
  ylab("Pobrani davki v milijonih €") +
  theme(plot.title=element_text(hjust=0.5, size=15, face="bold")) + 
  scale_x_continuous("Leto", labels=as.character(davki.slovenije$Leto), breaks=davki.slovenije$Leto)

# plot(graf.davki.slo)


# 3. Indeks emisij držav glede na BDP v letu 2017 ==============================================================
## Izracun ----
emisije.v.bdp <- inner_join(emisije, bdp, by=c("Drzava", "Leto")) %>% 
  filter(skupne.emisije != "0" & is.na(skupne.emisije) == FALSE & Leto == 2017 & Sector.gospodarstva == "Total - all NACE activities") %>%
  transform(emisije.v.bdp.stolpec = round(skupne.emisije / BDP.E, 4)) %>%
  select(Drzava, emisije.v.bdp.stolpec)

## Graf ----
graf.emisije.v.bdp.2017 <- ggplot(emisije.v.bdp, 
                                  aes(x=reorder(Drzava, emisije.v.bdp.stolpec),
                                      y=emisije.v.bdp.stolpec,
                                      fill=factor(ifelse(emisije.v.bdp$Drzava=="Slovenia","T","F")))) + 
  geom_bar(stat="identity", show.legend=FALSE) +
  scale_fill_manual(name="emisije.v.bdp.2017$Drzava", values=c("royalblue","red")) +
  xlab("Države") + 
  ylab("Skupne emisije / BDP\n(tone / mio €)") + 
  ggtitle("Indeks emisij glede na BDP \n(države EU v letu 2017)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

# plot(graf.emisije.v.bdp.2017)


# 4. Uvoz zemljevida sveta==================================================================================
source("lib/uvozi.zemljevid.r")
svet <- uvozi.zemljevid("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", 
                        "ne_50m_admin_0_countries", encoding = "utf-8") %>% fortify()

# Skrcitev na zemljevid Evrope
europe <- svet %>% filter(CONTINENT == "Europe") %>% 
  filter(long < 55 & long > -25 & lat > 35 & lat < 72) %>%
  filter(NAME != "Jersey" & NAME != "Russia")

# Ureditev tabele 'europe'
colnames(europe)[26] <- 'Drzava'
europe$NAME <- as.character(europe$NAME)


#5. Zemljevid evropskih drzav v letu 2017 (obarvane glede na velikost BDP)====================================
## Izracun ----
bdp.2017 <- bdp %>% 
  filter(Leto == 2017) %>% 
  transform(BDP.E = BDP.E / 1000000) %>%
  right_join(europe, by=c("Drzava"="NAME"))

## Zemljevid ----
zemljevid.bdp.2017 <- ggplot() + 
  geom_polygon(data=bdp.2017, aes(x=long, y=lat, group=group, fill=BDP.E)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  scale_fill_gradient(high="#001933", low="#cce5ff") +
  labs(title = "Zemljevid držav EU", 
       subtitle = "BDP v letu 2017") +
  guides(fill=guide_colorbar("BDP v \nbilijonih €")) +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"),
        plot.subtitle = element_text(hjust=0.5),
        legend.position="right")

# plot(zemljevid.bdp.2017)


#6. Zemljevid INDEKSA eko-izdatkov glede na BDP (2016)==============================================================
## Izracun ----
ekoizdatki.v.bdp <- inner_join(bdp, eko.potrosnja, by=c("Drzava", "Leto")) %>% 
  transform(ekoizdatki.v.bdp.stolpec = round((Izdatki.za.ekologijio / BDP.E), digits=4)) %>%
  filter(Leto == 2016) %>% 
  right_join(europe, by=c("Drzava"="NAME"))
  
## Zemljevid ----
zemljevid.izdatki.v.bdp.2016 <- ggplot() + 
  geom_polygon(data=ekoizdatki.v.bdp, 
               aes(x=long, y=lat, group=group, fill=ekoizdatki.v.bdp.stolpec)) + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  labs(title = "Zemljevid indeksa izdatki za ekologijo glede na BDP",
       subtitle = "(evropske države v letu 2016)") +
  guides(fill=guide_colorbar("Vrednost \nindeksa")) +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"),
        plot.subtitle = element_text(hjust=0.5),
        legend.position="right") +
  scale_fill_gradient(high="#B2FF66", low="#193300")

# plot(zemljevid.izdatki.v.bdp.2016)


# 7. Razvrščanje (Cluster) ==================================================================================================
##Podobnosti med državami glede na letni BDP in izpuščene emisije
podobnosti <- dcast(bdp, Drzava~Leto, value.var='BDP.E')
priprava.plini <- emisije %>% 
  filter(Leto > 2007 & Sector.gospodarstva == "Total - all NACE activities") %>% 
  dcast(Drzava ~ Leto, value.var='skupne.emisije')
podobnosti <- right_join(podobnosti, priprava.plini, by=c('Drzava'))

podobnosti.a <- podobnosti[,-1]
fit <- hclust(dist(scale(podobnosti.a)))
skupine2 <- cutree(fit, 5)

cluster2 <- mutate(podobnosti, skupine2)

zemljevid.cluster <- ggplot() + 
  geom_polygon(data=right_join(cluster2[c(-2:-31)], europe, by=c('Drzava')), 
               aes(x=long, y=lat, group=group, fill=factor(skupine2))) + 
  geom_line() +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), 
        panel.background = element_blank()) + 
  guides(fill=guide_legend(title='Skupine')) + 
  ggtitle('Razvrščanje držav glede na letni BDP \nin izpuščene emisije') + 
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

# print(zemljevid.cluster)


# 8. Plotly: Pobrani davki in izmerjene vrednosti emisij ================================================
## Izracun ----
plotly.tabela <- inner_join(eko.davki, emisije, by=c('Drzava','Leto')) %>% 
  filter(Leto >= "2008" & Sector.gospodarstva == "Total - all NACE activities") %>%
  transform(skupne.emisije = skupne.emisije / 1000000) %>%
  transform(Pobrani.davki = Pobrani.davki / 1000)

## Graf
plotly.graf2 <- ggplot(data=plotly.tabela, aes(x=Pobrani.davki, y=skupne.emisije, color=Drzava)) +
  geom_point(aes(frame=Leto)) + 
  scale_x_continuous() +
  xlab("Pobrani davki v milijardah €") + 
  ylab("Vrednost vseh emisij v megatonah") + 
  theme(legend.position="none")

plotly.graf2 <- ggplotly(plotly.graf2, dynamicTics=TRUE, width=900)

# print(plotly.graf2)


# 9. Graf KOLICINE EMISIJ VSEH SEKTORJEV GLEDE na povrsino gozda (2017) ===============================
## Izracun ----
gozd.emisije <- inner_join(gozd, emisije, by="Drzava") %>% 
  transform(Emisije.na.povrsino = skupne.emisije / Povrsina.gozda) %>%
  filter(Leto == 2017 & Sector.gospodarstva == "Total - all NACE activities") %>%
  filter(skupne.emisije != "0" & is.na(Emisije.na.povrsino) == FALSE) %>%
  filter(Drzava != "Malta" & Drzava != "Netherlands") %>%
  select(Drzava, Emisije.na.povrsino)

## Graf ----
graf.gozd.emisije <- ggplot(gozd.emisije, 
                                  aes(x = reorder(Drzava, Emisije.na.povrsino),
                                      y = Emisije.na.povrsino,
                                      fill=factor(ifelse(gozd.emisije$Drzava == "Slovenia","T","F")))) + 
  geom_bar(stat="identity", show.legend=FALSE) +
  scale_fill_manual(name = "gozd.emisije$Drzava", values=c("#66CC00","red")) +
  xlab("Države") + 
  ylab("Skupne emisije / površina gozda\n(tone / kvadratni kilometri)") + 
  ggtitle("Vrednost emisij glede na površino gozda v letu 2017") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

# plot(graf.gozd.emisije)


# 10. FUNKCIJA ZA SHINY==============================================================================================
zemljevid.leto <- function(letnica, sektor="Total - all NACE activities") {
  
  ggplot() + geom_polygon(data=emisije %>% 
                            filter(Leto == letnica & Sector.gospodarstva == sektor) %>%
                            right_join(europe, by=c("Drzava"="NAME")) %>%
                            transform(skupne.emisije = round(skupne.emisije / 1000000, 4)),
                          aes(x=long, y=lat, group=group, fill=skupne.emisije)) + 
    xlab("") + ylab("")  + 
    guides(fill=guide_colorbar("Vrednost emisij \nv megatonah")) +
    theme(axis.title=element_blank(), 
          axis.text=element_blank(), 
          axis.ticks=element_blank(), 
          panel.background=element_blank()) + 
    scale_fill_gradient(low="#32CD32", high="#8b4513")
}
