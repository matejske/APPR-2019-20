#Napoved ekoloških izdatkov Slovenije v prihodnjih letih =======================================
eko.potrosnja.slovenija <- eko.potrosnja %>%
  filter(Drzava == 'Slovenia') %>%
  transform(Izdatki.za.ekologijio = Izdatki.za.ekologijio / 1000)

napoved <- lm(data=eko.potrosnja.slovenija, eko.potrosnja.slovenija$Izdatki.za.ekologijio ~Leto)
dodatna.leta <- data.frame(Leto=seq(2017,2021,1))
napoved.potrosnje <- mutate(dodatna.leta, Izdatki.za.ekologijio=predict(napoved, dodatna.leta))

graf.napovedi.praga.slo <- ggplot(eko.potrosnja.slovenija, aes(x=Leto, y=Izdatki.za.ekologijio)) + 
  geom_smooth(method=lm, fullrange=TRUE, color='yellow') + 
  geom_point(data=napoved.potrosnje, inherit.aes=TRUE, colour="red") + 
  geom_point() + 
  scale_x_continuous('Leto', breaks=seq(2008, 2021, 1), limits=c(2008, 2021)) +
  ggtitle('Napoved izdatkov za ekologijo Slovenije v letih 2017-2021') + 
  theme(plot.title=element_text(hjust=0.5, size=15, face="bold")) +
  theme_bw() +
  xlab("Leto") + 
  ylab("Izdatki za ekologijo v milijardah €")

# plot(graf.napovedi.praga.slo)


##Napoved ekoloških izdatkov Nemčije v prihodnjih letih =======================================
eko.potrosnja.nemcija <- eko.potrosnja %>% filter(Drzava == 'Germany')
eko.potrosnja.nemcija <- transform(eko.potrosnja.nemcija, 
                                   Izdatki.za.ekologijio = eko.potrosnja.nemcija$Izdatki.za.ekologijio / 1000)

napoved <- lm(data=eko.potrosnja.nemcija, eko.potrosnja.nemcija$Izdatki.za.ekologijio ~Leto)
dodatna.leta <- data.frame(Leto=seq(2017,2021,1))
napoved.potrosnje <- mutate(dodatna.leta, Izdatki.za.ekologijio=predict(napoved, dodatna.leta))

graf.napovedi.praga.nem <- ggplot(eko.potrosnja.nemcija, aes(x=Leto, y=Izdatki.za.ekologijio)) + 
  geom_smooth(method=lm, fullrange=TRUE, color="yellow") + 
  geom_point(data=napoved.potrosnje, inherit.aes=TRUE, colour="red") + 
  geom_point() + 
  scale_x_continuous('Leto', breaks = seq(2008, 2021, 1), limits=c(2008, 2021)) +
  ggtitle('Napoved izdatkov za ekologijo Nemčije v letih 2016-2020') + 
  theme(plot.title=element_text(hjust=0.5, size=15, face="bold")) +
  theme_bw() +
  xlab("Leto") + 
  ylab("Izdatki za ekologijo v milijardah €")

# plot(graf.napovedi.praga.nem)
