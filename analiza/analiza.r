source('lib/libraries.r')

eko.potrosnja.slovenija<-eko.potrosnja%>%filter(Drzava == 'Slovenia')

napoved <- lm(data=eko.potrosnja.slovenija, eko.potrosnja.slovenija$Izdatki.za.ekologijio ~Leto)
dodatna.leta <- data.frame(Leto=seq(2016,2020,1))
napoved.potrosnje<-mutate(dodatna.leta, Izdatki.za.ekologijio=predict(napoved,dodatna.leta))

graf.napovedi.praga <- ggplot(eko.potrosnja.slovenija, aes(x=Leto, y=Izdatki.za.ekologijio)) + geom_point(data=napoved.potrosnje, inherit.aes = TRUE) + 
  geom_smooth(method=lm, fullrange=TRUE, color='green') + geom_point() + scale_x_continuous('Leto',breaks = seq(2008, 2020, 1), limits = c(2008, 2020))

