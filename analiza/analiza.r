source('lib/libraries.r')
source('uvoz/uvoz.r')
#=================================================================================================

eko.potrosnja.slovenija<-eko.potrosnja%>%filter(Drzava == 'Slovenia')

napoved <- lm(data=eko.potrosnja.slovenija, eko.potrosnja.slovenija$Izdatki.za.ekologijio ~Leto)
dodatna.leta <- data.frame(Leto=seq(2016,2020,1))
napoved.potrosnje <- mutate(dodatna.leta, Izdatki.za.ekologijio=predict(napoved,dodatna.leta))

graf.napovedi.praga <- ggplot(eko.potrosnja.slovenija, aes(x=Leto, y=Izdatki.za.ekologijio)) + 
  geom_point(data=napoved.potrosnje, inherit.aes = TRUE) + 
  geom_smooth(method=lm, fullrange=TRUE, color='green') + 
  geom_point() + 
  scale_x_continuous('Leto',breaks = seq(2008, 2020, 1), limits = c(2008, 2020)) +
  ggtitle('Napoved izdatkov za ekologijo Slovenije v letih 2016-2020') + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold")) +
  theme_bw() 

plot(graf.napovedi.praga)
