source("lib/libraries.r")
#loc <- locale(encoding = 'UTF-8', decimal_mark = ',', grouping_mark = '.')

#1. TABELA: BDP v tekočih cenah (v milijonih evrov) (1998-2018)------------------------------------
bdp <- read.csv(file = 'podatki/BDP.csv', 
                col.names = c("A", "Drzava", "Leto", "B", "BDP.E"),
                na = c(":", " ", "", "-"))

bdp <- bdp[,c(-1, -4)]
bdp <- bdp %>% filter(BDP.E != "") %>% filter(Leto > 1997) %>% 
  filter(Drzava != "European Union - 28 countries" & 
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (12 countries)")
bdp$Drzava <- gsub('^Germany.*', 'Germany', bdp$Drzava)


#2. TABELA: ŠTEVILO PREBIVALCEV -------------------------------------------------------------------
populacija <- read.csv(file = 'podatki/populacija.csv', 
                 col.names = c("A", "Drzava", "Leto", "B", "C", "Stevilo.prebivalcev"),
                 na = c(":", " ", "", "-"))
populacija <- populacija %>% filter(B == "Total")

populacija <- populacija [, c(-1, -4, -5)]
populacija <- populacija  %>% filter(Stevilo.prebivalcev != "") %>% filter(Leto > 1997) %>% 
  filter(Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 27 countries (2007-2013)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (18 countries)" &
           Drzava != "Euro area (12 countries)")

populacija$Drzava <- gsub('^Germany.*', 'Germany', populacija$Drzava)
populacija$Drzava <- gsub('^France.*', 'France', populacija$Drzava)



#3. TABELA: Konsolidirani državni dolg (v milijonih evrov)-----------------------------------------
dolg <- read.csv(file = 'podatki/dolg.csv', 
                 col.names = c("A", "Drzava", "Leto", "B", "C", "Dolg"),
                 na = c(":", " ", "", "-"))

dolg <- dolg [, c(-1, -4, -5)]
dolg <- dolg  %>% filter(Dolg != "") %>% filter(Leto > 1997) %>% 
  filter(Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 27 countries (2007-2013)" &
           Drzava != "European Union - 25 countries (2004-2006)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (18 countries)" &
           Drzava != "Euro area (17 countries)" & 
           Drzava != "Euro area (12 countries)")

dolg$Drzava <- gsub('^Germany.*', 'Germany', dolg$Drzava)



#4. TABELA: Izdatki držav za ekologijo (v milijonih evrov)-----------------------------------------
eko.potrosnja <- read.csv(file = 'podatki/ekoloska_potrosnja.csv',
                 col.names = c("A", "Drzava", "Leto", "B", "Izdatki.za.ekologijio"),
                 na = c(":", " ", "", "-"))


eko.potrosnja <- eko.potrosnja[, c(-1, -4)]
eko.potrosnja <- eko.potrosnja  %>% filter(Izdatki.za.ekologijio != "") %>% filter(Leto > 1997) %>% 
  filter(Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 27 countries (2007-2013)" &
           Drzava != "European Union - 25 countries (2004-2006)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (18 countries)" &
           Drzava != "Euro area (17 countries)" & 
           Drzava != "Euro area (12 countries)")

eko.potrosnja$Drzava <- gsub('^Germany.*', 'Germany', eko.potrosnja$Drzava)


#5. TABELA: Izmerjene vrednosti emisij (v tonah)---------------------------------------------------
emisije <- read.csv(file = 'podatki/emisije.csv', dec = '.',
                 col.names = c("A", "Drzava", "Leto", "Merjen.plin", "Sector.gospodarstva", "Izpuscene.emisije"),
                 na = c(":", " ", "", "-"), encoding=('Windows-1250'))


emisije <- emisije  %>% filter(Leto > 1997) %>%
  filter(Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 27 countries (2007-2013)" &
           Drzava != "European Union - 25 countries (2004-2006)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (18 countries)" &
           Drzava != "Euro area (17 countries)" &
           Drzava != "Euro area (12 countries)")

emisije$Drzava <- gsub('^Germany.*', 'Germany', emisije$Drzava)
metan <-emisije %>% filter(Merjen.plin == "Methane")
metan <- metan[,-4]
co.2 <- emisije %>% filter(Merjen.plin == "Carbon dioxide")
co.2 <- co.2[,-4]
no.2 <- emisije %>% filter(Merjen.plin == "Nitrous oxide")
no.2 <- no.2[,-4]
skupno.plini <- inner_join(metan, co.2, by = c("Drzava", "Leto", "Sector.gospodarstva"))
skupno.plini <- inner_join(skupno.plini, no.2, by = c("Drzava", "Leto", "Sector.gospodarstva"))
skupno.plini$skupne.emisije <- as.numeric(skupno.plini$Izpuscene.emisije.x) + as.numeric(skupno.plini$Izpuscene.emisije.y)

#6. TABELA: Pobrani davki s strani ekoloških dajatev (v miljonih evrov)----------------------------
eko.davki <- read.csv(file = 'podatki/ekoloski_davki.csv',
                 col.names = c("A", "Drzava", "Leto", "B", "Pobrani.davki"),
                 na = c(":", " ", "", "-"))

eko.davki <- eko.davki  %>% filter(Pobrani.davki != "") %>% filter(Leto > 1997) %>% 
  filter(A == "Total environmental taxes") %>% filter(B == "Million euro") %>%
  filter(Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 27 countries (2007-2013)" &
           Drzava != "European Union - 25 countries (2004-2006)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (18 countries)" &
           Drzava != "Euro area (17 countries)" & 
           Drzava != "Euro area (12 countries)")
eko.davki <- eko.davki[,c(-1,-4)]
eko.davki$Drzava <- gsub('^Germany.*', 'Germany', eko.davki$Drzava)
