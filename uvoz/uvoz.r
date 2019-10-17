source("lib/libraries.r")

#1. TABELA: BDP v tekočih cenah (v milijonih evrov) (1998-2018)------------------------------------
bdp <- read_csv('podatki/BDP.csv', skip = 1,
                col_names = c("A", "Drzava", "Leto", "B", "BDP.E"),
                na = c(":", " ", "", "-"),
                locale=locale(grouping_mark=".", encoding="Windows-1250"))

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

bdp$BDP.E <- as.numeric(bdp$BDP.E)



#2. TABELA: ŠTEVILO PREBIVALCEV -------------------------------------------------------------------
populacija <- read_csv('podatki/populacija.csv', skip = 1,
                       locale=locale(grouping_mark=".", encoding="Windows-1250"),
                       col_names = c("A", "Drzava", "Leto", "B", "C", "Stevilo.prebivalcev"),
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

populacija$Stevilo.prebivalcev <- as.numeric(populacija$Stevilo.prebivalcev)



#3. TABELA: Konsolidirani državni dolg (v milijonih evrov)-----------------------------------------
dolg <- read_csv(file = 'podatki/dolg.csv', skip = 1,
                 locale=locale(grouping_mark=".", encoding="Windows-1250"),
                 col_names = c("A", "Drzava", "Leto", "B", "C", "Dolg"),
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

dolg$Dolg <- as.numeric(dolg$Dolg)

#dolg.slovenija <- filter(dolg, Drzava =='Slovenia')



#4. TABELA: Izdatki držav za ekologijo (v milijonih evrov)-----------------------------------------
eko.potrosnja <- read_csv(file = 'podatki/ekoloska_potrosnja.csv', skip = 1,
                          locale=locale(grouping_mark=".", encoding="Windows-1250"),
                          col_names = c("A", "Drzava", "Leto", "B", "Izdatki.za.ekologijio"),
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

eko.potrosnja$Izdatki.za.ekologijio <- as.numeric(eko.potrosnja$Izdatki.za.ekologijio)




#5. TABELA: Izmerjene vrednosti emisij (v tonah)---------------------------------------------------
emisije <- read_csv('podatki/emisije.csv', na=c(":", " ", "", "-"), skip=1,
                    locale=locale(grouping_mark=" ", encoding="Windows-1250"),
                    col_names=c("Enota", "Drzava", "Leto", "Merjen.plin",
                                "Sector.gospodarstva", "Izpuscene.emisije"))

emisije %>% group_by(Drzava, Leto, Sector.gospodarstva) %>% 
  summarise(Skupne.emisije=sum(Izpuscene.emisije, na.rm=TRUE))

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
emisije$Izpuscene.emisije <- as.numeric(emisije$Izpuscene.emisije)


metan <- emisije %>% filter(Merjen.plin == "Methane")
metan <- metan[,-4]
co.2 <- emisije %>% filter(Merjen.plin == "Carbon dioxide")
co.2 <- co.2[,-4]
no.2 <- emisije %>% filter(Merjen.plin == "Nitrous oxide")
no.2 <- no.2[,-4]

skupno.plini <- inner_join(metan, co.2, by = c("Drzava", "Leto", "Sector.gospodarstva"))
skupno.plini <- inner_join(skupno.plini, no.2, by = c("Drzava", "Leto", "Sector.gospodarstva"))
skupno.plini$skupne.emisije <- skupno.plini$Izpuscene.emisije.x + skupno.plini$Izpuscene.emisije.y +
  skupno.plini$Izpuscene.emisije
skupno.plini <- skupno.plini[, c(-(4:9), -1)]



#6. TABELA: Pobrani davki s strani ekoloških dajatev (v miljonih evrov)----------------------------
eko.davki <- read_csv(file = 'podatki/ekoloski_davki.csv', skip = 1,
                      locale=locale(grouping_mark=".", encoding="Windows-1250"),
                      col_names = c("A", "Drzava", "Leto", "B", "Pobrani.davki"),
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

eko.davki$Pobrani.davki <- as.numeric(eko.davki$Pobrani.davki)



#=IZRAČUNI==================================================================================
#BDP NA PREBIVALCA
bdp.pc <- inner_join(bdp, populacija, by=c("Drzava", "Leto"))
bdp.pc <- transform(bdp.pc, bdp.pc.stolpec = round(((bdp.pc$BDP.E/bdp.pc$Stevilo.prebivalcev)*
                                                      1000000), digits = 2))
bdp.pc <- bdp.pc[,c(-3,-4)]


#DOLG V BDP
dolg.v.bdp <- inner_join(bdp, dolg, by=c("Drzava", "Leto"))
dolg.v.bdp <- transform(dolg.v.bdp, dolg.v.bdp.stolpec = 1 - 
                          round((dolg.v.bdp$Dolg/dolg.v.bdp$BDP.E), digits = 4))
dolg.v.bdp <- dolg.v.bdp[,c(-3,-4)]


#IZDATKI ZA EKOLOGIJO V BDP
ekoizdatki.v.bdp <- inner_join(bdp, eko.potrosnja, by=c("Drzava", "Leto"))
ekoizdatki.v.bdp <- transform(ekoizdatki.v.bdp, ekoizdatki.v.bdp.stolpec = 
                                round((ekoizdatki.v.bdp$Izdatki.za.ekologijio/
                                         ekoizdatki.v.bdp$BDP.E), digits = 4))
ekoizdatki.v.bdp <- ekoizdatki.v.bdp[,c(-3,-4)]


#IZDATKI GLEDE NA POBRANE DAVKE
ekoizdatki.v.davkih <- inner_join(eko.potrosnja, eko.davki, by=c("Drzava", "Leto"))
ekoizdatki.v.davkih <- transform(ekoizdatki.v.davkih, ekoizdatki.v.davkih.stolpec = 
                                   round((ekoizdatki.v.davkih$Izdatki.za.ekologijio/
                                            ekoizdatki.v.davkih$Pobrani.davki), digits = 4))
ekoizdatki.v.davkih <- ekoizdatki.v.davkih[,c(-3,-4)]



#IZVOZ TABEL=====================================================================================
write.csv2(bdp,'podatki/tidy_BDP.csv', fileEncoding = 'UTF-8')
write.csv2(populacija,'podatki/tidy_populacija.csv', fileEncoding = 'UTF-8')
write.csv2(dolg,'podatki/tidy_dolg.csv', fileEncoding = 'UTF-8')
write.csv2(eko.potrosnja,'podatki/tidy_ekoloska_potrosnja.csv', fileEncoding = 'UTF-8')
write.csv2(eko.davki,'podatki/tidy_ekoloski_davki.csv', fileEncoding = 'UTF-8')
write.csv2(emisije,'podatki/tidy_emisije.csv', fileEncoding = 'UTF-8')