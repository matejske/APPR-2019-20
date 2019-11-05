source("lib/libraries.r")
#=====================================================================================================================
#1. TABELA: BDP v tekočih cenah (v milijonih evrov) (1975-2018)=======================================================
bdp <- read_csv('podatki/BDP.csv', skip = 1,
                col_names = c("A", "Drzava", "Leto", "B", "BDP.E"),
                na = c(":", " ", "", "-"),
                locale=locale(grouping_mark=".", encoding="Windows-1250"))

bdp <- bdp[,c(-1, -4)]
bdp <- bdp %>% 
  filter(BDP.E != "") %>% 
  filter(Leto >= 1998 & Leto <= 2017) %>% 
  filter(Drzava != "European Union - 28 countries" & 
           Drzava != "European Union - 28 countries" &
           Drzava != "European Union - 27 countries (from 2019)" &
           Drzava != "European Union - 15 countries (1995-2004)" &
           Drzava != "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)" &
           Drzava != "Euro area (19 countries)" &
           Drzava != "Euro area (12 countries)")
bdp$Drzava <- gsub('^Germany.*', 'Germany', bdp$Drzava)
bdp$Drzava <- gsub('^Kosovo.*', 'Kosovo', bdp$Drzava)
bdp$BDP.E <- as.numeric(bdp$BDP.E)




#2. TABELA: Izmerjene vrednosti emisij (v tonah) (1995-2017)==========================================================
emisije <- read_csv('podatki/emisije.csv', na=c(":", " ", "", "-"), skip=1,
                    locale=locale(grouping_mark=" ", encoding="Windows-1250"),
                    col_names=c("Enota", "Drzava", "Leto", "Merjen.plin",
                                "Sector.gospodarstva", "Izpuscene.emisije"))

emisije <- emisije %>% 
  group_by(Drzava, Leto, Sector.gospodarstva) %>%
  summarise(skupne.emisije=sum(Izpuscene.emisije, na.rm=TRUE))

emisije <- emisije  %>%
  filter(Leto >= 1998 & Leto <= 2017) %>%
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
emisije$Drzava <- gsub('^Kosovo.*', 'Kosovo', emisije$Drzava)
emisije$skupne.emisije <- as.numeric(emisije$skupne.emisije)




# 3. TABELA: Pokritost držav z gozdom (v 1000 ha -> km^2) =============================
uvozi.gozd <- function(){
  link <- "https://www.worldatlas.com/articles/european-countries-with-the-most-forest-cover.html"
  stran <- html_session(link) %>% read_html()
  gozd <- stran %>% html_node(xpath="//*[@id='artReg-table']/table") %>% 
    html_table()
}


# Čiscenje podatkov
gozd <- uvozi.gozd()
gozd <- gozd[,-1]
colnames(gozd) <- c("Drzava", "Povrsina.gozda")
gozd$Povrsina.gozda <- parse_number(gozd$Povrsina.gozda, na = character(), 
                                    locale = locale(decimal_mark = ".", grouping_mark = ",", encoding = "UTF-8"))

## Pretvorba v kvadratne kilometre (1000 ha = 10 km^2)
gozd <- transform(gozd, Povrsina.gozda = (Povrsina.gozda * 10))




#4. TABELA: Izdatki držav za ekologijo (v milijonih evrov) (2006-2018)================================================
eko.potrosnja <- read_csv(file = 'podatki/ekoloska_potrosnja.csv', skip = 1,
                          locale=locale(grouping_mark=".", encoding="Windows-1250"),
                          col_names = c("A", "Drzava", "Leto", "B", "Izdatki.za.ekologijio"),
                          na = c(":", " ", "", "-"))

eko.potrosnja <- eko.potrosnja[, c(-1, -4)]
eko.potrosnja <- eko.potrosnja  %>% 
  filter(Izdatki.za.ekologijio != "") %>% 
  filter(Leto >= 1998 & Leto <= 2017) %>% 
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
eko.potrosnja$Drzava <- gsub('^Kosovo.*', 'Kosovo', eko.potrosnja$Drzava)
eko.potrosnja$Izdatki.za.ekologijio <- as.numeric(eko.potrosnja$Izdatki.za.ekologijio)




#5. TABELA: ŠTEVILO PREBIVALCEV (1960-2018)==========================================================================
populacija <- read_csv('podatki/populacija.csv', skip = 1,
                       locale=locale(grouping_mark=".", encoding="Windows-1250"),
                       col_names = c("A", "Drzava", "Leto", "B", "C", "Stevilo.prebivalcev"),
                       na = c(":", " ", "", "-"))
populacija <- populacija %>% filter(B == "Total")

populacija <- populacija [, c(-1, -4, -5)]
populacija <- populacija  %>% 
  filter(Stevilo.prebivalcev != "") %>% 
  filter(Leto >= 1998 & Leto <= 2017) %>% 
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
populacija$Drzava <- gsub('^Kosovo.*', 'Kosovo', populacija$Drzava)
populacija$Stevilo.prebivalcev <- as.numeric(populacija$Stevilo.prebivalcev)



#6. TABELA: Pobrani davki s strani ekoloških dajatev (v miljonih evrov) (1995-2017)===================================
eko.davki <- read_csv(file = 'podatki/ekoloski_davki.csv', skip = 1,
                      locale=locale(grouping_mark=".", encoding="Windows-1250"),
                      col_names = c("A", "Drzava", "Leto", "B", "Pobrani.davki"),
                      na = c(":", " ", "", "-"))

eko.davki <- eko.davki  %>% 
  filter(Pobrani.davki != "") %>% 
  filter(Leto >= 1998 & Leto <= 2017) %>% 
  filter(A == "Total environmental taxes" & B == "Million euro") %>% 
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
eko.davki$Drzava <- gsub('^Kosovo.*', 'Kosovo', eko.davki$Drzava)

eko.davki$Pobrani.davki <- as.numeric(eko.davki$Pobrani.davki)



#IZVOZ TABEL (Tidy Data)=====================================================================================
write.csv2(bdp,'podatki/tidy_BDP.csv', fileEncoding = 'UTF-8')
write.csv2(gozd, 'podatki/tidy_gozd.csv', fileEncoding = 'UTF-8')
write.csv2(populacija,'podatki/tidy_populacija.csv', fileEncoding = 'UTF-8')
write.csv2(eko.potrosnja,'podatki/tidy_ekoloska_potrosnja.csv', fileEncoding = 'UTF-8')
write.csv2(eko.davki,'podatki/tidy_ekoloski_davki.csv', fileEncoding = 'UTF-8')
write.csv2(emisije,'podatki/tidy_emisije.csv', fileEncoding = 'UTF-8')
