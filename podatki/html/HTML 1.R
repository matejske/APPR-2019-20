#Ne sme bit isto kot:

library(dplyr)

#https://github.com/vidamaver/APPR-2017






#Uvoz HTML https://github.com/maxfilip98/APPR-2017-18/blob/master/uvoz/uvoz.r




link1 <- "https://ec.europa.eu/eurostat/tgm/refreshTableAction.do?tab=table&plugin=1&init=1&pcode=tec00001&language=en"
stran <- html_session(link1) %>% read_html()
drzave <- stran %>% html_nodes(xpath="//table[@id='fixtable']") %>% .[[1]] %>%
  html_table(header = FALSE)
leta <- stran %>% html_nodes(xpath="//table[@id='headtable']") %>% .[[1]] %>%
  html_table(header = FALSE)
bdp_po_letih_html  <- stran %>% html_nodes(xpath="//table[@id='contenttable']") %>% .[[1]] %>% html_table()
bdp_po_letih_html  <- cbind(drzave, bdp_po_letih_html )
colnames(bdp_po_letih_html ) <- c("drzava", leta)
bdp_po_letih_html  <- bdp_po_letih_html  %>%
  melt(id.vars = "drzava", variable.name = "leto", value.name = "stopnja") %>%
  mutate(leto = parse_number(leto),
         stopnja = parse_number(stopnja, na = ":"))
# bdp_po_letih_html $drzava <- gsub("Former Yugoslav Republic of Macedonia, the", "Macedonia", bdp_po_letih_html $drzava) 
