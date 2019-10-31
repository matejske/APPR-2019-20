# UVOZ BDP-ja z Wikipedije

uvozi.bruto.domaci.proizvod <- function(){
  link <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
  stran <- html_session(link) %>% read_html()
  bruto.domaci.proizvod <- stran %>% html_nodes(xpath= "//*[@id="mw-content-text"]/div/table[2]") %>% 
    html_table()

}

bruto.domaci.proizvod <- uvozi.bruto.domaci.proizvod()


#Alternativno iz:
#https://appsso.eurostat.ec.europa.eu/nui/submitViewTableAction.do