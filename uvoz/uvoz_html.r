# Pokritost držav z gozdom
source("lib/libraries.r")
uvozi.gozd <- function(){
  link <- "https://www.worldatlas.com/articles/european-countries-with-the-most-forest-cover.html"
  stran <- html_session(link) %>% read_html()
  gozd <- stran %>% html_node(xpath="//*[@id='artReg-table']/table") %>% 
    html_table()
  
}


gozd <- uvozi.gozd()

#Pokritost v 1000 hektarih
# Ciscenje
gozd <- gozd[,-1]
colnames(gozd) <- c("Drzava", "Povrsina.gozda")
gozd$Povrsina.gozda <- parse_number(gozd$Povrsina.gozda, na = character(), 
                                    locale = locale(decimal_mark = ".", grouping_mark = ",", encoding = "UTF-8"))
