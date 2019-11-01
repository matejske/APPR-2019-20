# Analiza podatkov s programom R, 2019/20

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2019/20

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/matejske/APPR-2019-20/master?urlpath=shiny/APPR-2019-20/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/matejske/APPR-2019-20/master?urlpath=rstudio) RStudio

## Analiza okoljske ozaveščenosti evropskih držav

Analiziral sem ekološke podatke držav Evropske unije. Za merilo ozaveščenosti sem izbral izdatke posamezne države za ekologijo, izmerjene vrednosti emisij treh najbolj škodljivih toplogrednih plinov, površino  pokrito z gozdom in pobrane davke s strani ekoloških dajatev. Zgoraj omenjene podatke sem primerjal z BDP-jem, številom prebivalcev in konsolidiranim državnim dolgom. Naredil sem tudi napovedi količine državnih izdatkov za ekologijo za Slovenijo in Nemčijo.

Da bom lahko opazoval povezave med temi spremenljivkami, bom podatke uredil v sledeče tabele. 

Tabele:
1. `bdp` - podatki o bruto domačem proizvodu (BDP) držav Evropske unije (1998-2017)
  - `Drzava` - - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `BDP.E` - meritev: BDP v tekočih cenah v milijonih evrov, (število)
  
2. `emisije` - letne meritve  treh najškodljivejših toplogrednih plinov v ozračju po sektorjih gospodarstva 
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `Merjen.plin` - spremenljivka: vrsta plina (Carbon dioxide, Carbon dioxide, Nitrous oxide), (neurejen faktor)
  - `Sector.gospodarstva` - spremenljivka: sektor gospodarstva, ki je v zrak izpustil določeno količino plina, (neurejen faktor)
  - `Izpuscene.emisije` - meritev: letna izmerjena količina v tonah, (število)

3. `gozd` - površina gozdnatih površin v kvadratnih kilometrih v letu 2017
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Povrsina.gozda` - meritev: skupna površina v kvadratnih kilometrih, (število)

4. `dolg` - konsolidirani državni dolg Konsolidirani državni dolg (1998-2017)
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `Dolg` - meritev: vrednost dolga v milijonih evrov, (število)

5. `eko.potrosnja` - letni državni izdatki za ekologijo (1998-2017)
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `Izdatki.za.ekologijio` - meritev: izdatki (potrošnja) držav za ekologijo v milijonih evrov, (število)
  
6. `populacija` - število prebivalcev držav Evropske unije (1998-2017)
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `Stevilo.prebivalcev` - meritev: število prebivalcev, (število)

7. `eko.davki` - letni državni prihodki s strani ekoloških davkov za podjetja in posameznike
  - `Drzava` - spremenljivka: imena opazovanih držav, (neurejen faktor)
  - `Leto` - spremenljivka: leto testiranja, (število)
  - `Pobrani.davki` - meritev: vrednost pobranih ekoloških davkov v milijonih evrov, (število)

Podatki bodo obsegali obdobje zadnjih 20 let, torej 1998 - 2018.

Viri: 
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=nama_10_gdp&lang=en 
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=demo_pjan&lang=en
* https://www.worldatlas.com/articles/european-countries-with-the-most-forest-cover.html
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=gov_10dd_ggd&lang=en
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=env_ac_epneis&lang=en
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=env_ac_ainah_r2&lang=en
* https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=env_ac_tax&lang=en


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
