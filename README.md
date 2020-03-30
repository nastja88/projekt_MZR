# Projekt pri predmetu Matematika z računalnikom – Spomin

## Ideja projekta

Igra Spomin vsebuje `k*n` kart oziroma `n` skupin velikosti `k` istih kart (pri standardni verziji je seveda `k = 2`). Igro igra `p` igralcev, pri čemer je `p` vsaj 2. Ko je posamezen igralec na potezi, zaporedoma odkrije `k` kart. Če se ujemajo, jih pobere in nadaljuje z odkrivanjem naslednjih `k` kart, sicer pa je na vrsti naslednji igralec. Igra se konča, ko so vsi pari pobrani, zmaga pa tisti igralec, ki je pobral največ parov.

Pri simulaciji igre predpostavljamo, da so igralci popolnoma racionalni oziroma igrajo optimalno strategijo. Tako je njihov edini cilj zmaga. So pa igralci omejeni s svojim spominom, ki ga modeliramo kot verjetnost (torej ima vrednosti na intervalu [0,1]), da se igralec pri posamezni izbiri karte spomni prejšnjih kart. Vrednost 1 torej pomeni popoln spomin, 0 pa popolno naključnost. Vektor spominov vseh igralcev poimenujemo `spomini`.

## Implementacija igre

Shranjena je v datoteki `spomin.R`. Igro simuliramo s pomočjo štirih funkcij, in sicer `igra`, `poteza_igre`, `izbira_skupine` in `izbira_nakljucne_karte`. Osnovni funkciji `igra` podamo parametre `p`, `k`, `n` in `spomini`. Tekom igre hranimo več podatkov:
* matriko `odkrite_karte` oblike `n*k`, ki vsebuje indikatorje ali je bila posamezna karta že odkrita, 
* vektor `pobrane_skupine` vsebuje indikatorje ali je bila posamezna skupina že pobrana, 
* vektor `skupine_po_igralcih` vsebuje števila pobranih skupin po igralcih, 
* število `stevilo_potez` šteje število potez oziroma zamenjav igralcev.

Funkcija `igra` gre po vrsti po igralcih in za vsakega naredi eno potezo s pomočjo funkcije `poteza_igre`. To počne dokler niso pobrane vse skupine. Vmes šteje število potez, na koncu pa določi še vrstni red uspešnosti igralcev. Oboje ob zaključku tudi vrne.

Prej omenjena funkcija `poteza_igre` naredi eno potezo igre (torej od takrat, ko je igralec na vrsti, do takrat, ko igro nadaljuje naslednji) pri dani matriki odkritih kart `odkrite_karte`, vektorju pobranih skupin `pobrane_skupine` in številu pobranih skupin `skupine_po_igralcih`, indeksu igralca, ki je na potezi, `igralec` in številu `spomin` iz intervala [0,1], ki predstavlja spomin tega igralca. Ta funkcija dela korake posameznega igralca (s pomočjo funkcije `izbira_skupine`) dokler zaporedoma pobira skupine in seveda še niso vse pobrane. Vrne posodobljene vrednosti `odkrite_karte`, `pobrane_skupine` in `skupine_po_igralcih`.

Funkcija `izbira_skupine` sprejme in vrne istovrstne podatke kot prejšnja funkcija. Najprej preveri, ali morda obstaja kakšna skupina, katere karte so bile že vse odkrite, a skupina še ni bila pobrana. S tem pravzaprav preverimo, ali bi igralec lahko že vedel za kakšno skupino istih kart. Če taka skupina obstaja in se je igralec spomni (to implementiramo s pomočjo generiranja iz enakomerne zvezne porazdelitve na intervalu [0,1] in primerjanjem tako dobljenega števila z verjetnostjo, da se spomni vseh `k` kart), skupino poberemo in ustrezno popravimo vektorja `pobrane_skupine` in `skupine_po_igralcih`. V nasprotnem primeru igralec prične z izbiranjem kart preko funkcije `izbira_nakljucne_karte`. Po vsaki izbiri se posodobi matrika `odkrite_karte` in igralec preveri, ali se morda v trenutni izbiri pojavita dve različni karti in če se, prekine z izbiranjem in na potezi je naslednji igralec. Glede na predpostavko o optimalni strategiji namreč ne želi pomagati drugim igralcem z odkrivanjem novih kart, saj ve, da jih sam v tem koraku gotovo ne bo mogel pobrati. Če so v tem koraku vse do sedaj izbrane karte iz iste skupine in so bile že prej odkrite še preostale iz te skupine ter se jih igralec spomni, igralec pobere skupino in posodobita se vektorja `pobrane_skupine` in `skupine_po_igralcih`.

Ostala nam je le še funkcija `izbira_nakljucne_karte`. Ta poleg nekaterih že prej omenjenih argumentov sprejme tudi `2*k` matriko `izbrane_karte`, v katero si zapisujemo v tem koraku izbrane karte, da se izognemo večkratni izbiri iste karte v posameznem koraku. Natančneje si v prvo vrstico i-tega stolpca zapišemo indeks vrste i-te izbrane karte, v drugo vrstico pa pripadajoč indeks stolpca. Tu število `spomin` uporabimo na način, da z verjetnostjo `spomin` izbiramo izmed še neodkritih kart, z verjetnostjo `1 - spomin` pa izmed vseh še ne pobranih kart. V primeru, da neodkritih kart ni več, seveda izbiramo med že odkritimi (a še ne pobranimi). Izmed tako dobljenih možnih kart izžrebamo eno, vse z enako verjetnostjo.
  
## Aplikacija

Aplikacija je shranjena v datoteki `shiny.R` in za njen zagon je dovolj, da zgolj poženemo to datoteko. S tem se najprej naložijo vse potrebne knižnice, ki se nahajajo v datoteki `knjiznice.R`, in funkcije za izvedbo igre v datoteki `spomin.R`. Ob zagonu aplikacije se uporabniku ponudi možnost izbire nastavitve števila igralcev (`p`), velikosti (`k`) in števila (`n`) skupin ter števila ponovitev poskusa (`m`). Po izbiri števila igralcev se pojavi tudi enako število novih izbir spomina (za vsakega od igralcev ena). Za izvedbo simulacije (s paralelnim računanjem) z želenimi parametri je potrebno le še pritisniti na gumb `Izvedi simulacijo`. Po nekaj trenutkih se pojavi histogram števila menjav med igralci s pripadajočim vzorčnim povprečjem. Poleg tega so prikazane tudi vzorčne verjetnosti zmag vsakega od igralcev (razvrščeno po velikosti).
