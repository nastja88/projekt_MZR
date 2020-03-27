set.seed(2020)


izbira_nakljucne_karte <- function (odkrite_karte, pobrane_skupine, izbrane_karte, spomin) {
  # funkcija "izbira_nakljucne_karte" izbere naključno karto izmed vseh še ne pobranih kart 
  # pri dani matriki odkritih kart "odkrite_karte", matriki "izbrane_karte" z indeksi vrstic
  # in stolpcev v tem koraku izbranih kart in verjetnosti nepozabljanja "spomin" iz [0,1]; z 
  # verjetnostjo "spomin" izbiramo izmed še neodkritih kart, z verjetnostjo 1-"spomin" pa 
  # izmed vseh še ne pobranih kart ; 1 pomeni popoln spomin, 0 pa popolno naključnost
  
  n <- nrow(odkrite_karte)  # število skupin
  k <- ncol(odkrite_karte)  # velikost skupin
  izbira_neodkrite <- (runif(1) <= spomin)  # indikator ali izbiramo med neodkritimi ali nepobranimi odkritimi
  
  nepobrane_odkrite <- odkrite_karte - pobrane_skupine  # vsakemu stolpcu odštejemo morebitne pobrane
  indeksi_nepobranih_odkritih <- which(nepobrane_odkrite == 1)  # indeksi nepobranih odkritih kart (gre po stolpcih)
  
  if (izbira_neodkrite | (length(indeksi_nepobranih_odkritih) == 0)) {  # med neodkritimi izbiramo tudi, če ni nepobranih odkritih
    indeksi_kart <- which(odkrite_karte == 0)  # indeksi neodkritih kart (gre po stolpcih)
  } else{  # izbiramo med nepobranimi 
    nepobrane_karte <- matrix(1, nrow = n, ncol = k) - pobrane_skupine
    stevilo_izbranih_kart <- min(which(izbrane_karte[1,] == 0)) - 1
    for (i in 1:stevilo_izbranih_kart) {
      nepobrane_karte[izbrane_karte[1,i], izbrane_karte[2,i]] <- 0  # v istem koraku ne izberemo ene karte večkrat
    }
    
    indeksi_kart <- which(nepobrane_karte == 1)
  }
  
  if (length(indeksi_kart) == 1) {
    indeks_karte <- indeksi_kart  # izberemo edino možno karto (moramo zapisati posebej zaradi posebnosti funkcije sample)
  } else {
    indeks_karte <- sample(indeksi_kart, 1)  # indeks izbrane karte
  }
  
  stolpec <- 1  # indeks stolpca, v katerem se nahaja nova karta
  while (indeks_karte > n) {  # zagotovo ne izvedemo več kot (k-1)-krat -> ne pademo iz ranga
    indeks_karte <- indeks_karte - n
    stolpec <- stolpec + 1
  }
  izbrana_karta <- c(indeks_karte, stolpec)
  
  # print("Izbrana karta:")
  # print(izbrana_karta)
  
  return(izbrana_karta)
  
}


izbira_skupine <- function (odkrite_karte, pobrane_skupine, skupine_po_igralcih, igralec, spomin) {
  # funkcija "izbira_skupine" izbere novo skupino pri dani matriki odkritih kart 
  # "odkrite_karte", vektorju pobranih skupin "pobrane_skupine", številu pobranih skupin 
  # "skupine_po_igralcih" po igralcih, indeksu igralca "igralec", ki je na potezi, in 
  # verjetnosti nepozabljanja "spomin" iz [0,1]
  
  k <- ncol(odkrite_karte)  # velikost skupin
  n <- nrow(odkrite_karte)  # število skupin
  
  # vektor "odkrite_skupine" ima 1 na mestu skupine, kjer so vse karte iz skupine že odkrite, 
  # drugje so 0
  odkrite_skupine <- (rowSums(odkrite_karte) == k)  
  # vektor ima 1 na mestu, kjer so vse karte iz skupine že odkrite in skupina še ni pri 
  # nobenem od igralcev, drugje so 0 
  potencialne_skupine <- odkrite_skupine - pobrane_skupine 
  
  if ((sum(potencialne_skupine) >= 1) & (runif(1) <= spomin^k)) { 
    # če so bile že odkrite vse karte iz skupine in se jih igralec spomni 
    
    indeks_skupine <- min(which(potencialne_skupine == 1))  # npr. vzame skupino z najmanjšim indeksom, lahko bi vzeli tudi naključno
    pobrane_skupine[indeks_skupine] <- 1
    skupine_po_igralcih[igralec] <- skupine_po_igralcih[igralec] + 1
    
  } else { # če nobena skupina še ni bila odkrita ali se jih igralec ne spomni
    
    # matrika "izbrane_karte" ima v i-tem stolpcu zapisana indeksa vrstice in stolpca 
    # (v tem zaporedju) i-te izbrane karte 
    izbrane_karte <- matrix(0, nrow = 2, ncol = k) 
    
    for (i in 1:k) {
      
      izbrane_karte[,i] <- izbira_nakljucne_karte(odkrite_karte, pobrane_skupine, izbrane_karte, spomin)
      odkrite_karte[izbrane_karte[1,i], izbrane_karte[2,i]] <- 1
      
      stevilo_razlicnih_kart <- length(unique(izbrane_karte[1,1:i]))
      if (stevilo_razlicnih_kart > 1) {  # odkrili smo dve različni karti -> v tem koraku ne moremo odkriti skupine
        break
      } 
      
      if ((stevilo_razlicnih_kart == 1) & (sum(odkrite_karte[izbrane_karte[1,i],]) == k) &
          (runif(1) <= spomin^(k-i))) {  
        # če je do sedaj vseh i kart enakih in je bilo še ostalih k-i odkritih ter se jih 
        # igralec spomni -> poberemo skupino
        pobrane_skupine[izbrane_karte[1,i]] <- 1
        skupine_po_igralcih[igralec] <- skupine_po_igralcih[igralec] + 1
        break
      }
      
    }
    
  }
  
  return(list("odkrite_karte" = odkrite_karte, 
              "pobrane_skupine" = pobrane_skupine, 
              "skupine_po_igralcih" = skupine_po_igralcih))
  
}


poteza_igre <- function (odkrite_karte, pobrane_skupine, skupine_po_igralcih, igralec, spomin) {
  # funkcija "poteza_igre" naredi eno potezo igre (torej od takrat, ko je igralec 
  # na vrsti, do takrat, ko igro nadaljuje naslednji) pri dani matriki odkritih kart 
  # "odkrite_karte", vektorju pobranih skupin "pobrane_skupine" in številu pobranih skupin 
  # "skupine_po_igralcih" igralca, ki je na potezi
  
  stevilo_prej_najdenih_skupin <- sum(skupine_po_igralcih)
  nadaljuj <- TRUE
  
  while (nadaljuj == TRUE) {
    
    izbira_skupine <- izbira_skupine(odkrite_karte, pobrane_skupine, skupine_po_igralcih, igralec, spomin)
    odkrite_karte <- izbira_skupine$odkrite_karte
    pobrane_skupine <- izbira_skupine$pobrane_skupine
    skupine_po_igralcih <- izbira_skupine$skupine_po_igralcih
    
    stevilo_sedaj_najdenih_skupin <- sum(skupine_po_igralcih)
    # če poberemo skupino in še niso pobrane vse skupine, nadaljujemo
    nadaljuj <- (stevilo_prej_najdenih_skupin + 1 == stevilo_sedaj_najdenih_skupin) & 
      (stevilo_sedaj_najdenih_skupin < n)  
    stevilo_prej_najdenih_skupin <- stevilo_sedaj_najdenih_skupin
    
  }
  
  return(list("odkrite_karte" = odkrite_karte, 
              "pobrane_skupine" = pobrane_skupine, 
              "skupine_po_igralcih" = skupine_po_igralcih))
  
}


igra <- function (p, k, n, spomini) {
  # funkcija "igra" za podano število igralcev "p", velikost skupine "k" in 
  # število skupin "n" odigra naključno igro spomina
  
  odkrite_karte <- matrix(0, nrow = n, ncol = k)  # matrika odkritih kart
  pobrane_skupine <- rep(0, n)  # vektor pobranih skupin
  skupine_po_igralcih <- rep(0, p)  # vektor števila pobranih skupin po igralcih 
  stevilo_potez <- 0  # število potez oziroma zamenjav igralcev
    
  while (sum(pobrane_skupine) != n) {
  
    for (igralec in 1:p) {
      
      # ena poteza igre 
      poteza_igre <- poteza_igre(odkrite_karte, pobrane_skupine, skupine_po_igralcih, igralec, spomini[igralec])
      odkrite_karte <- poteza_igre$odkrite_karte
      pobrane_skupine <- poteza_igre$pobrane_skupine
      skupine_po_igralcih <- poteza_igre$skupine_po_igralcih
      
      stevilo_potez <- stevilo_potez + 1
      
      # print(paste("Poteza:", stevilo_potez))
      # print("Odkrite karte:")
      # print(odkrite_karte)
      # print("Pobrane skupine")
      # print(pobrane_skupine)
      # print("Skupine po igralcih")
      # print(skupine_po_igralcih)
      
      if (sum(pobrane_skupine) == n) {  # igralec je pobral zadnjo skupino
        break
      }
      
    }
  
  }
  
  zaporedje_igralcev <- order(skupine_po_igralcih, decreasing = TRUE)  # indeksi igralcev razvrščeni od zmagovalca do poraženca
  # zmagovalec <- which.max(skupine_po_igralcih)  # če potrebujemo le zmagovalca (hitreje kot zgoraj)
  
  return(list("zaporedje_igralcev" = zaporedje_igralcev, 
              "stevilo_potez" = stevilo_potez))
  
}


p <- 2  # število igralcev
k <- 2  # velikost skupine enakih kart 
n <- 10  # število skupin
spomini <- c(0,0)  # verjetnost nepozabljanja za vsakega od igralcev (nanaša se na posamezno karto)

# igra(p, k, n, spomini)


library(parallel)  # detectCores
library(doParallel)  # registerDoParallel
library(doRNG)  # %dorng%

m <- 100  # število ponovitev poskusa

# uporabili bomo paralelno računanje
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

rez <- foreach(j = 1:m, .combine = "rbind", 
               .export = c("izbira_nakljucne_karte", "izbira_skupine", "poteza_igre")) %dorng% {
                 
                 rezultati_igre <- igra(p, k, n, spomini)
                 
                 # zapišimo rezultate
                 cbind("stevilo_menjav" = rezultati_igre$stevilo_potez,  # število menjav igralcev
                       "zmagovalec" = rezultati_igre$zaporedje_igralcev[1])  # indeks zmagovalca
                 
               }

stopCluster(cl)

rez <- as.data.frame(rez)
saveRDS(rez, "rezultati")

povprecje <- mean(rez$stevilo_menjav)

# grafični prikaz
library(ggplot2)
ggplot(data = rez, aes(x = stevilo_menjav)) +
  geom_histogram(aes(x = stevilo_menjav, y = ..density..), fill = "cadetblue1", bins = 20) +
  geom_line(stat="density", color = "red", size = 1) +
  labs(title = "Prikaz vzorčne porazdelitve števila menjav med igralci", 
       x = "Število menjav", y = "Frekvenca [%]") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

zmage <- table(rez$zmagovalec)
delez_zmag_po_igralcih <- sort(zmage / sum(zmage), decreasing = TRUE) * 100  # v %
