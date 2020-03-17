# oba igralca imata popoln spomin
set.seed(2020)

izbira_nakljucne_karte <- function (k, n, odkrite_karte) {
  # funkcija "izbira_nakljucne_karte" izbere naključno karto izmed vseh še ne 
  # odkritih kart (vsako izmed njih izbere z enako verjetnostjo) pri dani 
  # velikosti >>parov<< "k", številu >>parov<< "n", matriki odkritih kart 
  # "odkrite_karte"
  
  indeksi_neodkritih <- which(odkrite_karte == 0)  # indeksi neodkritih kart (gre po stolpcih)
  
  if (length(indeksi_neodkritih) == 1) {
    indeks_karte <- indeksi_neodkritih  # izberemo zadnjo še neodkrito karto
  } else {
    indeks_karte <- sample(indeksi_neodkritih, 1)  # indeks izbrane karte
  }
  
  
  if (indeks_karte <= n) {
    izbrana_karta <- c(indeks_karte, 1)
  } else {
    izbrana_karta <- c(indeks_karte - n, 2)
  }
  
  return(izbrana_karta)
  
}


izbira_para <- function (k, n, odkrite_karte, pobrani_pari, pari, igralec) {
  # funkcija "izbira_para" izbere nov par pri dani velikosti >>parov<< "k", številu 
  # >>parov<< "n", matriki odkritih kart "odkrite_karte", vektorju pobranih parov 
  # "pobrani_pari", številu pobranih parov "pari" po igralcih in indeks igralca "igralec",
  # ki je na potezi
  
  # vektor "odkriti_pari" ima 1 na mestu skupine, kjer sta obe karti iz para že odkriti, 
  # drugje so 0
  odkriti_pari <- (rowSums(odkrite_karte) == k)  
  # vektor ima 1 na mestu, kjer sta obe karti iz para že odkriti in par še ni pri nobenem 
  # od igralcev, drugje so 0 (v resnici imamo zaradi popolnega spomina lahko največ k enic)
  potencialni_pari <- odkriti_pari - pobrani_pari 
  
  if (sum(potencialni_pari) >= 1) { # če igralec že ve, kje je par
    
    indeks_para <- min(which(potencialni_pari == 1))  # npr. vzame par z najmanjšim indeksom, lahko bi vzeli tudi naključnega
    pobrani_pari[indeks_para] <- 1
    pari[igralec] <- pari[igralec] + 1
    
  } else { # če igralec ne pozna nobenega para
    
    # izbira prve karte (naključna - izmed še neodkritih vse z enako verjetnostjo)
    izbrana_karta_1 <- izbira_nakljucne_karte(k, n, odkrite_karte)
    odkrite_karte[izbrana_karta_1[1], izbrana_karta_1[2]] <- 1
    
    
    # izbira druge karte
    
    ## če se prva karta ujema s katero izmed že odkritih
    if (sum(odkrite_karte[izbrana_karta_1[1],]) == 2) {
      
      pobrani_pari[izbrana_karta_1[1]] <- 1
      pari[igralec] <- pari[igralec] + 1
      
    } else { ## če se prva karta ne ujema z nobeno izmed že odkritih (naključna izbira izmed preostalih)
      
      izbrana_karta_2 <- izbira_nakljucne_karte(k, n, odkrite_karte)
      odkrite_karte[izbrana_karta_2[1], izbrana_karta_2[2]] <- 1
      
      ## če se karti ujemata, smo našli par
      if (izbrana_karta_1[1] == izbrana_karta_2[1]) {
        pobrani_pari[izbrana_karta_1[1]] <- 1
        pari[igralec] <- pari[igralec] + 1
      }
      
    }
  }
  
  return(list("odkrite_karte" = odkrite_karte, 
              "pobrani_pari" = pobrani_pari, 
              "pari" = pari))
  
}


poteza_igre <- function (k, n, odkrite_karte, pobrani_pari, pari, igralec) {
  # funkcija "poteza_igre" naredi eno potezo igre (torej od takrat, ko je igralec 
  # na vrsti, do takrat, ko igro nadaljuje naslednji) pri dani velikosti >>parov<< 
  # "k", številu >>parov<< "n", matriki odkritih kart "odkrite_karte", vektorju 
  # pobranih parov "pobrani_pari" in številu pobranih parov "pari" igralca, ki 
  # je na potezi
  
  stevilo_prej_najdenih_parov <- sum(pari)
  nadaljuj <- TRUE
  
  while (nadaljuj == TRUE) {
    
    izbira_para <- izbira_para(k, n, odkrite_karte, pobrani_pari, pari, igralec)
    odkrite_karte <- izbira_para$odkrite_karte
    pobrani_pari <- izbira_para$pobrani_pari
    pari <- izbira_para$pari
    
    stevilo_sedaj_najdenih_parov <- sum(pari)
    # če poberemo par in še niso pobrani vsi pari, nadaljujemo
    nadaljuj <- (stevilo_prej_najdenih_parov + 1 == stevilo_sedaj_najdenih_parov) & 
      (stevilo_sedaj_najdenih_parov < n)  
    stevilo_prej_najdenih_parov <- stevilo_sedaj_najdenih_parov
    
  }
  
  return(list("odkrite_karte" = odkrite_karte, 
              "pobrani_pari" = pobrani_pari, 
              "pari" = pari))
  
}


igra <- function (p, k, n) {
  # funkcija "igra" za podano število igralcev "p", velikost skupine "k" in 
  # število skupin "n" odigra naključno igro spomina
  
  odkrite_karte <- matrix(0, nrow = n, ncol = k)  # matrika odkritih parov
  pobrani_pari <- rep(0, n)  # vektor pobranih parov
  pari <- rep(0, p)  # vektor števila pobranih parov 
  stevilo_potez <- 0  # število potez oziroma zamenjav igralcev
    
  while (sum(pobrani_pari) != n) {
  
    for (igralec in 1:p) {
      
      # ena poteza igre 
      poteza_igre <- poteza_igre(k, n, odkrite_karte, pobrani_pari, pari, igralec)
      odkrite_karte <- poteza_igre$odkrite_karte
      pobrani_pari <- poteza_igre$pobrani_pari
      pari <- poteza_igre$pari
      
      stevilo_potez <- stevilo_potez + 1
      
      print(paste("Poteza:", stevilo_potez))
      print("Odkrite karte:")
      print(odkrite_karte)
      print("Pobrani pari")
      print(pobrani_pari)
      print("Pari")
      print(pari)
      
      if (sum(pobrani_pari) == n) {  # igralec je pobral zadnji par
        break
      }
      
    }
  
  }
  
  zaporedje_igralcev <- order(pari, decreasing = TRUE)  # indeksi igralcev razvrščeni od zmagovalca do poraženca
  # zmagovalec <- which.max(pari)  # če potrebujemo le zmagovalca (hitreje kot zgoraj)
  
  return(list("zaporedje_igralcev" = zaporedje_igralcev, 
              "stevilo_potez" = stevilo_potez))
  
}


p <- 2  # število igralcev
k <- 2  # velikost >>para<< oziroma skupine enakih kart # zaenkrat deluje le za 2!
n <- 10  # število >>parov<<

igra(p, k, n)

