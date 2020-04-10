# poskus prikaza trajanja simulacije skupaj z paralelnim računanjem 
# (še ne deluje!)

# naložimo knjižnice, ki jih potrebujemo
library(shiny)  # vse funkcije, ki so povezane z aplikacijo
library(purrr)  # map
library(parallel)  # detectCores
library(doParallel)  # registerDoParallel
library(doRNG)  # %dorng%
library(ggplot2)  # ggplot
library(doSNOW)  # registerDoSNOW
library(future)  # future
library(promises)  # %...>%
library(ipc)  # AsyncProgress 

source("spomin.R")  # naložimo funkcije, ki jih potrebujemo za igro

p0 <- 2

ui <- fluidPage(
  
  titlePanel("Igra Spomin"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "p",
                  label = "Število igralcev:",
                  min = 2,
                  max = 10,
                  value = p0,
                  step = 1),
      
      sliderInput(inputId = "k",
                  label = "Velikost skupine:",
                  min = 2,
                  max = 10,
                  value = 2,
                  step = 1),
      
      sliderInput(inputId = "n",
                  label = "Število skupin:",
                  min = 2,
                  max = 20,
                  value = 10,
                  step = 1),
      
      sliderInput(inputId = "m",
                  label = "Število ponovitev poskusa:",
                  min = 50,
                  max = 200,
                  value = 100,
                  step = 10),
      
      uiOutput("spomin0"),
      
      actionButton("simulacija", "Izvedi simulacijo")
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "histPlot"),
      
      textOutput(outputId = "zmage_besedilo"),
      
      tableOutput(outputId = "zmage_tabela")
      
    )
  )
)


server <- function(input, output) { 
  
  spomini <- rep(1, p0)
  
  id_names <- reactive(paste0("Spomin igralca ", seq_len(input$p)))
  
  output$spomin0 <- renderUI({
    purrr::map(id_names(), ~ sliderInput(inputId = .x,
                                         label = .x,
                                         min = 0,
                                         max = 1,
                                         value = 1,
                                         step = 0.1))
  })
  
  rez <- eventReactive (eventExpr = input$simulacija,
                        valueExpr = {
                          
    req(input[["Spomin igralca 1"]])  # s tem preprečimo, da bi se ta del izvedel prej kot so nastavljeni spomini
    
    p <- input$p # število igralcev
    k <- input$k  # velikost skupine enakih kart
    n <- input$n  # število skupin
    m <- as.numeric(input$m)  # število ponovitev poskusa
    
    if (exists('spomini') == FALSE) {
      spomini <- rep(1,p)  # verjetnost nepozabljanja posameznega igralca (nanaša se na posamezno karto; zaenkrat za vse igralce enako)
    } else {
      spomini <- sapply(1:p, function(i) { as.numeric(input[[paste0("Spomin igralca ", i)]]) })
    }
    
    progress = AsyncProgress$new(message = "Simulacija iger")
    
    rez <- future({
      
      # uporabili bomo paralelno računanje
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores)
      plan(cluster, workers = cl)
      registerDoSNOW(cl)
      
      rez <- foreach(j = 1:m, .combine = "rbind", .packages = "shiny",
                     .export = c("izbira_nakljucne_karte", "izbira_skupine",
                                 "poteza_igre", "igra", 
                                 "progress", "n", "k")) %dopar% {
                                   
         rezultati_igre <- igra(p, k, n, spomini)
         progress$inc(1/m)
         
         # zapišimo rezultate
         cbind("stevilo_menjav" = rezultati_igre$stevilo_potez,  # število menjav igralcev
               "zmagovalec" = rezultati_igre$zaporedje_igralcev[1])  # indeks zmagovalca
         
       }
      
      stopCluster(cl)
      
      rez <- as.data.frame(rez)
      colnames(rez) <- c("stevilo_menjav", "zmagovalec")
      
      progress$close()
      
      rez
      
    }) 
    
    rez
    
  })

  output$histPlot <- renderPlot({
    
    rez <- (rez() %...>% req())
    povprecje <- (rez$stevilo_menjav %...>% mean())
    standardni_odklon <- (rez$stevilo_menjav %...>% sd())
    
    rez %...>% function (myData) {ggplot(data = myData, aes(x = myData$stevilo_menjav)) +
      geom_histogram(aes(x = myData$stevilo_menjav, y = ..density..), fill = "cadetblue2",
                     bins = min(20, max(myData$stevilo_menjav) - min(myData$stevilo_menjav) + 1)) +
      #geom_line(stat="density", color = "red", size = 1) +
      geom_vline(xintercept = povprecje, size = 1, color = "blueviolet") +
      geom_text(aes(x = povprecje + standardni_odklon/4, y = 0), label = povprecje, color = "blueviolet") +
      labs(title = "Prikaz vzorčne porazdelitve števila menjav med igralci",
           subtitle = "(Z vijolično barvo je označeno vzorčno povprečje.)",
           x = "Število menjav", y = "Frekvenca [%]") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 11),
            legend.position = "none")}
    
  })
  
  output$zmage_besedilo <- renderText({ 
    "Verjetnost zmage posameznega igralca je: " 
  })
  
  output$zmage_tabela <- renderTable ({
    
    rez <- (rez() %...>% req())
    zmage <- (rez$zmagovalec %...>% table())
    if (zmage %...>% function (x) {length(x) == 1}) {
      delez_zmag <- (zmage %...>% 
                       function (x) {data.frame("Igralec" = as.integer(names(x)),
                                                "Verjetnost zmage [%]" = 100)})
    } else {
      delez_zmag <- (zmage %...>% 
                       function (x) {as.data.frame(sort(x/sum(x) * 100, 
                                                        decreasing = TRUE))})
    }
    delez_zmag <- delez_zmag %...>% 
                    function (x) {
                      colnames(x) <- c("Igralec", "Verjetnost zmage [%]")
                      return(x)
                    }
    delez_zmag
   
  })
  
}

shinyApp(ui = ui, server = server)