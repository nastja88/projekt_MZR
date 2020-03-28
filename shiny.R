library(shiny)

source("knjiznice.R")  # naložimo knjižnice, ki jih potrebujemo
source("spomin.R")  # naložimo funkcije, ki jih potrebujemo za igro

m <- 100  # število ponovitev poskusa

# uporabili bomo paralelno računanje
no_cores <- detectCores() - 1

ui <- fluidPage(
  
  titlePanel("Igra Spomin"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "igralci",
                  label = "Število igralcev:",
                  min = 2,
                  max = 10,
                  value = 2,
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
      
      sliderInput(inputId = "spomin",
                  label = "Spomin:",
                  min = 0,
                  max = 1,
                  value = 1,
                  step = 0.1),
      
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
  
  rez <- reactive ({
    
    input$simulacija
    
    p <- isolate(input$igralci) # število igralcev
    k <- isolate(input$k)  # velikost skupine enakih kart
    n <- isolate(input$n)  # število skupin
    spomin <- isolate(input$spomin)  # verjetnost nepozabljanja posameznega igralca (nanaša se na posamezno karto; zaenkrat za vse igralce enako)
    spomini <- rep(spomin, p)
    
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    
    rez <- foreach(j = 1:m, .combine = "rbind",
                   .export = c("izbira_nakljucne_karte", "izbira_skupine",
                               "poteza_igre", "igra")) %dorng% {
                                 
                                 rezultati_igre <- igra(p, k, n, spomini)
                                 
                                 # zapišimo rezultate
                                 cbind("stevilo_menjav" = rezultati_igre$stevilo_potez,  # število menjav igralcev
                                       "zmagovalec" = rezultati_igre$zaporedje_igralcev[1])  # indeks zmagovalca
                                 
                               }
    
    stopCluster(cl)
    
    rez <- as.data.frame(rez)
    rez
    
  })
  
  output$histPlot <- renderPlot({
    
    rez <- rez()
    povprecje <- mean(rez$stevilo_menjav)
    standardni_odklon <- sd(rez$stevilo_menjav)
    
    ggplot(data = rez, aes(x = stevilo_menjav)) +
      geom_histogram(aes(x = stevilo_menjav, y = ..density..), fill = "cadetblue2", 
                     bins = min(20, max(rez$stevilo_menjav) - min(rez$stevilo_menjav) + 1)) +
      #geom_line(stat="density", color = "red", size = 1) +
      geom_vline(xintercept = povprecje, size = 1, color = "blueviolet") +     
      geom_text(aes(x = povprecje + standardni_odklon/4, y = 0), label = povprecje, color = "blueviolet") +
      labs(title = "Prikaz vzorčne porazdelitve števila menjav med igralci",
           subtitle = "(Z vijolično barvo je označeno vzorčno povprečje.)",
           x = "Število menjav", y = "Frekvenca [%]") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5), 
            plot.subtitle = element_text(hjust = 0.5, size = 11),
            legend.position = "none")
    
  })
  
  output$zmage_besedilo <- renderText({ "Verjetnost zmage posameznega igralca je: " })
  
  output$zmage_tabela <- renderTable ({
    zmage <- table(rez()$zmagovalec)
    delez_zmag <- as.data.frame(sort(zmage/sum(zmage) * 100, decreasing = TRUE))  # v %
    colnames(delez_zmag) <- c("Igralec", "Verjetnost zmage [%]")
    delez_zmag
  })
  
}

shinyApp(ui = ui, server = server)