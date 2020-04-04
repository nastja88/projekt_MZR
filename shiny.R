source("knjiznice.R")  # naložimo knjižnice, ki jih potrebujemo

source("spomin.R")  # naložimo funkcije, ki jih potrebujemo za igro

# uporabili bomo paralelno računanje
no_cores <- detectCores() - 1

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
  
  rez <- reactive ({
     
    req(input[["Spomin igralca 1"]])  # s tem preprečimo, da bi se ta del izvedel prej kot so nastavljeni spomini
    
    input$simulacija
   
    p <- isolate(input$p) # število igralcev
    k <- isolate(input$k)  # velikost skupine enakih kart
    n <- isolate(input$n)  # število skupin
    m <- as.numeric(isolate(input$m))  # število ponovitev poskusa
    
    if (exists('spomini') == FALSE) {
      spomini <- rep(1,p)  # verjetnost nepozabljanja posameznega igralca (nanaša se na posamezno karto; zaenkrat za vse igralce enako)
    } else {
      spomini <- sapply(1:p, function(i) { as.numeric(isolate(input[[paste0("Spomin igralca ", i)]])) })
    }

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
    if (length(zmage) == 1) {
      delez_zmag <- data.frame("Igralec" = as.integer(names(zmage)), "Verjetnost zmage [%]" = 100)
    } else {
      delez_zmag <- as.data.frame(sort(zmage/sum(zmage) * 100, decreasing = TRUE))
    }
    colnames(delez_zmag) <- c("Igralec", "Verjetnost zmage [%]")
    delez_zmag
  })
  
}

shinyApp(ui = ui, server = server)