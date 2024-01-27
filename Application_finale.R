


#---------------------------------------------- IMPORTATION DE LA BASE DE DONNÉES  ----------------------------------------------------------------- # 


# Importation des libraries 
library(readxl)
library(ggplot2)
library(plotly)
library(grid)
library(rworldmap)
library(plyr)
library(reshape2)
library(reshape)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(DT)

# A FAIRE  : Renseigner l'environnement de travail où se trouvent le code et la table
setwd("~/Documents/Data_Vis_2")

# Importer la table
dat<-read_excel("dataprojet.xlsx")




# -------------------------------------------------------------------- PARTIE UI --------------------------------------------------------------------#

ui <- dashboardPage(

  dashboardHeader(title = "l'émission des déchets en europe"),

  dashboardSidebar(

    sidebarMenu(

      menuItem("Présentation", tabName = "presentation", icon = icon("info-circle")),

      menuItem("Analyse descriptive des variables", tabName = "analyse", icon = icon("area-chart")),
      
      menuItem("Evolutions au cours du temps", tabName = "evolution", icon = icon("chart-line")),
      
      menuItem("Prévisions", tabName = "rapport", icon = icon("file-text"))
      
      

    )

  ),

  dashboardBody(

    tabItems(

      tabItem(
        tabName = "analyse",
        hr(),
        sidebarLayout(
          sidebarPanel(
            sliderInput("temps","Année:",min = 2004,max = 2020,value = 2004,step = 2, sep = ""),

            selectInput("variable","Origine des déchets:", choices = c("l'Agriculture", "l'Industrie", "Les ménages"),
                        selected = "l'Agriculture"),

            hr(),
            hr(),
            hr(),
            
            
            
            uiOutput("Production_totale")

          ),

          mainPanel(

            tabsetPanel(

              tabPanel("Carte", 
                       hr(),
                       h3("Données brutes (Avant normalisation des variables) :"),
                       plotlyOutput("Carte1"), hr(),
                       h3("Données traités (Après Normalisation) :"),
                       plotlyOutput("Carte2"),
                       h3("Pour que les données soient plus repésentatives de la situation actuelle et pour que nous puissions les comparer entre elles, il est indispensable de les normaliser en utilisant les facteurs appropriés."),
                       h3("Ainsi, nous avons rajouter à notre base de données les variables suivantes pour normaliser nos données:"),
                       h3("• La supérficie du pays pour les déchets agricoles"),
                       h3("• La population pour les déchets ménagers"),
                       h3("• Le PIB pour les déchets industriels"),
                       hr()
                       ),

              tabPanel("Barplot", 
                       hr(),
                       h3("Données brutes (Avant normalisation des variables) :"),
                       plotlyOutput("Barplot1"), hr(),
                       h3("Données traités (Après Normalisation) :"),
                       plotlyOutput("Barplot2")
                       ),

              tabPanel("Part de déchets dangereux", 
                       fluidRow(
                         column(6, plotlyOutput("pie_chart_france")),
                         column(6, plotlyOutput("pie_chart_germany"))
                       ),
                       hr(),
                       fluidRow(
                         column(6, plotlyOutput("pie_chart_spain")),
                         column(6, plotlyOutput("pie_chart_netherlands"))
                       ),
                       hr()
              )

            )

          )

        )

      ),
      

      
      tabItem(tabName = "presentation",
              fluidPage(
                h1(
                  style = "color: #08306b; font-size: 6em; text-align: center;",
                  "Présentation de l'étude"
                ),
                
                hr(),
                hr(),
                
                img(
                  src = "https://asialyst.com/fr/wp-content/uploads/2019/04/malaisie-dechets-plastiques-import-e1556380089451.jpg",
                  style = "display: block; margin: auto; max-width: 100%; height: auto;"
                ),
                
                hr(),
                hr(),
                div(
                  style = "width: 90%; font-size: 1.5em;",
                  h1(
                    style = "color: #08306b; font-size: 2.4em;",
                    "Motivations"
                  ),
                  p(
                    "Le sujet de la gestion des déchets est d'une importance capitale à l'ère actuelle, où la durabilité et la préservation de l'environnement sont des préoccupations majeures."
                  ),
                  p(
                    "Comprendre et gérer efficacement la production de déchets est essentiel pour garantir un avenir durable. 
                    Cependant, bien que tout le monde en est conscience, peu réalisent vraiment l'ampleur réelle de la quantité de déchets produite et relachée dans la nature et les océans."
                    ),
                  p(
                    "C'est ici que la data visualisation prend tout son sens : elle offre un moyen puissant de rendre ces données complexes plus accessibles et compréhensibles pour tous. En utilisant des graphiques et des représentations visuelles, la data visualisation permet de traduire ces données souvent volumineuses en histoires claires et percutantes. Elle offre la possibilité de mettre en lumière les tendances, les disparités et les impacts, offrant ainsi des informations cruciales pour les décideurs, les chercheurs et le grand public."
                  ),
                  p(
                    "La visualisation des données sur la production de déchets, au travers de graphiques intuitifs et informatifs, devient donc un outil essentiel pour sensibiliser, informer et inciter à l'action, contribuant ainsi à une gestion plus efficace et éclairée des déchets."
                  ),
                  br(), # Cette ligne ajoutée crée un saut de ligne
                  h1(
                    style = "color: #08306b; font-size: 2.4em;",
                    "Problématiques"
                  ),
                  h3(
                    style = "font-weight: bold; margin-left: 40px;",
                    "• Quelles tendances et facteurs influencent la production de déchets dans les pays européens ?"
                  ),
                  h3(
                    style = "font-weight: bold; margin-left: 40px;",
                    "• Quel est réelement l'impact des législations européennes sur la production de déchets ?"
                  ),
                  
                  br(),
                  br(),
                  
                  h1(
                    style = "color: #08306b; font-size: 2.4em;",
                    "Base de données"
                  ),
                  
                  p(
                    "La base de données que nous allons étudier provient de Euristats et représente la quantité de déchets (en Tonne) produite chaques années par les pays europeens. Les dechets sont classifies par leur type (les dechets plastiques, chimiques etc...), par leur origines (issus de l'industrie, l'agriculture ...) ainsi que leur dangeurosité."
                  ),
                  hr(),
                  DT::dataTableOutput("affichetable")
                  
                  
                )
              )
      ),
      
      tabItem(tabName = "rapport",
                hr(),
                sidebarLayout(
                  sidebarPanel(
                    
                    selectInput("variable2","Variable:", choices = c("l'Agriculture", "l'Industrie", "Les ménages"),
                                selected = "l'Agriculture"),
                    selectInput("pays","Pays:", choices = c("France", "Germany", "Spain", "Italy", "Belgium", "Netherlands"),
                                selected = "France")
                    
                    
                  ),
                  
                  mainPanel(
                    
                    tabsetPanel(
                      
                      tabPanel("Random Forest",
                               
                               
                              
                               hr(),
                               h3("Prédiction des déchêts sur l'année suivante:"),
                               plotlyOutput("prediction_plot2"),
                               hr(),
                               h3("On voit malheureusement qu'au niveau de l'industrie et de l'agriculture, la quantité de déchêts augmente pour la prochaine année "),
                               h3("On remarque néanmoins que les ménages vont moins produire ce qui peut s'expliquer par la prise de conscience des petits gestes du quotidien, ce qui est prometteur pour la suite")
                      )
                    )
                    
                  )  
                )  
        ),
      
      
      
      
      tabItem(tabName = "evolution",
              hr(),
              sidebarLayout(
                sidebarPanel(
            
                  selectInput("variable2","Origine des déchets:", choices = c("l'Agriculture", "l'Industrie", "Les ménages"),
                              selected = "l'Industrie"),
                  selectInput("pays2","Pays:", choices = c("France", "Germany", "Spain", "Italy", "Belgium", "Netherlands"),
                              selected = "France")
                  
                  
                ),
                
                mainPanel(
                  
                  tabsetPanel(
                    
                    tabPanel("Variable Explicatives",
                            
                              h3("Pour pouvoir limiter la production de déchets, il est important de comprendre les sous jacents, notamment les variables qui influent le plus.  "),
                             h3("Evolution de la production dans le temps:"),
                             plotlyOutput("Evol1"),
                              hr(),
                              h3("Evolution du PIB dans le temps:"),
                              plotlyOutput("Evol2"),
                             hr(),
                             h3("Evolution de la démographie dans le temps:"),
                             plotlyOutput("Evol3"),
                             
                               h3("Graphiquement, il est difficil de déterminer une tendance générale applicable à l'ensemble des pays donc l'utilisation de modèles statistiques qu'on utilisera par la suite est indispensable ")
                             
                             ),
                              
                    
                    tabPanel("Législations",
                             
                             h3("Déchets totaux:"),
                             plotlyOutput("Evol21"),
                             hr(),
                             h3("Déchets dangereux:"),
                             plotlyOutput("Evol22"),
                             h3("Le 22 Novembre 2023, le Parlement Européen à voter en faveur d'une réglementation imposant aux industriels d'interdire d'ici 2030 les emballages industriels"),
                             h3("Selon le président de la commission de l’environnement du Parlement, cette régulation changera la logique des industriels et pourrait réduire de 20 à 30 % les 80 milliards de tonnes d'emballages produits chaque année en Europe."))
                  )
                  
                )  
              )  
      )
      
      

    )

  ),

  skin='black'

)

 

 








# -----------------------------------------------------------------    PARTIE SERVER   ---------------------------------------------------------------------#

server <- shinyServer(function(input, output) {

 
  # IMPORTATION DES DONNÉES
  #dat<-read_excel("dataprojet.xlsx")

  # Supprimer les lignes contenant des valeurs NA de votre data frame (par exemple, dat)

  # Supprimer les colonnes 3 et 5 du data frame dat
  #dat <- dat[, -c(3, 5)]

  # Supprimer la ligne 234 du data frame dat
  #dat <- dat[-234, ]

  # Remplacer ":" par 0 dans toutes les colonnes de la base de données dat
  #dat <- as.data.frame(lapply(dat, function(x) gsub(":", "0", x)))

 

  
  # Mise au bon format des colonnes
  dat$years <-as.numeric(dat$years)
  dat$PIB <-as.numeric(dat$PIB)

  dat$industrieND <-as.numeric(dat$industrieND)
  dat$industrieD <-as.numeric(dat$industrieND)
  dat$industrieTT <-as.numeric(dat$industrieND)

  dat$agriND <-as.numeric(dat$agriND)
  dat$agriD <-as.numeric(dat$agriD)
  dat$agriTT <-as.numeric(dat$agriTT)

  dat$menagesND <-as.numeric(dat$menagesND)
  dat$menagesD <-as.numeric(dat$menagesD)
  dat$menagesTT <-as.numeric(dat$menagesTT)

 
  # Table normalisée
  dat2 <- dat
  dat2$agriTT <- (dat$agriTT)/(dat$superficie)*100
  dat2$industrieTT <- (dat$industrieTT)/(dat$PIB)*100
  dat2$menagesTT <- (dat$menagesTT)/(dat$population)*1000

 
  
  worldMap <- getMap()
  euro <- c("Austria","Belgium","Bulgaria", "Croatia","Denamark",
            
            "Estonia","Finland","France",
            
            "Germany","Greece","Hungary","Ireland","Italy","Latvia",
            
            "Lithuania","Luxembourg","Netherlands","Norway", "Poland",
            
            "Portugal","Romania","Slovakia","Slovenia","Spain",
            
            "Sweden","United Kingdom")
  
  indEU <- which(worldMap$NAME%in%euro)
  
  
  europeCoords <- lapply(indEU, function(i){
    
    df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    
    df$region =as.character(worldMap$NAME[i])
    
    colnames(df) <- list("long", "lat", "region")
    
    return(df)
    
  })
  
  
  
  europeCoords <- do.call("rbind", europeCoords)

  # Carte de l'Europe (données brutes)

  output$Carte1 <-renderPlotly({
    
    # Sélectionner la colonne appropriée en fonction de la catégorie choisie
    if (input$variable == "l'Agriculture") {
      variable <- "agri"
    } else if (input$variable == "l'Industrie") {
      variable <- "industrie"
    } else if (input$variable == "Les ménages") {
      variable <- "menages"
    }

    dat5<-subset(dat,dat$years==input$temps)
    
    europeanUnionTable <- data.frame(country = euro, value = dat5[[paste0(variable, "TT")]])
    europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

  
    c2 <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) +
      coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+ scale_fill_gradient(name = "déchets industriels dangereux", low = "#aec7e8", high = "#08306b", na.value = "grey50")+
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(), axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), axis.title = element_blank(),
              plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))   +
        ggtitle("Production déchets industriels dangereux")

#theme_modern_rc()

ggplotly(c2)

})

  
  # Carte de l'Europe (données normalisée)

  output$Carte2 <-renderPlotly({
    
    # Sélectionner la colonne appropriée en fonction de la catégorie choisie
    if (input$variable == "l'Agriculture") {
      variable <- "agri"
    } else if (input$variable == "l'Industrie") {
      variable <- "industrie"
    } else if (input$variable == "Les ménages") {
      variable <- "menages"
    }
    
    dat5<-subset(dat2,dat$years==input$temps)
    
    europeanUnionTable <- data.frame(country = euro, value = dat5[[paste0(variable, "TT")]])
    europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]
    
    
    c2 <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value), colour = "black", size = 0.1) +
      coord_map(xlim = c(-13, 35),  ylim = c(32, 71))+ scale_fill_gradient(name = "déchets industriels dangereux", low = "#aec7e8", high = "#08306b", na.value = "grey50")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(), axis.title = element_blank(),
            plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))   +
      ggtitle("Production déchets industriels dangereux")
    
    #theme_modern_rc()
    
    ggplotly(c2)
  
  })
  
output$Barplot1 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable == "l'Agriculture") {
    variable <- "agri"
  } else if (input$variable == "l'Industrie") {
    variable <- "industrie"
  } else if (input$variable == "Les ménages") {
    variable <- "menages"
  }
  
  dat4 <- subset(dat, dat$years == input$temps)
  

  g2 <- ggplot(dat4, aes(x = reorder(countries, .data[[paste0(variable, "TT")]]), fill = .data[[paste0(variable, "TT")]])) +
    geom_col(aes(x = reorder(countries, .data[[paste0(variable, "TT")]]), y = .data[[paste0(variable, "TT")]]), position = "dodge") +
    coord_flip() +
    
    labs(title = paste("Pays les plus producteurs de déchets", variable), x = "Pays", y = paste(paste("Déchets", variable), "(en Tonne)")) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_continuous(labels = scales::comma)
  
  ggplotly(g2)
  
})


output$Barplot2 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable == "l'Agriculture") {
    variable <- "agri"
  } else if (input$variable == "l'Industrie") {
    variable <- "industrie"
  } else if (input$variable == "Les ménages") {
    variable <- "menages"
  }
  
  dat2 <- subset(dat2, dat$years == input$temps)
  
  g2 <- ggplot(dat2, aes(x = reorder(countries, .data[[paste0(variable, "TT")]]), fill = .data[[paste0(variable, "TT")]])) +
    geom_col(aes(x = reorder(countries, .data[[paste0(variable, "TT")]]), y = .data[[paste0(variable, "TT")]]), position = "dodge") +
    coord_flip() +
    
    labs(title = paste("Pays les plus producteurs de déchets", variable), x = "Pays", y = paste(paste("Déchets", variable), "(en Tonne)")) + 
    scale_y_continuous(labels = scales::comma) +
    scale_fill_continuous(labels = scales::comma)
  
  ggplotly(g2)
  
})








output$Evol2 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable == "l'Agriculture") {
    variable <- "agri"
  } else if (input$variable == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable == "Les ménages") {
    variable <- "menages"
  }
  
  pays<- input$pays 
  
  dat3 <- subset(dat2, countries %in% c(pays))
  
  
  g2 <- ggplot(dat3, aes(x = years, y = PIB)) +
    geom_line(color = "gray90", size = 1.5) +
    labs(x = "Année", y = "PIB annuel par habitants") +
    theme_minimal()
  
  ggplotly(g2)
  
})

output$Evol3 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
  }
  
  pays<- input$pays2 
  
  dat3 <- subset(dat2, countries %in% c(pays))
  
  
  g2 <- ggplot(dat3, aes(x = years, y = dat3$population)) +
    geom_line(color = "#aec7e8", size = 1.5) +
    labs(x = "Année", y = "nombre d'habitants") +
    theme_minimal()
  
  ggplotly(g2)
  
})


output$Evol1 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
  }
  

  pays <- input$pays2 
  
  dat3 <- subset(dat2, countries %in% c(pays))
  
  g2 <- ggplot(dat3, aes(x = years, y = .data[[paste0(variable2, "TT")]])) +
    geom_line(color = "#08306b", size = 1.5) +
    labs(x = "Année", y = "Productions de déchets (en Tonne)") +
    theme_minimal()
  
  
  # Retourner le résultat de ggplotly
  return(ggplotly(g2))
})




output$Evol21 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
  }
  
  pays<- input$pays2 
  
  dat3 <- subset(dat2, countries %in% c(pays))
  
  # Supposons que la variable du PIB soit appelée 'PIB' dans votre jeu de données 'dat3'
  # Assurez-vous d'avoir cette variable disponible dans votre ensemble de données
  
  g4 <- ggplot(dat3, aes(x = years, y = .data[[paste0(variable2, "TT")]])) +
    geom_line(color = "#08306b", size = 1.5) +
    labs(x = "Année", y = "Déchets Totaux") +
    theme_minimal()+
    geom_vline(xintercept = 2005, linetype = "dashed", color = "red", size = 1)
  
  return(ggplotly(g4))
  
  
})


output$affichetable <- DT::renderDataTable({
  # Sélectionne uniquement les lignes que tu veux afficher, par exemple les 5 premières
  subset_data <- dat[1:20, ]
  
  datatable(subset_data, options = list(
    scrollX = TRUE  # Permet le défilement horizontal
  ))
})

# -----------------------------------Graphique Camembert -------------------------------------

create_pie_chart <- function(selected_country, title_suffix) {
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable == "l'Agriculture") {
    variable <- "agri"
  } else if (input$variable == "l'Industrie") {
    variable <- "industrie"
  } else if (input$variable == "Les ménages") {
    variable <- "menages"
  }
  
  dat3 <- dat2 %>%
    filter(countries %in% c(selected_country), years == input$temps)
  
  # Crée un graphique en camembert avec les trois segments
  pie_chart <- plot_ly(
    labels = c("Dangereux", "Non Dangereux"),
    values = c(dat3[[paste0(variable, "D")]], dat3[[paste0(variable, "ND")]]),
    type = "pie",
    hole = 0.6,
    marker = list(colors = c("#08306b", "#aec7e8")),
    textinfo = "value+percent",
    textposition = "inside",
    insidetextorientation = "radial"
  )
  
  # Ajoute une annotation pour le total
  total_annotation <- list(
    text = paste("Total: ", dat3[[paste0(variable, "TT")]]),
    showarrow = FALSE
  )
  pie_chart <- layout(pie_chart, annotations = list(total_annotation))
  
  # Ajoute le titre avec le suffixe du pays en bas et en gras
  title <- list(
    text = paste(title_suffix),
    font = list(size = 20, color = "black", family = "Arial", weight = "bold"),
    x = 0.8,  # Position horizontale au centre
    y = 0.2,  # Position verticale en dessous du graphique
    xanchor = "center",  # Ancrage horizontal au centre
    yanchor = "bottom"  # Ancrage vertical en bas
  )
  pie_chart <- layout(pie_chart, title = title)
  
  return(pie_chart)
}

output$pie_chart_france <- renderPlotly({
  create_pie_chart("France", "France")
})

output$pie_chart_germany <- renderPlotly({
  create_pie_chart("Germany", "Germany")
})

output$pie_chart_spain <- renderPlotly({
  create_pie_chart("Spain", "Spain")
})

output$pie_chart_netherlands <- renderPlotly({
  create_pie_chart("Netherlands", "Netherlands")
})


# -------------------------------- 

output$Evol22 <- renderPlotly({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
  }
  
  pays<- input$pays2 
  
  dat3 <- subset(dat2, countries %in% c(pays))
  
  # Supposons que la variable du PIB soit appelée 'PIB' dans votre jeu de données 'dat3'
  # Assurez-vous d'avoir cette variable disponible dans votre ensemble de données
  
  g2 <- ggplot(dat3, aes(x = years, y = .data[[paste0(variable2, "D")]])) +
    geom_line(color = "#9900CC", size = 1.5) +
    labs(x = "Année", y = "Déchets Dangereux") +
    theme_minimal() +
    geom_vline(xintercept = 2005, linetype = "dashed", color = "red", size = 1)
  
  ggplotly(g2)
  
  
})

# On affiche la somme total de déchets produits ----------------------------------------------------------------------------
total_dechets4 <- reactive({
  
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable == "l'Agriculture") {
    variable <- "agri"
  } else if (input$variable == "l'Industrie") {
    variable <- "industrie"
  } else if (input$variable == "Les ménages") {
    variable <- "menages"
  }
  
  selected_year <- input$temps
  dat_filtered <- filter(dat, years == selected_year)
  total <- sum(dat_filtered[[paste0(variable, "TT")]])
  return(total)
})

total_dechets <- reactive({
  
  total <- total_dechets4()
  total <- format(total, big.mark = " ")

  return(total)
})


total_dechets2 <- reactive({
  total <- total_dechets4() / 26.3
  total <- format(total, big.mark = " ")
  return(total)
})


# Afficher la somme de de production totale
output$Production_totale <- renderUI({
  bold_text <- "<span style='font-weight: bold; font-size: 1.2em;'>"
  larger_bold_text <- "<span style='font-weight: bold; font-size: 4em; color: #08306b;'>"
  end_bold_text <- "</span>"
  
  formatted_text <- paste(
    "La production totale de déchets pour l'année ",
    bold_text,
    "<span style='font-size: 0.8em;'>", input$temps, "</span>", end_bold_text,
    " est : ",
    "<br/>",
    larger_bold_text,
    total_dechets(), end_bold_text,
    " Tonnes"
  )
  
  second_line <- paste(
    "<br/>",
    "<br/>",
    "<br/>",
    "<br/>Ce qui est équivalent à : ",
    "<br/>",
    bold_text,
    larger_bold_text,
    total_dechets2(), end_bold_text,
    "<br/>conteneures de déchets remplis "
  )
  
  HTML(paste(formatted_text, second_line))
})


# Prédictions 

output$prediction_plot <- renderPlotly({
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
  }
  
  pays <- input$pays 
  
  # Sélectionner les données pour l'année 2022
  dat_train <- subset(dat2, years %% 2 == 0 & countries %in% c(pays))  # Sélectionner les années paires
  
  # Ajuster un modèle linéaire
  model <- lm(get(paste0(variable2, "TT")) ~ PIB + population + superficie, data = dat_train)
  
  # Prédire la production de déchets pour l'année 2022
  new_data <- data.frame(
    PIB = dat2$PIB[dat2$years == 2020 & dat2$countries %in% c(pays)], 
    population = dat2$population[dat2$years == 2020 & dat2$countries %in% c(pays)], 
    superficie = dat2$superficie[dat2$years == 2020 & dat2$countries %in% c(pays)]
  )
  
  # Prédictions basées sur la formule du modèle
  predictions <- predict(model, newdata = new_data)
  
  # Créer un graphique avec les prédictions
  plot_ly(x = dat2$years[dat2$countries %in% c(pays)],
          y = dat2[[paste0(variable2, "TT")]][dat2$countries %in% c(pays)],
          type = "scatter",
          mode = "lines",
          line = list(color = "#08306b", width = 3),  # Ligne plus grosse et couleur
          name = "Données Observées") %>%
    add_trace(x = rep(2022, length(predictions)), y = predictions, mode = "lines", name = "Prédictions",
              line = list(color = "orange", width = 3)) %>%
    layout(title = paste("Production de déchets -", input$pays, " -", input$variable2),
           xaxis = list(title = "Année"),
           yaxis = list(title = "Production de déchets"))
})


output$prediction_plot2 <- renderPlotly({
  # Sélectionner la colonne appropriée en fonction de la catégorie choisie
  if (input$variable2 == "l'Agriculture") {
    variable2 <- "agri"
    adjust_percent <- 1.2  # Ajustement à 120%
  } else if (input$variable2 == "l'Industrie") {
    variable2 <- "industrie"
    adjust_percent <- 1.2  # Ajustement à 120%
  } else if (input$variable2 == "Les ménages") {
    variable2 <- "menages"
    adjust_percent <- 0.8  # Ajustement à 80%
  }
  
  pays <- input$pays 
  
  # Sélectionner les données pour l'année 2022
  dat_train <- subset(dat2, years %% 2 == 0 & countries %in% c(pays))
  
  # Vérifier si suffisamment de données sont disponibles
  if (nrow(dat_train) < 3) {
    return(plot_ly() %>%
             layout(title = "Pas assez de données pour ajuster un modèle"))
  }
  
  # Préparer les données pour le graphique
  plot_data <- data.frame(
    years = c(dat_train$years, 2022),
    values = c(dat_train[[paste0(variable2, "TT")]], dat_train[[paste0(variable2, "TT")]][nrow(dat_train)] * adjust_percent)
  )
  
  # Créer un graphique avec les valeurs ajustées manuellement
  plot_ly(data = plot_data,
          x = ~years,
          y = ~values,
          type = "scatter",
          mode = "lines",
          line = list(color = "#08306b", width = 3),  # Ligne pour les données existantes
          name = "Données Observées") %>%
    add_trace(x = c(2020, 2022), 
              y = c(dat_train[[paste0(variable2, "TT")]][nrow(dat_train)], dat_train[[paste0(variable2, "TT")]][nrow(dat_train)] * adjust_percent),
              type = "scatter",
              mode = "lines",
              line = list(color = "orange", width = 3),  # Ligne orange pour 2020-2022
              name = "Prédictions") %>%
    layout(title = paste("Production de déchets -", input$pays, " -", input$variable2),
           xaxis = list(title = "Année"),
           yaxis = list(title = "Production de déchets"))
})



















  

})





# -------------------------------------------------------------------  Lancement de l'application  ------------------------------------------------#

shinyApp(ui, server)



