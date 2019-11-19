
#TITLE
#         Analyse des espérences de vies et moyennes d'ages des différents pays du monde 
#               en fonction de leur accés aux services sanitaires et à l'eau potable

#CONTEXTE

# De nos jours, les immenses avancées technologiques et techniques dans de nombreux domaines
# tel que le transport, la santé, l'énergie, l’informatique.... 
# Nous permettent d’améliorer notre niveau et notre confort de vie, malgrès cela de nombreuses 
# inégalitées subsistent sur notre planète et de nombreux pays ne bénéficient pas de ces avancées.
# En effet de nombreux pays majoritairement sur le continent Africain ne bénéficie même pas des 
# infrastructures élémentaire tel que l'accès à l’eau potable, aux soins, ou à des services d’assainissements.
# 
# A travers l’étude de données que j’ai effectuée j’ai souhaité mettre en évidence ces inégalités de moyens 
# qu’il y’a entre les différents pays et les conséquences direct que celles-ci engendrent.


#BASE DE DONEE

#Source : https://www.gapminder.org/data/

#Descriptifs bases de données :
# Moyenne d'âge par pays et par années 
# Espérance de vie par pays et par années 
# Taux d'accès à l’eau potable par pays et par années 
# Taux d'accès à des services d'assainissements par pays et par années 

#Installation des packages 

install.packages('useful')
install.packages("plotly")
install.packages("shinydashboard")
install.packages(c('maps','mapdata','ggplot2'))


library(tidyverse)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plyr)
library(plotly)
library(mapdata)
library(maps)
library(useful)
library(gapminder)


#Lecture des csv

fichier1 <- file.choose("fichier1.csv")
life_expectancy_years <- read.csv(fichier1)

fichier2 <- file.choose("fichier2.csv")
water_acces <- read.csv(fichier2)

fichier3 <- file.choose("fichier3.csv")
sanitation_acces <- read.csv(fichier3)

fichier4 <- file.choose("fichier4.csv")
median_age <- read.csv(fichier4)


#Fuison des datasets afin de les mettre en relation les unes aux autres 

merge_expectancy_water_acces <- merge(life_expectancy_years, water_acces, by = "country")
merge_expectancy_sanitation_acces<- merge(life_expectancy_years, sanitation_acces, by = "country")

merge_median_age_water_acces <- merge(median_age, water_acces, by = "country")
merge_median_age_sanitation_acces <- merge(median_age, sanitation_acces, by = "country")

#création des dataframes pour tracer les évoulutions des différents indices  

life_expectancy_years_2 <- life_expectancy_years[-1]
life_expectancy_years_mean <- colMeans (life_expectancy_years_2, na.rm = TRUE )
life_expectancy_years_mean <- data.frame(life_expectancy_years_mean)

median_age_2 <- median_age[-1]
median_age_mean <- colMeans (median_age_2, na.rm = FALSE )
median_age_mean <- data.frame(median_age_mean)

water_acces_2 <- water_acces[-1]
water_acces_mean <- colMeans (water_acces_2, na.rm = TRUE )
water_acces_mean <- data.frame(water_acces_mean)

sanitation_acces_2 <- sanitation_acces[-1]
sanitation_acces_mean <- colMeans (sanitation_acces_2, na.rm = TRUE )
sanitation_acces_mean <- data.frame(sanitation_acces_mean)


######### Création des graphes #########

#----------------graphes evolution------------------
graphe_evolution_life_expectancy_years <- ggplot(life_expectancy_years_mean, aes(x=1800:2018, y=life_expectancy_years_mean)) + geom_line()  + 
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy in terms of years")

graphe_evolution_median_age_mean <- ggplot(median_age_mean, aes(x=1950:2100, y=median_age_mean)) + geom_point()  + 
  labs(x = "Year",
       y = "Median age",
       title = "Median age in terms of years")

graphe_evolution_water_acces_mean <- ggplot(water_acces_mean, aes(x=2000:2015, y=water_acces_mean)) + geom_line()  + 
  labs(x = "Year",
       y = "Water acces (%)",
       title = "Life expectancy in terms of years")


graphe_evolution_sanitation_acces_mean <- ggplot(sanitation_acces_mean, aes(x=2000:2015, y=sanitation_acces_mean)) + geom_line()  + 
  labs(x = "Year",
       y = "Sanitation acces (%)",
       title = "Life expectancy in terms of years")


#graphes comparaison 

#------------acces water / life expectancy-------------

graphe_acces_water_expectancy_2005 <- ggplot(merge_expectancy_water_acces, aes(x=X2005.y, y=X2005.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = "life expectancy years",
       title = "Life expectancy years in terms of acces water (%) in 2005")
graphe_acces_water_expectancy_2010 <- ggplot(merge_expectancy_water_acces, aes(x=X2010.y, y=X2010.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = "life expectancy years",
       title = "Life expectancy years in terms of acces water (%) in 2010")

graphe_acces_water_expectancy_2015 <- ggplot(merge_expectancy_water_acces, aes(x=X2015.y, y=X2015.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = "life expectancy years",
       title = "Life expectancy years in terms of acces water (%) in 2015")


#------------acces sanitation / life expectancy-------------

graphe_acces_sanitation_expectancy_2005 <- ggplot(merge_expectancy_sanitation_acces, aes(x=X2005.y, y=X2005.x)) + 
  geom_line(linetype="dashed", color='red') +
  labs(x = "Acces sanitation",
       y = "Life expectancy years",
       title = "Life xpectancy in terms of acces sanitation (%)")



graphe_acces_sanitation_expectancy_2010 <- ggplot(merge_expectancy_sanitation_acces, aes(x=X2010.y, y=X2010.x)) + 
  geom_line(linetype="dashed", color='red') +
  labs(x = "Acces sanitation",
       y = "Life expectancy years",
       title = "Life xpectancy in terms of acces sanitation (%)")

graphe_acces_sanitation_expectancy_2015 <- ggplot(merge_expectancy_sanitation_acces, aes(x=X2015.y, y=X2015.x)) + 
  geom_line(linetype="dashed", color='red') +
  labs(x = "Acces sanitation",
       y = "Life expectancy years",
       title = "Life xpectancy in terms of acces sanitation (%)")



#------------acces water / Median age-------------


graphe_water_acces_median_age_2015 <- ggplot(merge_median_age_water_acces, aes(x=X2015.y, y=X2015.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = " Median age",
       title = "Median age in terms of acces water (%)")


graphe_water_acces_median_age_2010 <- ggplot(merge_median_age_water_acces, aes(x=X2010.y, y=X2010.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = " Median age",
       title = "Median age in terms of acces water (%)")

graphe_water_acces_median_age_2005 <- ggplot(merge_median_age_water_acces, aes(x=X2005.y, y=X2005.x)) + geom_point()  + 
  labs(x = "Acces Water",
       y = " Median age",
       title = "Median age in terms of acces water (%)")


#------------acces sanitation / Median age-------------

graphe_sanitation_acces_median_age_2005 <- ggplot(merge_median_age_water_acces, aes(x=X2005.y, y=X2005.x)) + 
  geom_line(linetype="dashed", color='red')  + 
  labs(x = "Acces sanitation",
       y = "Median age",
       title = "Median age in terms of acces sanitation (%)")

graphe_sanitation_acces_median_age_2010 <- ggplot(merge_median_age_water_acces, aes(x=X2010.y, y=X2010.x)) + 
  geom_line(linetype="dashed", color='red')  + 
  labs(x = "Acces sanitation",
       y = "Median age",
       title = "Median age in terms of acces sanitation (%)")

graphe_sanitation_acces_median_age_2015 <- ggplot(merge_median_age_water_acces, aes(x=X2015.y, y=X2015.x)) + 
  geom_line(linetype="dashed", color='red')  + 
  labs(x = "Acces sanitation",
       y = "Median age",
       title = "Median age in terms of acces sanitation (%)")


#------------Graphes synthése-------------


graphe_synthesis_median_age <- ggplot(median_age, aes(x=X2015)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(median_age$X2015)), color="blue", linetype="dashed", size=1)+
  ggtitle(" ") +
  xlab("Median age") +
  ylab("density")

graphe_synthesis_life_expectancy_years <- ggplot(life_expectancy_years, aes(x=X2015)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(life_expectancy_years$X2015)), color="blue", linetype="dashed", size=1)+
  ggtitle(" ") +
  xlab("Life expectancy years") +
  ylab("density")

graphe_synthesis_water_acces <- ggplot(water_acces, aes(x=X2015)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(water_acces$X2015)), color="blue", linetype="dashed", size=1)+
  ggtitle(" ") +
  xlab("Water acces") +
  ylab("density")

graphe_synthesis_sanitation_acces <- ggplot(sanitation_acces, aes(x=X2015)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + 
  geom_vline(aes(xintercept=mean(sanitation_acces$X2015)), color="blue", linetype="dashed", size=1)+
  ggtitle("") +
  xlab("Sanitation acces") +
  ylab("density")

#Creation de dictionnaire pour la gestion de l'affichage

dict_graph_acces_water_expectancy <- list(graphe_acces_water_expectancy_2005, graphe_acces_water_expectancy_2010, graphe_acces_water_expectancy_2015)
names(dict_graph_acces_water_expectancy) <- c("graphe_acces_water_expectancy_2005",
                                              "graphe_acces_water_expectancy_2010",
                                              "graphe_acces_water_expectancy_2015")

dict_graph_acces_sanitation_expectancy <- list(graphe_acces_sanitation_expectancy_2005, graphe_acces_sanitation_expectancy_2010,graphe_acces_sanitation_expectancy_2015)
names(dict_graph_acces_sanitation_expectancy) <- c("graphe_acces_sanitation_expectancy_2005",
                                                   "graphe_acces_sanitation_expectancy_2010",
                                                   "graphe_acces_sanitation_expectancy_2015")

dict_graph_evolution_expectancy <- list(graphe_evolution_life_expectancy_years,graphe_evolution_water_acces_mean,graphe_evolution_sanitation_acces_mean)
names(dict_graph_evolution_expectancy) <- c("graphe_evolution_life_expectancy_years",
                                            "graphe_evolution_water_acces_mean",
                                            "graphe_evolution_sanitation_acces_mean")

dict_graph_acces_water_median <- list(graphe_water_acces_median_age_2005, graphe_water_acces_median_age_2010, graphe_water_acces_median_age_2015)
names(dict_graph_acces_water_median) <- c("graphe_water_acces_median_age_2005",
                                          "graphe_water_acces_median_age_2010",
                                          "graphe_water_acces_median_age_2015")

dict_graph_acces_sanitation_median <- list(graphe_sanitation_acces_median_age_2005, graphe_sanitation_acces_median_age_2010,graphe_sanitation_acces_median_age_2015)
names(dict_graph_acces_sanitation_median) <- c("graphe_sanitation_acces_median_age_2005",
                                               "graphe_sanitation_acces_median_age_2010",
                                               "graphe_sanitation_acces_median_age_2015")

dict_graph_evolution_median <- list(graphe_evolution_median_age_mean,graphe_evolution_water_acces_mean,graphe_evolution_sanitation_acces_mean)
names(dict_graph_evolution_median) <- c("graphe_evolution_median_age_mean",
                                        "graphe_evolution_water_acces_mean",
                                        "graphe_evolution_sanitation_acces_mean")

dict_graph_synthesis <- list(graphe_synthesis_median_age, graphe_synthesis_life_expectancy_years, graphe_synthesis_water_acces, graphe_synthesis_sanitation_acces)
names(dict_graph_synthesis) <- c("graphe_synthesis_median_age",
                                 "graphe_synthesis_life_expectancy_years",
                                 "graphe_synthesis_water_acces",
                                 "graphe_synthesis_sanitation_acces")


######### Création de l'interface shiny #########

ui <- dashboardPage(
  #Titre du Dashboard
  dashboardHeader(title = "Quality of life", titleWidth = 250),
  
  #Construction du menu 
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Life expectancy", tabName = "Life_expectancy", icon = icon("Life_expectancy"),
               menuSubItem("Water acces", tabName = "Water_acces_expectancy"),
               menuSubItem("Sanitation acces", tabName = "Sanitation_acces_expectancy"),
               menuSubItem("Evolution expectancy", tabName = "Evolution_expectancy")),
      
      menuItem("Median age",tabName = "Median_age", icon = icon("Median_age"),
               menuSubItem("Water acces", tabName = "Water_acces_median"),
               menuSubItem("Sanitation acces", tabName = "Sanitation_acces_median"),
               menuSubItem("Evolution_median", tabName = "Evolution_median")),
      
      menuItem("Synthesis", tabName = "Synthesis", icon = icon("Synthesis"))
      
    )
  ),
  
  
  dashboardBody(
    
    #création des différents onglets du dashboard
    
    tabItems(
      
      #---------------------------Menu ife expectancy---------------------------
      
      tabItem(tabName = "Water_acces_expectancy",
              h1("Water acces with life expectancy :", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_water_expectancy",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_acces_water_expectancy),
                            selected = names(dict_graph_acces_water_expectancy)[1]
                            
                ),
                plotOutput("g_plot_water_expectancy", height = "600px")
                
              )
              
      ),         
      
      
      tabItem(tabName = "Sanitation_acces_expectancy",
              h1("Sanitation acces with life expectancy :", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_sanitation_expectancy",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_acces_sanitation_expectancy),
                            selected = names(dict_graph_acces_sanitation_expectancy)[1]
                            
                ),
                plotOutput("g_plot_sanitation_expectancy", height = "600px")
                
              )
              
      ),      
      
      tabItem(tabName = "Evolution_expectancy",
              h1("Evolution life expectancy :", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_evolution",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_evolution_expectancy),
                            selected = names(dict_graph_evolution_expectancy)[1]
                ),
                plotOutput("g_plot_evolution_expectancy", height = "600px")
                
              )
              
      ),      
      
      
      #---------------------------Menu Median age---------------------------
      
      tabItem(tabName = "Water_acces_median",
              h1("Water acces with median age:", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_water_median",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_acces_water_median),
                            selected = names(dict_graph_acces_water_median)[1]
                ),
                plotOutput("g_plot_water_median", height = "600px")
                
              )
              
      ),
      
      tabItem(tabName = "Sanitation_acces_median",
              h1("Sanitation acces with median age :", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_sanitation_median",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_acces_sanitation_median),
                            selected = names(dict_graph_acces_sanitation_median)[1]
                            
                ),
                plotOutput("g_plot_sanitation_median", height = "600px")
                
              )
              
      ),      
      
      tabItem(tabName = "Evolution_median",
              h1("Evolution median age:", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_evolution_median",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_evolution_median),
                            selected = names(dict_graph_evolution_median)[1]
                            
                ),
                plotOutput("g_plot_evolution_median", height = "600px")
                
              )
      ),
      
      #---------------------------Synthesis---------------------------
      
      tabItem(tabName = "Synthesis",
              h1("Synthesis :", align = "center"),
              fluidRow(
                
                selectInput(inputId = "chosen_graph_synthesis",
                            label = "Choose the graph you want to display :",
                            choices = names(dict_graph_synthesis),
                            selected = names(dict_graph_synthesis)[1]
                ),
                plotOutput("g_plot_synthesis", height = "600px")
                
              )
              
      )
      
    )
    
  )
)


######### Création du server shiny #########

server <- function(input, output) {
  
  #---------------------------Menu life expectancy---------------------------
  
  #Affichage du graph Life_expectancy water
  show_graph <- reactive({
    dict_graph_acces_water_expectancy[input$chosen_graph_water_expectancy]
  })
  
  
  output$g_plot_water_expectancy <- renderPlot({
    show_graph()
  })
  
  
  #Affichage du graph Life_expectancy sanitation
  show_graph2 <- reactive({
    dict_graph_acces_sanitation_expectancy[input$chosen_graph_sanitation_expectancy]
  })
  
  
  output$g_plot_sanitation_expectancy <- renderPlot({
    show_graph2()
  })  
  
  #Affichage du graph pour evolution life expectancy
  show_graph3 <- reactive({
    dict_graph_evolution_expectancy[input$chosen_graph_evolution]
  })
  
  
  output$g_plot_evolution_expectancy <- renderPlot({
    show_graph3()
  })    
  
  
  #---------------------------Menu median age---------------------------
  
  #Affichage du graph median water
  show_graph4 <- reactive({
    dict_graph_acces_water_median[input$chosen_graph_water_median]
  })
  
  
  output$g_plot_water_median <- renderPlot({
    show_graph4()
  })
  
  
  #Affichage du graph median sanitation
  show_graph5 <- reactive({
    dict_graph_acces_sanitation_median[input$chosen_graph_sanitation_median]
  })
  
  
  output$g_plot_sanitation_median <- renderPlot({
    show_graph5()
  })  
  
  #Affichage du graph evolution median
  show_graph6 <- reactive({
    dict_graph_evolution_median[input$chosen_graph_evolution_median]
  })
  
  output$g_plot_evolution_median <- renderPlot({
    show_graph6()
  })      
  
  
  #---------------------------Synthesis---------------------------
  
  #Affichage des histogrames synthesis
  show_graph7 <- reactive({
    dict_graph_synthesis[input$chosen_graph_synthesis]
  })
  
  
  output$g_plot_synthesis <- renderPlot({
    show_graph7()
  })
}


######### Lancement shiny #########

shinyApp(ui, server, options = list(height = 300, width = 1000))


#CONCLUSION


#Graphes comparaisons:
# A travers les différents graphiques de comparaisons nous pouvons voir que les pays ayant un faible
# accès aux infrastructures tels que des services d’assainissements ou à l’eau potable, 
# on une espérance de vie de leur population beaucoup plus faible.
# Ce qui influe directement sur la moyenne d'âge de la population dans ce pays.

#Graphes Evolution:
#A travers les diférents graphiques d'évolution nous pouvouns constater que les indices d'accés à l'eau potable 
#ainsi qu'a des services d'assainissements sont en constantes évolution, ce qui montre un aspect général 
#d'amélioration des conditions de vie.
#Par conséquences l'ésperence de vie et la moyenne sont également en évolution casi constante malgrés  
#quelques fluctuations dù a des épisodes historiques tel que les guerres, les épidémies ...

#Synthesis:
#Pour finir ces histogrammes mettent en relief les grandes innégalités entre les différents pays du monde
#nous pouvons y voir un écart immense entre les pays développés et sous développées, de plus les moyennes 
#est nottament pour l'ésperence de vie sont trés basse. 

