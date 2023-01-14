library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
source("global.R")

shinyUI(
  dashboardPage(title = "Data Mining", skin = "black",
                dashboardHeader(
                  dropdownMenu(type="message"
                    
                  ),
                  dropdownMenu(type="notification"
                    
                  ),
                  dropdownMenu(type="tasks"
                               
                  )
                ),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Accueil", tabName = "accueil", icon = icon("home")),
                    menuItem("Description", tabName = "desc"),
                    menuItem("Données", tabName = "data", icon = icon("database")),
                    menuItem("Analyse Statistique", tabName = "AnaStat", icon = icon("desktop")),
                    menuItem("Classification", tabName="classi", icon = icon("book"),
                             startExpanded = TRUE,
                             menuSubItem("Supervisée", tabName="Csup"),
                             menuSubItem("Non Supeervisée", tabName="CnonSup")
                             ),
                    menuItem("A propos", tabName="apropos", icon = icon('th'))
                  )
                ),
                dashboardBody(themeSelector(),
                  tabItems(
                    tabItem(tabName="accueil",
                            h1("Accueil"),
                            fluidPage(
                              h2(strong("Application")),
                              verbatimTextOutput("consigne")
                            )
                            ),
                    tabItem(tabName="desc",
                            h1("Description"),
                            fluidPage(
                              h3("Description du jeu de données:",strong("Annealing Data")),
                              h4("Auteurs:",strong("David Sterling"),"et",strong("Wray Buntine")),
                              verbatimTextOutput("des")
                            )
                    ),
                    tabItem(tabName="data",
                            h1("Données"),
                            h4("Description des attributs"),
                            box(width = 12,"Description des attributs",verbatimTextOutput("SumD")),
                            box(width = 12,"Valeurs Manquantes",verbatimTextOutput("Notval")),
                            #box(title = "Dimensions", textOutput("dimD")),
                            
                            fluidPage(
                              #radioButtons("sep","seperator", choices = c(Comma=',', Period='.', Tilde="~", minus="-")),
                              checkboxInput("header", "header?"),
                                DTOutput("input_file"),
                              downloadButton("downloadData", "Download Data")
                            )
                    ),
                    tabItem(tabName="AnaStat",
                            h1("Analyse Statistique"),
                            h4("Données utilisées:"),
                            box("Règles d'association", verbatimTextOutput("rul")),
                            sidebarPanel(
                              selectInput('att', 'Attribut', vars),
                              #selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                              #numericInput('clusters2', 'Cluster count', 3, min = 1, max = 9),
                              #verbatimTextOutput('t_hierar')
                            ),
                            mainPanel(
                              plotOutput('hist')
                            )
                            
                    ),
                    tabItem(tabName="Csup",
                            h1("Classification Supervisée"),
                            fluidRow(
                              box(title = "Réseau de neurones",
                                  tabBox(width = 12,
                                         tabPanel(title = "Entraînement",plotOutput("resN_train")),
                                         #tabPanel(title = "Test", plotOutput("resN_test")),
                                         tabPanel(title = "Table de précision", verbatimTextOutput("resN_MC"))
                                  )
                              ),
                              box(title = "Arbre de décision",
                                  tabBox(width = 12,
                                         tabPanel(title = "Entraînement",plotOutput("ad_train")),
                                         #tabPanel(title = "Test", plotOutput("ad_test")),
                                         tabPanel(title = "Matrice de confusion", verbatimTextOutput("ad_MC"),verbatimTextOutput("acc"))
                                  )
                              ),
                              box(title = "Les plus proche voisins",
                                  sidebarPanel(width = 12,
                                               selectInput('xkcol', 'X Variable', vars),
                                               selectInput('ykcol', 'Y Variable', vars, selected = vars[[2]]),
                                               numericInput('Kp', 'Centre', 3, min = 1, max = 9)
                                  ),
                                  tabBox(width = 12,
                                         tabPanel(title = "Entraînement",plotOutput("kppv_train")),
                                         tabPanel(title = "Test", plotOutput("kppv_test")),
                                         tabPanel(title = "Matrice de confusion", verbatimTextOutput("kppv_MC"), verbatimTextOutput("acc2"))
                                  )
                              ),
                              box(title = "Support Vector Machine",
                                  tabBox(width = 12,
                                         tabPanel(title = "Entraînement",plotOutput("svm_train")),
                                         tabPanel(title = "Description", verbatimTextOutput("svm_test")),
                                         tabPanel(title = "Matrice de confusion", verbatimTextOutput("svm_MC"), verbatimTextOutput("acc3"))
                                  )
                              )
                            )
                    ),
                    tabItem(tabName="CnonSup",
                            h1("Classification Non supervisée"),
                            fluidRow(
                              #h2("Approche par les k-means"),
                              pageWithSidebar(
                                headerPanel('k-means'),
                                sidebarPanel(
                                  selectInput('xcol', 'X Variable', vars),
                                  selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                                  numericInput('clusters', 'Nombre de Cluster', 3, min = 1, max = 9)
                                ),
                                mainPanel(
                                  plotOutput('k_means')
                                )
                              ),br()
                            ),
                            fluidRow(
                              #h2("Approche par classification hierarchique"),
                              pageWithSidebar(
                                headerPanel('Hiérarchique'),
                                sidebarPanel(
                                  #selectInput('xcol', 'X Variable', vars),
                                  #selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                                  numericInput('clusters2', 'Cluster count', 3, min = 1, max = 9),
                                  verbatimTextOutput('t_hierar')
                                ),
                                mainPanel(
                                  plotOutput('hierar')
                                )
                              )
                            )
                    ),
                    tabItem(tabName="apropos",
                            h2("END!!!"))
                  )
                )
  )
)
