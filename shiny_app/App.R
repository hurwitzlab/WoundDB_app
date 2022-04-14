library(shiny)
library(viridis)
library(hrbrthemes)
library(treemapify)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

# ----------------------------------
# load dataset
datasetPat <- read_csv("../datasets/patients_interface_01.04.22.csv") 
control_patients <- datasetPat %>% filter(Disease=="Control")
datasetSamp <- read_csv("../datasets/sample_interface_01.04.22.csv")
# Color palette
hurwitz_colors <- c("#018d97", "#f5811f", "gold", "#61cddc", "#6a6a6a", "tomato4", "tan", "darkslategray1", "gray20", "gray80", "burlywood4", "red", "yellow", "darkgreen", "blue", "purple")


ui <- navbarPage(title = "Wound database",
                 theme = "style/style.css",
                 footer = includeHTML("footer.html"),
                 fluid = TRUE,
                 
                 # ----------------------------------
                 # tab panel 1 - Home
                 tabPanel("Home",
                          includeHTML("home.html"),
                          tags$script(src = "plugins/scripts.js"),
                          tags$head(
                            tags$link(rel = "stylesheet", 
                                      type = "text/css", 
                                      href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                            tags$link(rel = "icon", 
                                      type = "image/png", 
                                      href = "images/logo_icon.png"),
                            tags$style(HTML("hr {border-top: 1px solid #000000;}")
                            ),
                          ),
                 ),
                 # ----------------------------------
                 # tab panel 2 - Patient search
                 tabPanel(title = "Patients",
                         sidebarLayout(fluid=TRUE,
                           sidebarPanel(h2("Selection Patients"),
                                        h4(),
                                        selectInput("project", "Project:",
                                                           c("Gardiner2017" = "Gardiner2017",
                                                             "Jnana2020" = "Jnana2020",
                                                             "Loesche2017" = "Loesche2017",
                                                             "Malone2017a" = "Malone2017a",
                                                             "Malone2017b" = "Malone2017b",
                                                             "Malone2019" = "Malone2019", 
                                                             "Park2019" = "Park2019",
                                                             "Sloan2019" = "Sloan2019"),
                                                           selected=c("Gardiner2017", "Jnana2020",
                                                                      "Loesche2017", "Malone2017a"),
                                                          selectize = TRUE,
                                                          multiple = TRUE),
                                        
                                        
                                        hr(),
                                        h4("Patient characteristics"),
                                        checkboxGroupInput("disease", "Disease status:",
                                                           c("diabetic" = "diabetic",
                                                             "non-diabetic"="non-diabetic",
                                                             "T1D"="T1D",
                                                             "T2D"="T2D"),
                                                           selected=c("diabetes", "T1D",
                                                                      "T2D"),
                                                           inline=TRUE),
                                        checkboxGroupInput("sex", "Sex:",
                                                           c("female" = "F",
                                                             "male"="M",
                                                             "Unknown"="Unknown"),
                                                           selected=c("F", "M"),
                                                           inline=TRUE),
                                        
                                        hr(),
                                        h4("Wound characteristics"),
                                        checkboxGroupInput("wound", "Wound type:",
                                                           c("DFU" = "DFU",
                                                             "DFI"="DFI",
                                                             "non-diabetic wound"="non-diabetic wound"),
                                                           selected=c("DFU", "DFI"),
                                                           inline=TRUE),
                                        checkboxGroupInput("Location", "Wound location:",
                                                           c("foot" = "foot",
                                                             "foot plantar"="foot plantar",
                                                             "heel"="heel",
                                                             "lower-extremity area"="lower-extremity area"),
                                                           selected=c("foot", "foot plantar"),
                                                           inline=TRUE),
                                        checkboxGroupInput("FStatus", "Final wound status:",
                                                           c("unhealed" = "unhealed",
                                                             "healed"="healed",
                                                             "Unknown"="Unknown"),
                                                           selected=c("unhealed", "healed","Unknown"),
                                                           inline=TRUE),
                                        
                                        hr(),
                                        h4("Sampling characteristics"),
                                        sliderInput("slider_Visit", "nb of visits:", min = 1, 
                                                    max = 10, value = c(1, 5)),
                                        sliderInput("slider_Samples", "nb of samples:", min = 1, 
                                                    max = 14, value = c(1, 10)),
                                        
                                        
                                        hr(),
                                        downloadButton("downloadData", "Download dataset"),
                                        h4(),
                                        downloadButton("downloadPhySeq", "Download PhySeq"),
                                        ),
                           
                           
                           mainPanel(width = 8,
                                     fluidRow(
                                       column(5,plotOutput("disease_chart")),
                                       column(5,plotOutput("nb_sample_chart")),
                                     ),
                                     fluidRow(column(10, 
                                                     h2("List of selected patients"),
                                                     DT::dataTableOutput("tableview"),))
                                    ) 
                         ),
                  ),
                # ----------------------------------
                # tab panel 3 - Sample search
                tabPanel(title = "Sample",
                         sidebarLayout(fluid=TRUE,
                                       sidebarPanel(h2("Selection Sample"),
                                                    h4(),
                                                    selectInput("projectS", "Project:",
                                                                c("Gardiner2017" = "Gardiner2017",
                                                                  "Jnana2020" = "Jnana2020",
                                                                  "Loesche2017" = "Loesche2017",
                                                                  "Malone2017a" = "Malone2017a",
                                                                  "Malone2017b" = "Malone2017b",
                                                                  "Malone2019" = "Malone2019", 
                                                                  "Park2019" = "Park2019",
                                                                  "Sloan2019" = "Sloan2019"),
                                                                selected=c("Gardiner2017", "Jnana2020",
                                                                           "Loesche2017", "Malone2017a"),
                                                                selectize = TRUE,
                                                                multiple = TRUE),
                                                    hr(),
                                                    h4("Sampling characteristics"),
                                                    checkboxGroupInput("siteS", "Site of sampling:",
                                                                       c("adjacent" = "adjacent",
                                                                         "contralateral"="contralateral",
                                                                         "wound"="wound"),
                                                                       selected=c("wound", "adjacent"),
                                                                       inline=TRUE),
                                                    checkboxGroupInput("methodS", "Sampling method:",
                                                                       c("swab" = "swab",
                                                                         "tissue"="tissue"),
                                                                       selected=c("swab", "foot tissue"),
                                                                       inline=TRUE),
                                                    checkboxGroupInput("Status", "Current wound status:",
                                                                       c("non-healing" = "non-healing",
                                                                         "healing"="healing",
                                                                         "Unknown"="Unknown"),
                                                                       selected=c("non-healing", "healing"),
                                                                       inline=TRUE),
                                                    checkboxGroupInput("Treat", "Current treatment:",
                                                                       c("Antibiotic" = "Antibiotic",
                                                                         "non-antibiotic"="non-antibiotic",
                                                                         "Unknown"="Unknown"),
                                                                       selected=c("Antibiotic", "non-antibiotic", "Unknown"),
                                                                       inline=TRUE),
                                                    hr(),
                                                    h4("Sequencing characteristics"),
                                                    checkboxGroupInput("SeqTech", "Sequencing method:",
                                                                       c("Illumina" = "Illumina",
                                                                         "Ion Torrent"="Ion Torrent"),
                                                                       selected=c("Illumina", "Ion Torrent"),
                                                                       inline=TRUE),
                                                    hr(),
                                                    downloadButton("downloadSData", "Download dataset"),
                                                    h4(),
                                                    downloadButton("downloadSPhySeq", "Download PhySeq"),
                                       ),
                                       
                                       
                                       mainPanel(width = 8,
                                                 fluidRow(
                                                   column(5,plotOutput("project_chart")),
                                                   column(5,plotOutput("nb_samplePat_chart")),
                                                 ),
                                                 fluidRow(column(10, 
                                                                 h2("List of selected samples"),
                                                                 DT::dataTableOutput("tableSview"),))
                                       ) 
                         ),
                ),

)
                 

# ----------------------------------
# ----------------------------------
# ----------------------------------
# SERVER SIDE
# ----------------------------------
# ----------------------------------

server <- function(input, output) {
  
  
  # tab panel Patient
  # ----------------------------------
  patient_selection <- reactive({
    datasetPat %>%  filter(Project %in% c(input$project)) %>% 
      filter(Disease %in% c(input$disease)) %>%
      filter(Sex %in% c(input$sex)) %>%
      filter(Wound_Type %in% c(input$wound)) %>%
      filter(Location %in% c(input$Location)) %>%
      filter(Final_Status %in% c(input$FStatus)) %>% 
      filter(nb_of_Visits >= input$slider_Visit[1]) %>%
      filter(nb_of_Visits <= input$slider_Visit[2]) %>%
      filter(nb_of_Visits >= input$slider_Samples[1]) %>%
      filter(nb_of_Visits <= input$slider_Samples[2])
  })
  
  control_selection <- reactive({
    control_patients %>%  filter(Project %in% c(input$project))
  })
  
  output$disease_chart <- renderPlot({  
    display_dataset <- patient_selection() %>% group_by(Disease) %>% tally()
    
    display_dataset  %>%  ggplot(aes(x="", y=n, fill=Disease)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()+
      theme(legend.position="bottom")+
      scale_fill_manual(values = hurwitz_colors)+
      ggtitle("Disease of the Patient")
  })
  
  output$nb_sample_chart <- renderPlot({  
    display_dataset <- patient_selection() %>% group_by(Wound_Type, Location) %>% tally()
    
    display_dataset %>% ggplot(aes(x=Wound_Type, y=n, fill=Location)) +
      geom_histogram(stat="identity") +
      coord_flip()+
      theme_minimal()+
      theme(legend.position="bottom",
            axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      scale_fill_manual(values = hurwitz_colors)+
      ggtitle("Number of patients")
  })
  
  
  output$tableview <- renderDataTable({
    patient_selection() %>% select(Project_ID, Subject_ID, Geo_Location, Disease)
  }, escape = FALSE)
    
  # ----------------------------------
    # Download button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("WoundDB-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(patient_selection(), file)
      }
    )
    
    output$downloadPhySeq <- downloadHandler(
      filename = function() {
        paste("ArticleJL-", Sys.Date(), ".RDS", sep="")
      },
      content = function(file) {
        write_rds(article_selection(), file)
      }
    )
    # ----------------------------------
    
    
    
    
    # tab panel Sample
    # ----------------------------------
    sample_selection <- reactive({
      datasetSamp %>% filter(Project %in% c(input$projectS)) %>%
        filter(Site %in% c(input$siteS)) %>% 
        filter(Sampling_Method %in% c(input$methodS)) %>% 
        filter(Status_vs_Last_Time %in% c(input$Status)) %>%
        filter(Seq_Platform %in% c(input$SeqTech))
    })
    
    
    output$project_chart <- renderPlot({  
      display_dataset <- sample_selection() %>% group_by(Project) %>% tally()
      
      display_dataset  %>%  ggplot(aes(x="", y=n, fill=Project)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()+
        scale_fill_manual(values = hurwitz_colors)+
        ggtitle("Project")
    })
    
    output$nb_samplePat_chart <- renderPlot({  
      display_dataset <- sample_selection() %>% group_by(Subject_ID) %>% tally()
      
      display_dataset %>% ggplot(aes(x=n, fill="#018d97")) +
        geom_histogram(binwidth = 5) +
        theme_minimal()+
        theme(legend.position="none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank())+
        scale_fill_manual(values = hurwitz_colors)+
        ggtitle("Number of samples per patients")
    })
    
    
    output$tableSview <- renderDataTable({
      sample_selection() %>% select(Project, Run_ID, Seq_Platform)
    }, escape = FALSE)
    
    
    
    # Download button
    output$downloadSData <- downloadHandler(
      filename = function() {
        paste("WoundDB-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(sample_selection(), file)
      }
    )
    
    
    
    
    # ----------------------------------
  
}

shinyApp(server = server, ui = ui)