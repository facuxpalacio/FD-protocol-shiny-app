library(shiny) 
library(shinythemes)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("An eight-step protocol for functional diversity analysis"),
  withMathJax(),
  
  navbarPage("", theme = "bootstrap.css",
             
             tabPanel(icon("home"), 
                      
             p("This application is intended to provide students and researchers with 
             a checklist to maximize methods' reproducibility, comparability, and transparency across 
             trait-based studies. For further details, see: "),
             uiOutput("tab"), 
             style = "background-color:lightblue; border-radius:5px"),
  
             tabPanel("Step 1",
                      helpText("Start with the conceptualization of an ecological question 
                               ingrained in a theoretical framing with a set of hypotheses and 
                               predictions.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step1", "Identify an appropriate research question",
                              choices = c("What is your research question?",
                                          "Indicate your main hypotheses and predictions"))),
             
             
             tabPanel("Step 2",
                      helpText("Choose an appropriate sampling or experimental design, along with the 
                               scale of analysis and the study organisms and units (populations, 
                               species, communities) selected to answer the research question.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step2", "Identify an appropriate experimental or sampling design",
                              choices = c("What is(are) your scale(s) of analysis?",
                                          "What is your target ecological unit?",
                                          "Did you perform a power analysis?",
                                          "Did you preregister?"))),
             
             tabPanel("Step 3",
                      sidebarLayout(
                        sidebarPanel(
                      helpText("Collect occurrence data and build a matrix of", em("S"),
                               "sampling units \\(\\times\\)", em("N"), "taxa.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step3", "Assemble a community data matrix",
                                         choices = c("Indicate the focal taxon/taxa",
                                                     "What is your taxonomic resolution?",
                                                     "Indicate the number of taxa",
                                                     "Report sampling effort",
                                                     "Indicate the number of sampling units",
                                                     "Indicate the occurrence data type"))),
                        
                        mainPanel(
                        # Input: Load your community data
                        fileInput("community_dataset", 
                                           "Load your community data",
                                           accept = c("text/csv", 
                                                      "text/comma-separated-values,text/plain", 
                                                      ".csv")),
                        
                               # Input: Checkbox if file has header
                               checkboxInput("header1", "Header", TRUE),
                               
                               # Input: Select separator
                               radioButtons("sep1", "Separator",
                                            c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                            ","),
                      br(),
                        
                      # Output: community dataset (antes tableOutput)
                      textOutput("nrow_community"),
                      textOutput("ncol_community"),
                      
                      br(),
                      br(),
                      
                      dataTableOutput("community_table"))),
                      
             ),
             
             tabPanel("Step 4",
                      sidebarLayout( 
                        sidebarPanel(
                      helpText("Collect functional trait data and build a matrix of", em("N"), 
                      "taxa \\(\\times\\)", em("p"), "traits.",
                      style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step4", "Assemble a trait data matrix",
                                         choices = c("Indicate the number of traits",
                                                     "Indicate the trait data type",
                                                     "Report the sample sizes per species and trait",
                                                     "Are traits response or effect traits?",
                                                     "Are traits soft or hard traits?",
                                                     "Which is the ecological meaning of your traits?",
                                                     "Did you account for intraspecific trait variation?",
                                                     "Indicate the data sources"))),
                        
                        mainPanel(
                        # Input: Load your trait data
                        fileInput("trait_dataset", 
                                           "Load your trait data",
                                           accept = c("text/csv", 
                                                      "text/comma-separated-values,text/plain", 
                                                      ".csv")),
                        
                               # Input: Checkbox if file has header
                               checkboxInput("header2", "Header", TRUE),
                               
                               # Input: Select separator
                               radioButtons("sep2", "Separator",
                                            c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                            ","),
                        
                        br(),
                        
                        # Output: trait dataset (antes tableOutput)
                        textOutput("nrow_traits"),
                        textOutput("ncol_traits"),
                        
                        br(),
                        br(),
                        
                        dataTableOutput("trait_table"))),
                      
                      ),
             
             tabPanel("Step 5",
                      sidebarLayout( 
                        sidebarPanel(
                      helpText("Visually inspect the community and trait matrices to familiarize with your
                               data and deal with any issue therein.",
                               style = "background-color:lightblue; border-radius:5px"),
                      
                      checkboxGroupInput("step5", "Explore and preprocess the data",
                      choices = c("Plot your data!",
                                  "Is there collinearity among traits?",
                                  "Did you transform trait data?",
                                  "Do you have missing data? How did you handle these?",
                                  "Did you account for imperfect detection?")),
                    ),
                      
                    mainPanel(
                      tabsetPanel(             
                        tabPanel("Summary", 
                                 # Ouput: Data summaries
                                 h4("Community data", style="color:blue"),
                                 verbatimTextOutput("summary_community"),
                                 h4("Trait data", style="color:blue"),
                                 verbatimTextOutput("summary_trait")
                                 ),
                        
                        tabPanel("Trait plot",
                                 # Input: Select trait to plot
                                 selectInput("trait", 
                                             label = "Select a functional trait",
                                             choices = NULL),
                                 
                                 # Input: Select species to plot (if ITV is accounted for)
                                 selectInput("species",
                                             label = "Species (if several measurements
                                      per species are available):", choices = NULL),
                                 
                                 # Input: Select type of plot
                                 selectInput("plot.type", 
                                             label = "Plot type:",
                                             list(boxplot = "boxplot", 
                                                  histogram = "histogram",
                                                  density = "density")),
                                 
                                 # Output: Trait plot
                                 h3(textOutput("caption")),
                                 plotOutput("trait_plot")),
                        
                        tabPanel("Collinearity",
                                 # Input: Select two traits to plot
                                 checkboxGroupInput("traits_xy", 
                                             label = "Select two or more 
                                             functional traits",
                                             choices = NULL),
                                 
                                 # Output: scatterplots
                                 plotOutput("scatterplots"),
                                 
                                 # Output: correlation matrix
                                 tableOutput("correlation_matrix"))
                        
                        #tabPanel("Missing data",
                        # )
                            
                          )))
                     ),
  
             tabPanel("Step 6",
                      helpText("Now you can compute functional diversity metrics!",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step6", "Estimate functional diversity measure(s) of interest",
                      choices = c("Identify the level of analysis (alpha, beta, gamma)",
                                  "Did you subset your trait data?",
                                  "Select the appropriate method based on the research question",
                                  "Select the appropriate functional diversity metric",
                                  "Identify the level of functional diversity metric measurement"))),
             
             tabPanel("Step 7",
                      helpText("Fit, interpret, report and validate your statistical model.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step7", "Interpret and validate the results",
                      choices = c("Select an appropriate statistical model or test to answer your research question",
                                  "Report effect sizes, model support and uncertainty",
                                  "Provide a graphical output if needed",
                                  "Did you validate your model and how?"))),
             
             tabPanel("Step 8",
                      helpText("Provide enough data and code detail to allow full reproducibility
                               of your results.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step8", "Ensure reproducibility",
                      choices = c("Report the software, version and packages you used",
                                  "Deposit data in a public repository",
                                  "Provide your code (tidy and clean)")))

    ),
)

######################################################################################


server <- function(input, output, session) {
  url <- a("Palacio", em("et al."), " (2021). A protocol for conducting trait-based 
           analyses and maximizing their reproducibility. Journal name. XX: XX-XX.", 
           href = "https://www.google.com/")
  
  output$tab <- renderUI(tagList(url))
  
  toDisplay <- eventReactive(input$step1, {
    choices <- c("Which is your research question?",
                 "Indicate your main hypotheses and predictions")
    if (all(choices %in% input$step1)){
        return("")
    } else if (choices[1] %in% input$step1) {
      return("Your ecological question...")
    } else if (choices[2] %in% input$step1) {
      return("You should clearly state...")
    } else {}
  })
  output$step1 <- renderText(toDisplay())
  
  community_dataset <- reactive({
    req(input$community_dataset) # require data
    inFile <- input$community_dataset
    df <- read.csv(inFile$datapath, header = input$header1, sep = input$sep1)
    return(df)
  })
  
  # Update traits based on data
  trait_dataset <- reactive({
    req(input$trait_dataset) # require data
    inFile <- input$trait_dataset
    df <- read.csv(inFile$datapath, header = input$header2, sep = input$sep2)
    updateSelectInput(session, inputId = "trait", choices = colnames(df), 
                      selected = "")
    updateSelectInput(session, inputId = "species", choices = c(" ", colnames(df)), 
                      selected = "")
    return(df)
  })
  
  # View data tables
  output$community_table <- renderDataTable(community_dataset(),
                                        options = list(pageLength = 10)) # antes renderTable
  
  output$trait_table <- renderDataTable(trait_dataset(),
                                        options = list(pageLength = 10)) # antes renderTable
  
  # tab "Summary": Create a summary of the data 
  output$summary_community <- renderPrint(summary(community_dataset()))
  
  output$summary_trait <- renderPrint(summary(trait_dataset()))
  
  output$nrow_community <- renderText({
    paste0("Number of sampling units = ", nrow(community_dataset()))
  })
  
  output$ncol_community <- renderText({
    paste0("Number of species = ", ncol(community_dataset()))
  })
  
  output$nrow_traits <- renderText({
    paste0("Number of species/individuals = ", nrow(trait_dataset()))
  })
  
  output$ncol_traits <- renderText({
    paste0("Number of traits = ", ncol(trait_dataset()))
  })

  # tab "Trait plot": Plot univariate graphs
  output$caption <- renderText({
    switch(input$plot.type,
           "boxplot" = "Boxplot",
           "histogram" = "Histogram",
           "density" = "Density plot")
  })
  
  output$trait_plot <- renderPlot({
    sp <- trait_dataset()[, input$species]
    tr <- trait_dataset()[, input$trait]
    
    if(is.null(sp)){ 
      plot.type <- switch(input$plot.type,
                        "histogram" = geom_histogram(color = "black", 
                                                     fill = "white", alpha = 0.5),
                        "density" = geom_density(fill = "blue", alpha = 0.5, 
                                                 col = "blue"),
                        "boxplot" = geom_boxplot())
      ggplot(trait_dataset(), aes(x = tr)) + plot.type
      
      } else {
        
        plot.type <- switch(input$plot.type,
                            "histogram" = geom_histogram(alpha = 0.5),
                            "density" = geom_density(alpha = 0.5),
                            "boxplot" = geom_boxplot())
        
        if(input$plot.type == "boxplot"){
          
          ggplot(trait_dataset(), aes(x = sp, y = tr)) + plot.type
          
          } else {
            
            ggplot(trait_dataset(), aes(x = tr, group = sp, fill = sp)) + plot.type
      }
    }
      
  })
  
  # tab "Collinearity": Plot scatterplots and generate correlation matrix
  # Identify only numeric variables
  numericColumns <- reactive({
    df <- trait_dataset()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy", 
                             choices = numericColumns())
    })
  
  # Print correlation matrix
  output$correlation_matrix <- renderTable({
    cor(trait_dataset()[, input$traits_xy])
  })
  
}
 
shinyApp(ui = ui, server = server) 


