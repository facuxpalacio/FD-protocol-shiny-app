library(shiny) 
library(shinythemes)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("An eight-step protocol for functional diversity analysis"),
  
  navbarPage("", theme = "bootstrap.css",
             
             tabPanel(icon("home"), 
                      
             p("Through this application, it is intended to provide students and researchers a 
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
                              choices = c("Which is your research question?",
                                          "Indicate your main hypotheses and predictions"))),
             
             
             tabPanel("Step 2",
                      helpText("Choose an appropriate sampling or experimental design, along with the 
                               scale of analysis and the study organisms and units (populations, 
                               species, communities) selected to answer the research question.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step2", "Identify an appropriate experimental or sampling design",
                              choices = c("Which is your scale(s) of analysis?",
                                          "Which is your ecological unit target?",
                                          "Did you perform a power analysis?",
                                          "Did you preregister?"))),
             tabPanel("Step 3",
                      withMathJax(),
                      helpText("Collect occurrence data and build a matrix of", em("S"),
                               "sampling units \\(\\times\\)", em("N"), "taxa.",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step3", "Assemble a community data matrix",
                                         choices = c("Indicate the focal taxon/taxa",
                                                     "Which is the taxonomic resolution?",
                                                     "Indicate the number of taxa",
                                                     "Report sampling effort",
                                                     "Indicate the number sampling units",
                                                     "Indicate the occurrence data type"))),
             
             tabPanel("Step 4",
                      helpText("Collect functional trait data and build a matrix of", em("N"), 
                      "taxa \\(\\times\\)", em("p"), "traits.",
                      style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step4", "Assemble a trait data matrix",
                                         choices = c("Indicate the number of traits",
                                                     "Indicate the trait data type",
                                                     "Report the sample sizes per species and trait",
                                                     "Are traits response or effect traits?",
                                                     "Are traits soft or hard traits?",
                                                     "Which are the ecological meaning of your traits?",
                                                     "Did you account for intraspecific trait variation?",
                                                     "Indicate the data sources"))),
             
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
                        tabPanel("Dataset",
                                 # Input: Load your own data
                                 fileInput("dataset", 
                                           "Load your community or trait data",
                                           accept = c("text/csv", 
                                                      "text/comma-separated-values,text/plain", 
                                                      ".csv")),
                                 
                                 # Input: Checkbox if file has header
                                 checkboxInput("header", "Header", TRUE),
                                 
                                 # Input: Select separator
                                 radioButtons("sep", "Separator",
                                              c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                              ","),
                                 
                                 # Output: Dataset
                                 tableOutput("trait_table")),
                        
                        tabPanel("Summary", 
                                 # Ouput: Data summary
                                 verbatimTextOutput("summary")),
                        
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
                            
                          )))),
  
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
  
  output$tab <- renderUI({
    tagList(url)
  })
  
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
  output$step1 <- renderText({ 
    toDisplay()
  })
  
  # Update traits based on data
  dataset <- reactive({
    req(input$dataset) # require data
    inFile <- input$dataset
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    updateSelectInput(session, inputId = "trait", choices = colnames(df), 
                      selected = "")
    updateSelectInput(session, inputId = "species", choices = c(" ", colnames(df)), 
                      selected = "")
    return(df)
  })
  
  # tab "Dataset": Generate data table
  output$trait_table <- renderTable({
    dataset()
  })
  
  # tab "Summary": Create a summary of the data 
  output$summary <- renderPrint({
    summary(dataset())
  })

  # tab "Trait plot": Plot univariate graphs
  output$caption <- renderText({
    switch(input$plot.type,
           "boxplot" = "Boxplot",
           "histogram" = "Histogram",
           "density" = "Density plot")
  })
  
  output$trait_plot <- renderPlot({
    sp <- dataset()[, input$species]
    tr <- dataset()[, input$trait]
    
    if(is.null(sp)){ 
      plot.type <- switch(input$plot.type,
                        "histogram" = geom_histogram(color = "black", 
                                                     fill = "white", alpha = 0.5),
                        "density" = geom_density(fill = "blue", alpha = 0.5, 
                                                 col = "blue"),
                        "boxplot" = geom_boxplot())
      ggplot(dataset(), aes(x = tr)) + plot.type
      
      } else {
        
        plot.type <- switch(input$plot.type,
                            "histogram" = geom_histogram(alpha = 0.5),
                            "density" = geom_density(alpha = 0.5),
                            "boxplot" = geom_boxplot())
        
        if(input$plot.type == "boxplot"){
          
          ggplot(dataset(), aes(x = sp, y = tr)) + plot.type
          
          } else {
            
            ggplot(dataset(), aes(x = tr, group = sp, fill = sp)) + plot.type
      }
    }
      
  })
  
  # tab "Collinearity": Plot scatterplots and generate correlation matrix
  # Identify only numeric variables
  numericColumns <- reactive({
    df <- dataset()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy", 
                             choices = numericColumns())
    })
  
  # Print correlation matrix
  output$correlation_matrix <- renderTable({
    cor(dataset()[, input$traits_xy])
  })
  
}
 
shinyApp(ui = ui, server = server) 


