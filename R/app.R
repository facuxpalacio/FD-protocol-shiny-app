library(shiny) #1.6.0
library(shinythemes) #1.2.0
library(ggplot2) #3.3.2
library(GGally) # 2.0.0
library(ggdendro) # 0.1.22
library(shinyjs) #2.0.0
library(pheatmap) #1.0.12
library(vegan) #2.5-6
library(BAT) #2.6.0

ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinytheme("united"),
  titlePanel("An eight-step protocol for functional diversity analysis"),
  
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
                      radioButtons("step1", "Identify whether your work is open-ended or answers a specific research question",
                                   choices = c("My work focuses on a particular question, e.g. Does seed size decrease at higher latitudes?","My work is open-ended e.g. How do abiotic variables shape leaf morphology?")),     
                      fluidRow(
                        column(6, conditionalPanel('input.step1== ["My work focuses on a particular question, e.g. Does seed size decrease at higher latitudes?"]',textInput("hyp", "Hypotheses and predictions", value = "", placeholder = "My ecological question ...")))),
                      fluidRow(column(6, conditionalPanel('input.step1 == ["My work is open-ended e.g. How do abiotic variables shape leaf morphology?"]',
                                                          textInput("nohyp", "Main patterns/variables examined", value = "", placeholder = "Variables under study..."))))),
             
             
             tabPanel("Step 2",
                      helpText("Choose an appropriate sampling or experimental design, along with the 
                               scale of analysis and the study organisms and units (populations, 
                               species, communities) selected to answer the research question.",
                               style = "background-color:lightblue; border-radius:5px"),
                      div(id="step2", "Identify an appropriate experimental or sampling design"),
                                        textInput("scale","What is(are) your scale(s) of analysis?"),
                      textInput("unit","What is your target ecological unit?"),
                      radioButtons("pow", "Did you perform a power analysis?", choices=c("Yes", "No")),
                                   radioButtons("prer", "Did you preregister?", choices=c("Yes", "No"))),
            
              tabPanel("Step 3",
                      withMathJax(),
                       sidebarLayout(
                        sidebarPanel(
                          helpText("Collect occurrence data and build a matrix of", em("S"),
                                   "sampling units \\(\\times\\)", em("N"), "taxa.",
                                   style = "background-color:lightblue; border-radius:5px"),
                          div(id="step3", "Assemble a community data matrix"),
                          textInput("foc","Indicate the focal taxon/taxa"),
                          textInput("reso", "What is your taxonomic resolution?"),
                          textInput("ntax", "Indicate the number of taxa"),
                          textInput("s_eff", "Report sampling effort"),
                          textInput("s_units", "Indicate the number of sampling units"),
                          selectInput("dtyp","Indicate the occurrence data type", 
                                      choices=c("Presence-only","Presence-background", "Presence-absence", "Abundance", "Biomass", "Percent cover"))),
                         
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
                            tabPanel("Data summary", 
                                     # Ouput: Data summaries
                                    h4("Community data", style="color:blue"),
                                     verbatimTextOutput("summary_community"),
                                    h4("Trait data", style="color:blue"),
                                     verbatimTextOutput("summary_trait")
                            ),
                            
                            tabPanel("Community data",
                                     h4("Heatmap", style = "color:blue"),
                                     checkboxInput("LogX", "Log-transform occurrences", value = FALSE),
                                     # Output: heatmap
                                     plotOutput("heatmap_community"),
                                     # Output: rarefaction curves
                                     h4("Rarefaction curves", style = "color:blue"),
                                     plotOutput("rarefaction_curves"),
                                     # Output: histograms
                                     h4("Histograms", style = "color:blue"),
                                     sliderInput("bins",
                                                 "Number of bins:",
                                                 min = 5, max = 20, value = 10),
                                     fluidRow(
                                       column(6,
                                              plotOutput("richness")),
                                       column(6,
                                              plotOutput("prevalence"))
                                     )),
                            
                            tabPanel("Trait plot",
                                     # Input: Select traits to plot
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
                                     # Input: Select traits to plot
                                     checkboxGroupInput("traits_xy1", 
                                                        label = "Select two or more 
                                                        functional traits",
                                                        choices = NULL, inline = TRUE),
                                     
                                     # Output: scatterplots
                                     plotOutput("scatterplots")),
                            
                            tabPanel("Missing data",
                                     # Input: Select traits with missing data
                                     checkboxGroupInput("traits_na",
                                                        label = "You have the following traits with missing data",
                                                        choices = NULL)
                            )
                            
                          )))),
             
             tabPanel("Step 6",
                      sidebarLayout( 
                        sidebarPanel(
                      helpText("Now you can compute functional diversity metrics!",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step6", "Estimate functional diversity measure(s) of interest",
                                         choices = c("Identify the level of analysis (alpha, beta, gamma)",
                                                     "Did you subset your trait data?",
                                                     "Select the appropriate method based on the research question",
                                                     "Select the appropriate functional diversity metric",
                                                     "Identify the level of functional diversity metric measurement")),
                      
                      ),
                      
                      mainPanel(
                        tabsetPanel(                            
                          tabPanel("Richness",
                                   # Input: Select traits to plot
                                   fluidRow(
                                     column(4,
                                            checkboxGroupInput("traits_xy2", 
                                                               label = "Select two or more 
                                                               functional traits",
                                                               choices = NULL),
                                   
                                   # Inputs: dendrogram arguments
                                            radioButtons("dist.metric",
                                                         label = "Dissimilarity metric",
                                                         choices = c("Euclidean" = "euclidean", 
                                                         "Manhattan" = "manhattan", 
                                                         "Gower" = "gower", 
                                                         "Mahalanobis" = "mahalanobis")),
                                            radioButtons("cluster.method",
                                                         label = "Clustering method",
                                                         choices = c("Single" = "single", 
                                                                     "Complete" = "complete",
                                                                     "Average" = "average",
                                                                     "Ward" = "ward.D2")),
                                   ),
                                   
                                   # Output: dendrogram and functional richness
                                     column(8,
                                            plotOutput("dendrogram"),
                                            tableOutput("FRic_table"))
                                  )),
                          
                          tabPanel("Evenness",
                                   
                                   ),
                          
                          tabPanel("Regularity",
                                   
                                   ))))
                      
                      ),
                                   
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
                                                     "Provide your code (tidy and clean)"))),
             div(
               id = "form",
               actionButton("submit", "Submit", class = "btn-primary"),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks for creating your protocol! See the output folder for your filled form")
               )
             )  )
             
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
  
  # Tab "Summary": Create a summary of the data 
  output$summary_community <- renderPrint(summary(community_dataset()))
  
  output$summary_trait <- renderPrint(summary(trait_dataset()))
  
  output$nrow_community <- renderText({
    paste0("Number of sampling units = ", nrow(community_dataset()))
  })
  
  output$ncol_community <- renderText({
    paste0("Number of species = ", ncol(community_dataset()))
  })
  
  output$nrow_traits <- renderText({
    paste0("Number of species or individuals = ", nrow(trait_dataset()))
  })
  
  output$ncol_traits <- renderText({
    paste0("Number of traits = ", ncol(trait_dataset()))
  })
  
  # Tab "Community data": Heatmap, rarefaction curves and histograms
  output$heatmap_community <- renderPlot({
    if(input$LogX == TRUE){
    pheatmap(log(community_dataset() + 1))
    } else {
      pheatmap(community_dataset())
      }
    })
  
  output$rarefaction_curves <- renderPlot({
    raref.curve <- rarecurve(community_dataset())
    names(raref.curve) <- paste("site", 1:nrow(community_dataset()), 
                                sep = "")
    
    list.long <- mapply(FUN = function(x, y) {
      mydf <- as.data.frame(x)
      colnames(mydf) <- "value"
      mydf$site <- y
      mydf$subsample <- attr(x, "Subsample")
      mydf
    }, x = raref.curve, y = as.list(names(raref.curve)), SIMPLIFY = FALSE)
    
    xy <- do.call(rbind, list.long)
    
    ggplot(xy, aes(x = subsample, y = value, color = site)) +
      theme_bw() +
      scale_color_discrete() +
      geom_line(size = 0.8) +
      xlab("Sample size") + ylab("Species richness")
  })
  
  output$richness <- renderPlot({
    nspp <- data.frame(richness = rowSums(community_dataset()))
    ggplot(data = nspp, aes(x = richness)) + 
      geom_histogram(color = "black", fill = "white", bins = input$bins) +
      xlab("Species richness") + ylab("Frequency")
  })
  
  output$prevalence <- renderPlot({
    PA.comm <- 1*(community_dataset()>0)
    nsites <- nrow(PA.comm)
    abundance <- colSums(PA.comm)
    prev <- data.frame(prevalence = abundance/nsites)
    ggplot(data = prev, aes(x = prevalence)) + 
      geom_histogram(color = "black", fill = "white", bins = input$bins) + 
      xlab("Prevalence") + ylab("Frequency")
  })
  
  # Tab "Trait plot": Plot univariate graphs
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
  
  # Tab "Collinearity": Plot scatterplots and generate correlation matrix
  # Identify only numeric variables
  numericColumns <- reactive({
    df <- trait_dataset()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy1", 
                             choices = numericColumns())
  })
  
  # Identify variables with missing values
  NAcolumns <- reactive({
    df <- trait_dataset()
    colnames(df)[colSums(is.na(df)) > 0]
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_na", 
                             choices = NAcolumns())
  })
  
  
  # Print scatterplot matrix + correlations
  output$scatterplots <- renderPlot({
    my_fn <- function(data, mapping, ...){
      p <- ggplot(data = trait_dataset(), mapping = mapping) + 
        geom_point(alpha = 0.5, size = 2) + 
        geom_smooth(method = loess, fill = "blue", color = "blue", ...)
      p
    }
    
    ggpairs(trait_dataset()[, input$traits_xy1], 
            lower = list(continuous = my_fn))
  })
  
  ### Tab "Richness": plot functional dendrogram and compute FRic
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy2", 
                             choices = numericColumns())
  })
  
  output$dendrogram <- renderPlot({
    traits <- trait_dataset()
    rownames(traits) <- traits[, 1]
    dist.matrix <- vegdist(traits[, input$traits_xy2], 
                           method = input$dist.metric)
    cluster <- hclust(dist.matrix, method = input$cluster.method)
    ggdendrogram(cluster, rotate = TRUE, theme_dendro = FALSE)
  })
  
  output$FRic_table <- renderTable({
    dist.matrix <- vegdist(trait_dataset()[, input$traits_xy2], 
                           method = input$dist.metric)
    alpha(comm = community_dataset(), tree = hclust(dist.matrix, 
                                                    method = input$cluster.method))
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  saveData <- function(data) {
    fileName <- sprintf("FDprotocol_%s.csv",
                        humanTime())
    ##EJH set quote to FALSE because the commas within the output fields were dividing things across cells due to csv format
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = FALSE)
  }
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::show("thankyou_msg")
    saveData(formData())
  })
}

fieldsAll <- c("step1", "step2", "step3", "step4", "step5", "step6", "step7", "step8")
responsesDir <- file.path("../output")
epochTime <- function() {
  as.integer(Sys.time())
}
humanTime <- function() format(Sys.time(), "%Y%m%d")



shinyApp(ui = ui, server = server) 


