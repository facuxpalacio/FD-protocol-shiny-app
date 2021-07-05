library(shiny) #1.6.0
library(shinydashboard) #0.7.1
library(shinyjs) #2.0.0
library(ggplot2) #3.3.2
library(GGally) # 2.0.0
library(ggdendro) # 0.1.22
library(pheatmap) #1.0.12
library(vegan) #2.5-6
library(alphahull) #2.2
library(BAT) #2.6.0

ui <- dashboardPage(
  #shinyjs::useShinyjs(),
  dashboardHeader(title = "An eight-step protocol for functional diversity analysis"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("home")),
      menuItem("Step 1. Research question", tabName = "step1"),
      menuItem("Step 2. Study design", tabName = "step2"),
      menuItem("Step 3. Community data", tabName = "step3"),
      menuItem("Step 4. Trait data", tabName = "step4"),
      menuItem("Step 5. Explore your data!", tabName = "step5",
               menuSubItem("Data summary", tabName = "datasummary"),
               menuSubItem("Community data", tabName = "communitydata"),
               menuSubItem("Trait plots", tabName = "traitplots"),
               menuSubItem("Collinearity", tabName = "collinearity"),
               menuSubItem("Missing data", tabName = "missingdata")),
      menuItem("Step 6. Functional diversity", tabName = "step6",
               menuItem("Trait space", tabName = "traitspace"),
               menuItem("Richness", tabName = "richness",
                           menuSubItem("Alpha", tabName = "alpharich"),
                           menuSubItem("Beta", tabName = "betarich")),
               menuItem("Regularity", tabName = "regularity",
                           menuSubItem("Alpha", tabName = "alphareg"),
                           menuSubItem("Beta", tabName = "betareg")),
               menuItem("Divergence", tabName = "divergence"),
               menuItem("Similarity", tabName = "similarity"),
               menuItem("Species contributions", tabName = "spcontrib"),
               menuItem("Correlations among metrics", tabName = "corrFD")),
      menuItem("Step 7. Model fit", tabName = "step7"),
      menuItem("Step 8. Reproducibility", tabName = "step8")
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # Tab contents
      tabItem(tabName = "dashboard",
      p("This application is intended to provide students and researchers with 
        a checklist to maximize methods' reproducibility, comparability, and transparency across 
        trait-based studies. For further details, see: "), 
      uiOutput("tab"), 
      style = "background-color:lightblue; border-radius:5px"),
      
      tabItem(tabName = "step1",
      helpText("Start with the conceptualization of an ecological question 
               ingrained in a theoretical framing with a set of hypotheses and 
               predictions.", style = "background-color:lightblue; border-radius:5px"),
      radioButtons("step1", "Identify whether your work is open-ended or answers a specific research question",
                   choices = c("My work focuses on a particular question, e.g. Does seed size decrease at higher latitudes?","My work is open-ended e.g. How do abiotic variables shape leaf morphology?")),     
                      fluidRow(
                        column(6, conditionalPanel('input.step1== ["My work focuses on a particular question, e.g. Does seed size decrease at higher latitudes?"]',textInput("hyp", "Hypotheses and predictions", value = "", placeholder = "My ecological question ...")))),
                      fluidRow(column(6, conditionalPanel('input.step1 == ["My work is open-ended e.g. How do abiotic variables shape leaf morphology?"]',
                                                          textInput("nohyp", "Main patterns/variables examined", value = "", placeholder = "Variables under study..."))))
      ),
             
      tabItem(tabName = "step2",
                      helpText("Choose an appropriate sampling or experimental design, along with the 
                               scale of analysis and the study organisms and units (populations, 
                               species, communities) selected to answer the research question.",
                               style = "background-color:lightblue; border-radius:5px"),
                      div(id="step2", "Identify an appropriate experimental or sampling design"),
                                        textInput("scale","What is(are) your scale(s) of analysis?"),
                      textInput("unit","What is your target ecological unit?"),
                      radioButtons("pow", "Did you perform a power analysis?", choices=c("Yes", "No")),
                                   radioButtons("prer", "Did you preregister?", choices=c("Yes", "No"))
              ),
            
      tabItem(tabName = "step3",
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
                           fluidRow(
                          box(title = "Load your community data", width = 8, height = 300,
                              status = "primary", solidHeader = TRUE,
                          fileInput("community_dataset", 
                                    "",
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
                                       ",")),
                          
                          # Output: community dataset
                          box(title = "Number of sites and species", width = 4, height = 300,
                              status = "warning", solidHeader = TRUE,
                          textOutput("nrow_community"),
                          textOutput("ncol_community"))
                           ),
                          
                          fluidRow(
                          box(title = "Community data", width = 12, height = 300,
                              status = "warning", solidHeader = TRUE,
                          dataTableOutput("community_table"))
                          )
                          
                          ))
                      
             ),
             
      tabItem(tabName = "step4",
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
                          fluidRow(
                            box(title = "Load your trait data", width = 8, height = 300,
                                status = "primary", solidHeader = TRUE,
                          fileInput("trait_dataset", "",
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
                                       ",")
                          ),
                          
                          
                          # Output: trait dataset
                          box(title = "Number of species/individuals and traits", 
                              width = 4, height = 300, status = "warning", solidHeader = TRUE,
                          textOutput("nrow_traits"),
                          textOutput("ncol_traits"))
                          ),
                          
                          box(title = "Trait data", width = 12, height = 300, 
                              status = "warning", solidHeader = TRUE,
                          dataTableOutput("trait_table")))
                        )
                      ),
             
      tabItem(tabName = "step5",
                          helpText("Visually inspect the community and trait matrices to familiarize with your
                               data and deal with any issue therein.",
                                   style = "background-color:lightblue; border-radius:5px"),
                          
                          checkboxGroupInput("step5", "Explore and preprocess the data",
                                             choices = c("Plot your data!",
                                                         "Is there collinearity among traits?",
                                                         "Did you transform trait data?",
                                                         "Do you have missing data? How did you handle these?",
                                                         "Did you account for imperfect detection?"))
              ),
      
      tabItem(tabName = "datasummary",
              
              # Ouputs: Data summaries
              fluidRow(
                box(title = "Community data", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("summary_community")
                  )),
              
              fluidRow(
                box(title = "Trait data", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("summary_trait")
                  ))
              ),
                            
                          
      tabItem(tabName = "communitydata",
              
              fluidRow(
              box(title = "Heatmap", status = "warning", solidHeader = TRUE, width = 6,
              checkboxInput("LogX", "Log-transform occurrences", value = FALSE),
              # Output: heatmap
              plotOutput("heatmap_community")
              ),
                                     
              # Output: rarefaction curves
                box(title = "Rarefaction curves", status = "warning", solidHeader = TRUE, width = ,
                    plotOutput("rarefaction_curves"))
              ),
              
              box(title = "Inputs", status = "primary", solidHeader = TRUE,
                  sliderInput("bins1", "Number of bins:", min = 5, max = 20, value = 10),
              ),
              
              fluidRow(
                # Output: histograms
                box(title = "Histograms", status = "warning", solidHeader = TRUE, width = 12,
                      column(6,
                             plotOutput("richness")),
                      column(6,
                             plotOutput("prevalence")))
                    )
              ),
      
      tabItem(tabName = "traitplots",
              # Input: Select traits to plot
              box(title = "Trait plots", status = "primary", solidHeader = TRUE,
                  selectInput("trait", label = "Select a functional trait",
                              choices = NULL),
                                     
              # Input: Select species to plot (if ITV is accounted for)
                  selectInput("species", label = "Species (if several measurements
                               per species are available):", choices = NULL),
                                     
              # Input: Select type of plot
                  selectInput("plot.type", label = "Plot type:",
                               list(boxplot = "boxplot", 
                                    histogram = "histogram",
                                    density = "density"))),
              
              # Input: histogram inputs
              box(title = "Inputs", status = "primary", solidHeader = TRUE,
                  sliderInput("bins2", "Number of bins:", min = 5, max = 20, value = 10),
              ),
                                     
              # Output: Trait plot
               box(title = textOutput("caption"), status = "warning", solidHeader = TRUE,
                   plotOutput("trait_plot"))
              ),
                            
                          
      tabItem(tabName = "collinearity",
              
              # Input: Select traits to plot
              box(title = "Select two or more functional traits", status = "primary", solidHeader = TRUE,
                  checkboxGroupInput("traits_xy1", label = "", choices = NULL)
                  ),
                                     
              # Output: scatterplots
              fluidRow(
                box(title = "Scatterplots", status = "warning", solidHeader = TRUE, width = 12,
                    plotOutput("scatterplots"))
                  )
              ),
                            
                          
      tabItem(tabName = "missingdata",
              
              # Input: Select traits with missing data
              box(title = "You have the following traits with missing data",
               checkboxGroupInput("traits_na", label = "", choices = NULL))
              ),
                          
             
      tabItem(tabName = "step6",
                      helpText("Now you can compute functional diversity metrics!",
                               style = "background-color:lightblue; border-radius:5px"),
                      checkboxGroupInput("step6", "Estimate functional diversity measure(s) of interest",
                                         choices = c("Identify the level of analysis (alpha, beta, gamma)",
                                                     "Did you subset your trait data?",
                                                     "Select the appropriate method based on the research question",
                                                     "Select the appropriate functional diversity metric",
                                                     "Identify the level of functional diversity metric measurement"))
                      
                      ),
                      
      tabItem(tabName = "traitspace",
                                
              # Input: Select traits to plot
              box(title = "Select two or more functional traits",
                  status = "primary", solidHeader = TRUE,
                  checkboxGroupInput("traits_xy2", label = "", choices = NULL)
                  ),
                                   
              # Inputs: dendrogram inputs
              box(title = "Dendrogram inputs",
                  status = "primary", solidHeader = TRUE,
                  checkboxInput("standardize", "Standardize traits", value = FALSE),
                                   
                  radioButtons("dist.metric",
                                   label = "Dissimilarity metric",
                                   choices = c("Euclidean" = "euclidean", 
                                               "Manhattan" = "manhattan", 
                                               "Gower" = "gower",
                                               "Mahalanobis" = "mahalanobis"),
                                                selected = "gower"),
                                   
                      radioButtons("cluster.method",
                                   label = "Clustering method",
                                   choices = c("Single" = "single",
                                               "Complete" = "complete",
                                               "Average" = "average",
                                               "Ward" = "ward.D2"),
                                   selected = "average")
                                   ),
                                   
                                   # Input: PCoA arguments
                 box(title = "PCoA inputs", status = "primary", solidHeader = TRUE,
                                       radioButtons("corrections",
                                                    label = "Correction method for negative eigenvalues",
                                                    choices = c("None" = "none", 
                                                                "Lingoes" = "lingoes",
                                                                "Cailliez" = "cailliez")),
                                   
                                       sliderInput("max.naxes", "Maximum number of dimensions of the trait space",
                                                   min = 2, max = 10, value = 2),
                                   
                                       sliderInput("alpha1", "Convex hull transparency",
                                                    min = 0, max = 1, value = 0.5)
                       ),

               # Output: dendrogram, and PCoA
               box(title = "Functional dendrogram", status = "warning", solidHeader = TRUE,
                   plotOutput("dendrogram")
                   ),
              
              box(title = "PCoA", status = "warning", solidHeader = TRUE,
                  plotOutput("pcoa")
                  ),
                                   
              # Input: eigenvalues plot
              box(title = "Screeplot inputs", status = "primary", solidHeader = TRUE,
                  sliderInput("axes.eigenvalues", "Number of axes to plot",
                              min = 2, max = 20, value = 5)
                  ),
                                  
              # Output: eigenvalues
              fluidRow(
                box(title = "Screeplots", 
                    status = "warning", solidHeader = TRUE, width = 12,
                column(6,
                       plotOutput("raw_eigenvalues")),
                column(6,
                       plotOutput("rel_eigenvalues"))
                       ))
              ),
                          
    tabItem(tabName = "alpharich",
            # Input: Select traits to plot
            box(title = "Select two or more functional traits", 
                status = "primary", solidHeader = TRUE, width = 4,
                checkboxGroupInput("traits_xy3", label = "", choices = NULL)
                ),
            
            # Input: hypervolumes
            box(title = "Hypervolume inputs", 
                status = "primary", solidHeader = TRUE, width = 8,
                numericInput("hv.sites", "Number of sites to plot", value = 1),
                                            
               sliderInput("hv.axes", "Number of dimensions",
                           min = 0, max = 10, value = 2),
                                            
               radioButtons("hv.method", "Method",
                            choices = c("Gaussian kernel density" = "gaussian",
                                        "Box kernel density" = "box",
                                        "Support vector machines" = "svm"),
                            selected = "gaussian"),
                                            
               numericInput("npoints", "Number of sampling points", value = 1000),
                                            
               checkboxInput("hv.abund", "Use abundance as weights?", value = FALSE),
                                            
               actionButton("build.hv", "Build hypervolumes")
               ),
                                            
               # Output: hypervolumes
               box(title = "Hypervolumes", status = "warning", solidHeader = TRUE,
                   plotOutput("hv")),
                                    
               box(title = "Alpha functional richness", status = "warning", solidHeader = TRUE,
                   plotOutput("alpha.hv.FD"))
                   ),
                          
    tabItem(tabName = "betarich",
            # Inputs: similarity metric
            box(title = "Similarity metric", status = "primary", solidHeader = TRUE,
                radioButtons("sim.beta.rich", label = "Similarity metric",
                             choices = c("Jaccard" = "jaccard", 
                                         "Sorensen" = "sorensen"),
                             selected = "jaccard")  
            ),
            
            box(title = "Total beta functional diversity", status = "warning", solidHeader = TRUE,
                plotOutput("total.beta")
                ),
            
            box(title = "Turnover", status = "warning", solidHeader = TRUE,
                plotOutput("turnover.beta")
            ),
            
            box(title = "Species richness", status = "warning", solidHeader = TRUE,
                plotOutput("richness.beta")
            ),
                ),
    
    tabItem(tabName = "alphareg",
            
            ),
    
    tabItem(tabName = "betareg",
            
            ),
    
    tabItem(tabName = "divergence",
            
            ),
    
    tabItem(tabName = "similarity",
            
            ),
    
    tabItem(tabName = "spcontrib",
            
            ),
                                   
    tabItem(tabName = "step7",
            helpText("Fit, interpret, report and validate your statistical model.",
                     style = "background-color:lightblue; border-radius:5px"),
                      
              checkboxGroupInput("step7", "Interpret and validate the results",
                                 choices = c("Select an appropriate statistical model or test to answer your research question",
                                 "Report effect sizes, model support and uncertainty",
                                 "Provide a graphical output if needed",
                                 "Did you validate your model and how?"))
                ),
             
    tabItem(tabName = "step8",
            helpText("Provide enough data and code detail to allow full reproducibility
            of your results.",
            style = "background-color:lightblue; border-radius:5px"),
      
            checkboxGroupInput("step8", "Ensure reproducibility",
                                choices = c("Report the software, version and packages you used",
                                            "Deposit data in a public repository",
                                            "Provide your code (tidy and clean)"))
      )))
  )
    
             #div(
              # id = "form",
              # actionButton("submit", "Submit", class = "btn-primary"),
             
             #shinyjs::hidden(
              # div(
               #  id = "thankyou_msg",
                # h3("Thanks for creating your protocol! See the output folder for your filled form")
     #          )
    #         ))


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
      geom_histogram(color = "black", fill = "white", bins = input$bins1) +
      xlab("Species richness") + ylab("Frequency")
  })
  
  output$prevalence <- renderPlot({
    PA.comm <- 1*(community_dataset()>0)
    nsites <- nrow(PA.comm)
    abundance <- colSums(PA.comm)
    prev <- data.frame(prevalence = abundance/nsites)
    ggplot(data = prev, aes(x = prevalence)) + 
      geom_histogram(color = "black", fill = "white", bins = input$bins1) + 
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
                          "histogram" = geom_histogram(alpha = 0.5, bins = input$bins2),
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
            lower = list(continuous = my_fn),
            upper = list(continuous = "cor"))
  })
  
  ### Tab "Trait data space": dendrogram and PCoA
  allColumns <- reactive({
    df <- trait_dataset()
    colnames(df)
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy2",
                             choices = allColumns())
  })
  
  output$dendrogram <- renderPlot({
    traits <- trait_dataset()
    rownames(traits) <- traits[, 1]
    dist.matrix <- vegdist(traits[, input$traits_xy2], 
                           method = input$dist.metric)
    cluster <- hclust(dist.matrix, method = input$cluster.method)
    ggdendrogram(cluster, rotate = TRUE, theme_dendro = FALSE)
  })
  
  output$pcoa <- renderPlot({
    if(input$standardize == TRUE){
      traits <- scale(trait_dataset())
    } else {
      traits <- trait_dataset() 
      }
    rownames(traits) <- traits[, 1]
    dist.matrix <- vegdist(traits[, input$traits_xy2], 
                           method = input$dist.metric)
    pco <- cmdscale(dist.matrix, k = 2, eig = TRUE, add = TRUE)
    pcoa.axes <- as.data.frame(pco$points)
    efit <- envfit(ord = pco, env = traits[, input$traits_xy2])
    vec.sp.df <- as.data.frame(efit$vectors$arrows*sqrt(efit$vectors$r))
    trait.names <- colnames(traits[, input$traits_xy2])
    
    hull <- chull(vec.sp.df[, 1:2])
    
    ggplot() + 
      xlab("Principal Component 1") + ylab("Principal Component 2") +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1,
                 col = "gray") + 
      geom_vline(xintercept = 0, linetype = "dashed", size = 1,
                 col = "gray") +
      geom_point(data = pcoa.axes, aes(x = V1, y = V2), size = 4, 
                 col = "olivedrab3") +
      geom_segment(data = vec.sp.df, aes(x = 0, xend = Dim1 + 0.01, 
                                         y = 0, yend = Dim2 + 0.01),
                   arrow = arrow(length = unit(0.2, "cm")),
                   col = "cornflowerblue") +
      geom_polygon(data = vec.sp.df[hull, ], aes(x = Dim1, y = Dim2), fill = "firebrick1", alpha = input$alpha1) +
      geom_text(data = vec.sp.df, aes(x = Dim1, y = Dim2, label = trait.names),
                size = 4, check_overlap = TRUE) + theme_minimal() +
      xlim(min(vec.sp.df$Dim1) - 0.1, max(vec.sp.df$Dim1 + 0.1)) +
      ylim(min(vec.sp.df$Dim2) - 0.1, max(vec.sp.df$Dim2 + 0.1))
  })
  
  # Screeplots
  output$raw_eigenvalues <- renderPlot({
    if(input$standardize == TRUE){
      traits <- scale(trait_dataset())
    } else {
      traits <- trait_dataset() 
    }
    rownames(traits) <- traits[, 1]
    dist.matrix <- vegdist(traits[, input$traits_xy2], 
                           method = input$dist.metric)
    pco <- cmdscale(dist.matrix, k = input$max.naxes, eig = TRUE, add = TRUE)
    naxes <- input$axes.eigenvalues
    df <- data.frame(axis = 1:naxes, eig = pco$eig[1:naxes])
    ggplot(data = df, aes(x = axis, y = eig)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Component") + ylab("Raw eigenvalue") +
      theme_minimal()
  })
  
  output$rel_eigenvalues <- renderPlot({
    if(input$standardize == TRUE){
      traits <- scale(trait_dataset())
    } else {
      traits <- trait_dataset() 
    }
    rownames(traits) <- traits[, 1]
    dist.matrix <- vegdist(traits[, input$traits_xy2], 
                           method = input$dist.metric)
    pco <- cmdscale(dist.matrix, k = input$max.naxes, eig = TRUE, add = TRUE)
    naxes <- input$axes.eigenvalues
    df <- data.frame(axis = 1:naxes, eig = 100*pco$eig[1:naxes]/sum(pco$eig[1:naxes]))
    ggplot(data = df, aes(x = axis, y = eig)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Component") + ylab("Relative eigenvalue (%)") +
      theme_minimal()
  })
  
  ### Tab "Richness": compute FRic
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy3", 
                             choices = numericColumns())
  })
  
  hypervolumes <- eventReactive(input$build.hv, {
    trait <- as.matrix(trait_dataset()[, input$traits_xy3])
    rownames(trait) <- colnames(community_dataset())
    comm <- community_dataset()[1:input$hv.sites, ]
    
    kernel.build(comm = comm, trait = trait, axes = input$hv.axes,
                 method = input$hv.method, abund = input$hv.abund,
                 samples.per.point = input$npoints)
  })
  
  output$hv <- renderPlot(plot(hypervolumes()))
  
  alpha.FD <- eventReactive(input$build.hv, {
    kernelFD <- data.frame(site = 1:input$hv.sites, 
                           FD = kernel.alpha(hypervolumes()))
    kernelFD
    })
  
  output$alpha.hv.FD <- renderPlot({
    df <- alpha.FD()
    if(input$hv.sites > 15){
      ggplot(data = df, aes(x = FD)) + geom_histogram(bins = 5) +
        xlab("Alpha functional diversity") + ylab("Frequency")
    }
    else {
      ggplot(data = df, aes(x = site, y = FD)) + 
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Site") + ylab("Alpha functional diversity") + theme_bw()
    }
  })
  
  # Tab: Beta richness
  beta.FD <- eventReactive(input$build.hv, {
    kernel.beta.hv <- kernel.beta(hypervolumes(), func = input$sim.beta.rich)
    kernel.beta.hv
  })
  
  output$total.beta <- renderPlot({
    pheatmap(beta.FD()$Btotal)
  })
    
  output$turnover.beta <- renderPlot({
    pheatmap(beta.FD()$Brepl)
  })
    
  output$richness.beta <- renderPlot({
    pheatmap(beta.FD()$Brich)
  })
  
#  formData <- reactive({
#    data <- sapply(fieldsAll, function(x) input[[x]])
#    data <- c(data, timestamp = epochTime())
#    data <- t(data)
#    data
#  })
#  saveData <- function(data) {
#    fileName <- sprintf("FDprotocol_%s.csv",
#                        humanTime())
    ##EJH set quote to FALSE because the commas within the output fields were dividing things across cells due to csv format
#    write.csv(x = data, file = file.path(responsesDir, fileName),
#              row.names = FALSE, quote = FALSE)
#  }
  # action to take when submit button is pressed
#  observeEvent(input$submit, {
#    shinyjs::show("thankyou_msg")
#    saveData(formData())
#  })
}

#fieldsAll <- c("step1", "step2", "step3", "step4", "step5", "step6", "step7", "step8")
#responsesDir <- file.path("../output")
#epochTime <- function() {
#  as.integer(Sys.time())
#}
#humanTime <- function() format(Sys.time(), "%Y%m%d")



shinyApp(ui = ui, server = server) 


