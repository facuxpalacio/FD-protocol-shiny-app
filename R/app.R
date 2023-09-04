library(shiny) #1.6.0
library(shinydashboard) #0.7.1
library(shinyjs) #2.0.0
library(shinyFeedback) #0.3.0
library(waiter) #0.2.4
library(ggplot2) #3.3.2
library(GGally) # 2.0.0
library(ggpubr) # 0.6.0
library(factoextra) # 1.0.7
library(pheatmap) #1.0.12
library(vegan) #2.5-6
library(NbClust) #3.0.1
library(FD) #1.0-12.1
library(alphahull) #2.2
library(hypervolume) #2.0.12
library(BAT) #2.6.0
library(VIM) #6.0.0

ui <-dashboardPage(
  dashboardHeader(title = "A toolkit for exploratory functional diversity analyses",
                  titleWidth = 500),
  ## Sidebar content
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("About", tabName = "dashboard", icon = icon("home")),
      menuItem("Load your community data", tabName = "step1", icon = icon("tree")),
      menuItem("Load your trait data", tabName = "step2", icon = icon("leaf")),
      menuItem("Explore your data!", tabName = "step3", icon = icon("area-chart"),
               menuItem("Data summary", tabName = "datasummary"),
               menuItem("Community data", tabName = "communitydata"),
               menuItem("Univariate trait plots", tabName = "traitplots"),
               menuItem("Collinearity", tabName = "collinearity"),
               menuItem("Missing data", tabName = "missingdata"),
               menuItem("Functional trait spaces", tabName = "traitspace",
                        menuSubItem("Functional dendrogram", tabName = "fundend"),
                        menuSubItem("Functional ordination", tabName = "funord"),
                        menuSubItem("Hypervolumes", tabName = "funhv"))),
      menuItem("Functional diversity metrics", tabName = "step4", icon = icon("calculator"),
               menuItem("Richness", tabName = "richness",
                        menuSubItem("Classical metrics", tabName = "classical_rich"),
                        menuSubItem("Hypervolumes", tabName = "hv_rich")),
               menuItem("Regularity", tabName = "regularity",
                        menuSubItem("Classical metrics", tabName = "classical_reg"),
                        menuSubItem("Hypervolumes", tabName = "hv_reg")),
               menuItem("Divergence", tabName = "divergence",
                        menuSubItem("Classical metrics", tabName = "classical_diverg"),
                        menuSubItem("Hypervolumes", tabName = "hv_diverg")),
               menuItem("Species contribution and originality", tabName = "spcontrib"),
               menuItem("Correlations among metrics", tabName = "corrFD"))
    )
  ),
  
  ## Body content
  dashboardBody(
    shinyjs::useShinyjs(),
    use_waiter(),
    tabItems(
      
      # Tab contents
      tabItem(tabName = "dashboard",
              fluidRow(
              box(Title = "", width = 8, style = 'font-size:20px;color:black;',
                  "This application is intended to provide students, professors and researchers with a set of tools to  
                  explore their data and compute functional diversity metrics, as well as for teaching purposes. For further details, see also:",
                  tags$a("Palacio", em("et al."), " (2022). A protocol for reproducible functional diversity 
                           analyses. Ecography 2022: e06287", 
                           href = "https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.06287"), "and the ",
                  tags$a("user's guide.", href = "https://github.com/facuxpalacio/divan"),
                  br(),
                  br(),
                  em("This app is maintained by ",
                  tags$a("Facundo X. Palacio, ", href = "https://github.com/facuxpalacio"),
                  tags$a("Emma J. Hudgins ", href = "https://github.com/emmajhudgins"), "and ",
                  tags$a("Caio Graco-Roza.", href = "https://github.com/graco-roza"),
                  "Please feel free to contact us with any suggestions!")
                  ),
              
              img(src = "sticker_app.png", height = 150),
              
              box(Title = "Getting started", status = "primary", width = 8, style = 'font-size:20px;color:black;',
                  em("divan"), "requires two files to fully operate:",
                  br(),
                  icon("circle"), "A site by species dataframe - This is called the 'community matrix', and it is the
                  usual type of dataset used in classical community ecology.",
                  br(),
                  icon("circle"), "A species by trait dataframe - This is called the 'trait matrix' and contains 
                  the information about species traits.",
                  br(),
                  br(),
                  "Although both files are not strictly needed to be loaded, they are when the ultimate
                  goal is to compute functional diversity."
                  ))
      ),
            
      tabItem(tabName = "step1",
  
                          # Input: Load your community data
                          fluidRow(
                          box(title = "Community data", width = 8, height = 300,
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
                          box(title = "Number of sites and species/individuals", width = 4, height = 300,
                              status = "warning", solidHeader = TRUE,
                          strong(textOutput("nrow_community")),
                          strong(textOutput("ncol_community")))
                           ),
                          
                          fluidRow(
                          box(title = "Community data", width = 12, height = 500,
                              status = "warning", solidHeader = TRUE,
                          dataTableOutput("community_table"))
                          )
                          
                          
                      
             ),
             
      tabItem(tabName = "step2",
 
                          # Input: Load your trait data
                          fluidRow(
                            box(title = "Trait data", width = 8, height = 300,
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
                          strong(textOutput("nrow_traits")),
                          strong(textOutput("ncol_traits")))
                          ),
                          
                          box(title = "Trait data", width = 12, height = 500, 
                              status = "warning", solidHeader = TRUE,
                          dataTableOutput("trait_table"))
                      ),
      
      tabItem(tabName = "datasummary",
              
              # Ouputs: Data summaries
              fluidRow(
                box(title = "Community data", status = "warning", solidHeader = TRUE, width = 12,
                  dataTableOutput("summary_community")
                  )),
              
              fluidRow(
                box(title = "Trait data", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("summary_trait")
                  ))
              ),
                            
                          
      tabItem(tabName = "communitydata",
              
              fluidRow(
                column(width = 6,
              box(title = "Community heatmap inputs", status = "primary", 
                  solidHeader = TRUE, width = NULL,
                  selectInput("dist.metric1",
                          label = "Dissimilarity or correlation metric",
                          choices = c("Euclidean" = "euclidean", 
                                      "Manhattan" = "manhattan",
                                      "Binary" = "binary",
                                      "Correlation" = "correlation"),
                          selected = "euclidean"),
                  checkboxInput("LogX1", "Log-transform occurrences", value = FALSE)
                  ),
              
              # Output: heatmap
              box(title = "Heatmap", status = "warning", solidHeader = TRUE, width = NULL,
              plotOutput("heatmap_community"),
              textInput('filename', "Filename"),
              checkboxInput('savePlot', "Check to save")
              ),
              
              # Output: rank-abundance curve
              box(title = "Rank-abundance curves", status = "warning", solidHeader = TRUE, height = NULL, width = NULL,
                  plotOutput("rank_curve"))),
               
              column(width = 6,                      
                 
                 # Output: rarefaction curves
                box(title = "Rarefaction curves", status = "warning", 
                    solidHeader = TRUE, width = NULL,
                    plotOutput("rarefaction_curves")
                    ),
              
                 box(title = "Histogram and rank-abundance curve inputs", status = "primary", solidHeader = TRUE, width = NULL,
                  sliderInput("bins1", "Number of histogram bins", min = 5, max = 20, value = 10),
                  checkboxInput("density", "Add density plot", value = FALSE),
                  selectInput("select_sites", "Select number of sites to be plotted", choices = c("All combined", "Individual sites"))
                     ),
              
              # Output: histograms
              box(title = "Histograms", status = "warning", solidHeader = TRUE, height = NULL, width = NULL,
                  column(6,
                         plotOutput("richness")),
                  column(6,
                         plotOutput("prevalence")))
                    
              ))),
      
      tabItem(tabName = "traitplots",
              fluidRow(
              # Input: Select traits to plot
              box(title = "Trait plots", status = "primary", solidHeader = TRUE,
                  height = 300, width = 6,
                  selectInput("trait", label = "Select a functional trait",
                              choices = NULL),
                                     
              # Input: Select species to plot (if ITV is accounted for)
                  selectInput("group.var", label = "Grouping factor (e.g., habitat type,
                              species):", choices = NULL),
                                     
              # Input: Select type of plot
                  selectInput("plot.type", label = "Plot type:",
                               list(boxplot = "boxplot", 
                                    histogram = "histogram",
                                    density = "density"))),
              
              # Input: histogram inputs
              box(title = "Inputs", status = "primary", solidHeader = TRUE,
                  height = 300, width = 4,
                  selectInput("trans1", "Data transformation", 
                              choices = c("None", "Log", "Square-root"),
                              selected = "None"),
                  sliderInput("bins2", "Number of histogram bins", min = 5, max = 20, value = 10),
                  textOutput("invalid.trans1")
              )),
                                     
              # Output: Trait plot
              fluidRow(
               box(title = textOutput("caption1"), status = "warning", 
                   solidHeader = TRUE, width = 6,
                   plotOutput("trait_plot"))
              )),
                            
                          
      tabItem(tabName = "collinearity",
              # Input: Select traits to plot
              box(title = "Select two or more functional traits", status = "primary", solidHeader = TRUE,
                  checkboxGroupInput("traits_xy1", label = "", choices = NULL)
                  ),
              
              box(title = "Plot inputs", status = "primary", solidHeader = TRUE,
                  selectInput("trans2", "Data transformation", choices = c("None", "Log", "Square-root"),
                              selected = "None"),
                  checkboxGroupInput("model.scatt", label = "Model fit",
                                     choices = c("Linear regression" = "lm",
                                                 "LOESS" = "loess"),
                                     selected = "lm"),
                  textOutput("invalid.trans2")
                  ),
                                     
              # Output: scatterplots
              fluidRow(
                box(title = "Scatterplots", status = "warning", solidHeader = TRUE, width = 12,
                    textOutput("select.more.traits1"),
                    plotOutput("scatterplots"))
                  )
              ),
                            
                          
      tabItem(tabName = "missingdata",
              
              # Input: Select traits with missing data
              fluidRow(
              box(title = textOutput("caption2"),
                  status = "warning", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("traits_na", label = "", choices = NULL)
                  )),
              
              fluidRow(
              box(title = "Where's your missing data?", label = "", 
                  status = "warning", solidHeader = TRUE, height = 600, width = 6,
                  plotOutput("missing.data1")
                  ),
              
              box(title = "Distribution of missing data", label = "",
                  status = "warning", solidHeader = TRUE, height = 600, width = 6,
                  plotOutput("data.imputation"))
              )),
      
      tabItem(tabName = "fundend",
              # Input: Select traits to plot
              box(title = "Select two or more functional traits",
                  status = "primary", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("traits_xy2", label = "", choices = NULL),
                  checkboxInput("remove.na1", label = "Remove rows with missing data?",
                                value = FALSE),
                  checkboxInput("standardize1", "Standardize traits", value = FALSE),
                  textOutput("select.more.traits22")
                  ),
              
              # Inputs: dendrogram inputs
              box(title = "Dendrogram inputs",
                  status = "primary", solidHeader = TRUE, width = 4,
                  
                  selectInput("dist.metric2",
                               label = "Dissimilarity metric",
                               choices = c("Euclidean" = "euclidean", 
                                           "Manhattan" = "manhattan", 
                                           "Gower" = "gower",
                                           "Mahalanobis" = "mahalanobis"),
                               selected = "gower"),
                  
                  selectInput("cluster.method",
                               label = "Clustering method",
                               choices = c("Single" = "single",
                                           "Complete" = "complete",
                                           "Average" = "average",
                                           "Ward" = "ward.D2"),
                               selected = "average")
              ),
              
              box(title = "Functional group inputs",
                  status = "primary", solidHeader = TRUE,
                  numericInput("k.groups", "Number of functional groups",
                               min = 2, value = 3),
                  
                  checkboxInput("k.groups.optim", "Optimal number of functional groups",
                                value = FALSE),
                  conditionalPanel(
                    condition = "input.k.groups.optim == 1",   
                    selectInput("optim.group.method", "Method to find the optimal number of functional groups",
                                choices = c("KL", "CH", "Hartigan", "CCC", "Scott", "Marriot", 
                                            "TrCovW", "TraceW", "Friedman", "Rubin", "Cindex",
                                            "DB", "Silhouette", "Duda", "PseudoT2", "Beale", 
                                            "Ratkowsky", "Ball", "PtBiserial", "Frey", "McClain",
                                            "Dunn", "Hubert", "SDindex", "Dindex", "SDbw"))), 
                  
                  sliderInput("label.size", "Label size", 
                              min = 0.1, max = 1.5, value = 0.5),
                  checkboxInput("rectangle", "Draw rectangles",
                                     value = FALSE)
                  ),
              
              # Output: dendrogram
              fluidRow(
              box(title = "Functional dendrogram", status = "warning", solidHeader = TRUE,
                  plotOutput("dendrogram"))
              )),
      
      tabItem(tabName = "funord",
              # Input: Select traits to plot
              box(title = "Select two or more functional traits",
                  status = "primary", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("traits_xy3", label = "", choices = NULL),
                  checkboxInput("standardize2", "Standardize traits", value = FALSE),
                  textOutput("select.more.traits23")
                  ),
              
              # Input: PCoA arguments
              box(title = "PCoA inputs", status = "primary", solidHeader = TRUE,
                  height = 500,
                  selectInput("dist.metric3",
                              label = "Dissimilarity metric",
                              choices = c("Euclidean" = "euclidean", 
                                          "Manhattan" = "manhattan", 
                                          "Gower" = "gower",
                                          "Mahalanobis" = "mahalanobis"),
                              selected = "gower"),
                  
                  selectInput("eig.correction",
                              "Correction method for negative eigenvalues",
                              choices = c("Lingoes" = "lingoes",
                                          "Cailliez" = "cailliez")),
                  
                  sliderInput("alpha1", "Convex hull transparency",
                              min = 0, max = 1, value = 0),
                  
                  sliderInput("alpha2", "Kernel density transparency",
                              min = 0, max = 1, value = 0),
                  
                  sliderInput("bins3", "Kernel bandwidth", min = 5, max = 20, value = 10)
              ),
              
              fluidRow(
                column(6,
              box(title = "PCoA", status = "warning", solidHeader = TRUE, height = 450, width = NULL,
                  plotOutput("pcoa"))
                      ),
              
              # Output: variance explained
                column(6,
              box(title = "% variance explained", status = "warning", 
                  solidHeader = TRUE, width = NULL,
                  numericInput("show.pcoa.axes", "Number of axes to show", min = 1, value = 3),
                  tableOutput("var.explained.pcoa") 
                  ),
              
              # Input: eigenvalues plot
              box(title = "Screeplot inputs", status = "primary", 
                  solidHeader = TRUE, width = NULL,
                  numericInput("axes.eigenvalues", "Number of axes to plot",
                              min = 2, value = 5),
                  selectInput("which.screeplot", label = "Eigenvalues to plot",
                              choices = c("Raw eigenvalues" = "raw_eig", 
                                          "Relative eigenvalues" = "rel_eig"))),
                  )),
              
              # Output: eigenvalues
                box(title = "Screeplot", 
                    status = "warning", solidHeader = TRUE, height = NULL, width = 6,
                           plotOutput("scree"))
              ),
              
      tabItem(tabName = "funhv",
              # Input: hypervolumes
                # Input: Select traits to plot
                box(title = "Select two or more functional traits",
                    status = "primary", solidHeader = TRUE, width = 4,
                    checkboxGroupInput("traits_xy4", label = "", choices = NULL),
                    checkboxInput("remove.na3", label = "Remove rows with missing data?",
                                  value = FALSE),
                    textOutput("select.more.traits24")
                    ),
              
              box(title = "Hypervolume inputs", 
                  status = "primary", solidHeader = TRUE, height = 450, width = 4,
                  numericInput("hv.sites", "Number of sites to plot", value = 5),
                  
                  numericInput("hv.axes", "Number of dimensions",
                              min = 1, value = 2),
                  
                  selectInput("hv.method", "Method",
                              choices = c("Gaussian kernel density" = "gaussian",
                                          "Box kernel density" = "box",
                                          "Support vector machines" = "svm"),
                              selected = "gaussian"),
                  
                  numericInput("npoints", "Number of sampling points", value = 1000),
                  
                  checkboxInput("hv.abund", "Use abundance as weights?", value = FALSE),
                  
                  actionButton("build.hv0", "Build hypervolumes")
              ),
              
              # Output: hypervolumes
              box(title = "Hypervolumes", status = "warning", solidHeader = TRUE,
                  height = 450, width = 8, 
                  plotOutput("hv"),
                  textOutput("hv.data")
                  )
      ),       
      
    tabItem(tabName = "classical_rich",
            # Input: functional richness parameters
            fluidRow(
              column(width = 6,
              # Input: Select traits to plot
              box(title = "Select two or more functional traits",
                  status = "primary", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("traits_xy5", label = "", choices = NULL),
                  textOutput("select.more.traits25")
              ),
              
              box(title = "Alpha functional richness inputs", status = "primary", solidHeader = TRUE,
                  checkboxInput("w.abun", "Weight by species relative abundances?", value = TRUE),
                  checkboxInput("stand.x", "Standardize continuous traits?", value = TRUE),
                  checkboxInput("stand.FRic", "Standardize functional richness?", value = TRUE),
                  selectInput("ord", "Method for handling ordinal traits", 
                              choices = c("Podani" = "podani", 
                                          "Metric" = "metric")),
                  selectInput("corr", "Correction method for negative eigenvalues",
                              choices = c("None" = "none",
                                          "Lingoes" = "lingoes",
                                          "Cailliez" = "cailliez",
                                          "Square-root" = "sqrt")),
                  selectInput("dist.metric4a",
                              "Dissimilarity metric of the dendrograms",
                              choices = c("Euclidean" = "euclidean",
                                          "Gower" = "gower",
                                          "Bray-Curtis" = "bray"),
                              selected = "gower"),
                  selectInput("cluster.method2",
                              "Clustering dendrogram method",
                              choices = c("Single" = "single",
                                          "Complete" = "complete",
                                          "Average" = "average",
                                          "Ward" = "ward.D2"),
                              selected = "average"),
                  numericInput("class_rich.sites", "Number of sites to plot", value = 5)),
              
              box(title = "Beta functional richness inputs", status = "primary", solidHeader = TRUE,
                selectInput("sim.beta.rich1", label = "Similarity metric to compute richness",
                               choices = c("Jaccard" = "jaccard", 
                                           "Sorensen" = "sorensen"),
                               selected = "jaccard"),
                
                selectInput("dist.metric4b",
                            label = "Dissimilarity or correlation metric",
                            choices = c("Euclidean" = "euclidean", 
                                        "Manhattan" = "manhattan", 
                                        "Correlation" = "correlation"),
                            selected = "euclidean"))
              
              ),
            
              column(width = 6,
            # Output: functional richness
            box(title = "Functional richness (Villéger et al. 2008)", status = "warning", solidHeader = TRUE,
                plotOutput("class_FRic")),
            
            box(title = "Sum of dendrogram branch lengths", status = "warning", solidHeader = TRUE,
            plotOutput("dendro_FRic")),
            
            box(title = "Beta functional diversity (Cardoso et al. 2014)",
                status = "warning", solidHeader = TRUE, 
                plotOutput("betaTot_FRic")),
            
            box(title = "Turnover component (Cardoso et al. 2014)",
                status = "warning", solidHeader = TRUE, 
                plotOutput("betaRep_FRic")),
            
            box(title = "Species richness component (Cardoso et al. 2014)",
                status = "warning", solidHeader = TRUE, 
                plotOutput("betaRich_FRic"))
            
            ))),
                          
    tabItem(tabName = "hv_rich",
            
            fluidRow(
            box(title = "Alpha functional richness", status = "warning", solidHeader = TRUE,
                plotOutput("alpha.hv.FD")
                ),
                   
            # Inputs: similarity metric
            box(title = "Beta functional richness inputs", status = "primary", solidHeader = TRUE,
                selectInput("sim.beta.rich", label = "Similarity metric to compute richness",
                             choices = c("Jaccard" = "jaccard", 
                                         "Sorensen" = "sorensen"),
                             selected = "jaccard"),
                actionButton("build.hv1", "Compute functional richness"),
                selectInput("dist.metric4",
                            label = "Dissimilarity or correlation metric of the dendrograms",
                            choices = c("Euclidean" = "euclidean", 
                                        "Manhattan" = "manhattan",
                                        "Correlation" = "correlation"),
                            selected = "euclidean")
                )),
            
            # Outpus: hypervolume beta
            box(title = "Beta functional diversity", status = "warning", solidHeader = TRUE,
                plotOutput("total.beta")
                ),
            
            box(title = "Turnover component", status = "warning", solidHeader = TRUE,
                plotOutput("turnover.beta")
            ),
            
            box(title = "Species richness component", status = "warning", solidHeader = TRUE,
                plotOutput("richness.beta")
                ),
    ),
    
    tabItem(tabName = "classical_reg",
            # Input: functional evenness parameters
            fluidRow(
              column(width = 6,
                     
                     box(title = "Alpha functional regularity inputs", status = "primary", solidHeader = TRUE,
                         checkboxInput("w.abun1", "Weight by species relative abundances?", value = TRUE),
                         
                         checkboxInput("stand.x1", "Standardize continuous traits?", value = TRUE),
                         
                         selectInput("ord1", "Method for handling ordinal traits", 
                                     choices = c("Podani" = "podani", 
                                                 "Metric" = "metric")),
                         
                         selectInput("corr1", "Correction method for negative eigenvalues",
                                     choices = c("None" = "none",
                                                 "Lingoes" = "lingoes",
                                                 "Cailliez" = "cailliez",
                                                 "Square-root" = "sqrt")),
                         
                         numericInput("class_eve.sites", "Number of sites to plot", value = 5)),
                     
                     box(title = "Beta functional evenness inputs", status = "primary", solidHeader = TRUE,
                         checkboxInput("w.abun2", "Weight by species relative abundances?", value = TRUE),
                         
                         selectInput("dist.metric4c",
                                     "Dissimilarity metric of the dendrograms",
                                     choices = c("Euclidean" = "euclidean",
                                                 "Gower" = "gower",
                                                 "Bray-Curtis" = "bray"),
                                     selected = "gower"),
                         
                         selectInput("methodEve", label = "Method to compute evenness",
                                     choices = c("Expected values" = "expected", 
                                                 "Contribution of species to the tree" = "contribution"),
                                     selected = "expected"),
                         
                         selectInput("indexEve",
                                     label = "Index to compute evenness",
                                     choices = c("Camargo" = "camargo", 
                                                 "Bulla" = "bulla"),
                                     selected = "camargo"),
                     
                         selectInput("dist.metric4d",
                                     label = "Dissimilarity or correlation metric of the dendrograms",
                                     choices = c("Euclidean" = "euclidean", 
                                                 "Manhattan" = "manhattan",
                                                 "Correlation" = "correlation"),
                                     selected = "euclidean")
                     
              )),
              
              column(width = 6,
                     # Output: functional evenness
                     box(title = "Functional evenness (Villéger et al. 2008)", status = "warning", solidHeader = TRUE,
                         plotOutput("class_FEve")),
                     
                     box(title = "Beta functional evenness (Cardoso et al. 2014)",
                         status = "warning", solidHeader = TRUE, 
                         plotOutput("betaEve"))
                     
              ))
    ),
    
    tabItem(tabName = "hv_reg",
            fluidRow(width = 6,
            box(title = "Functional regularity inputs", status = "primary", solidHeader = TRUE, 
                selectInput("dist.metric5",
                            label = "Dissimilarity or correlation metric of the dendrogram",
                            choices = c("Euclidean" = "euclidean", 
                                        "Manhattan" = "manhattan",
                                        "Correlation" = "correlation"),
                            selected = "euclidean"),
                actionButton("build.hv2", "Compute functional regularity")
            )),
            
            box(title = "Alpha functional regularity", status = "warning", solidHeader = TRUE,
                plotOutput("alpha.regularity")
                ),
            
            box(title = "Beta functional regularity", status = "warning", solidHeader = TRUE,
                plotOutput("beta.regularity")
                )
            
            ),
    
    tabItem(tabName = "classical_diverg",
            # Input: functional evenness parameters
            fluidRow(
              column(width = 6,
                     
                     box(title = "Functional divergence inputs", status = "primary", solidHeader = TRUE,
                         checkboxInput("w.abun3", "Weight by species relative abundances?", value = TRUE),
                         
                         checkboxInput("stand.x3", "Standardize continuous traits?", value = TRUE),
                         
                         selectInput("ord3", "Method for handling ordinal traits", 
                                     choices = c("Podani" = "podani", 
                                                 "Metric" = "metric")),
                         
                         selectInput("corr3", "Correction method for negative eigenvalues",
                                     choices = c("None" = "none",
                                                 "Lingoes" = "lingoes",
                                                 "Cailliez" = "cailliez",
                                                 "Square-root" = "sqrt")),
                         
                         numericInput("class_diverg.sites", "Number of sites to plot", value = 5))
                     
                    ),
              
              column(width = 6,
                     # Output: functional divergence
                     box(title = "Functional divergence (Villéger et al. 2008)", status = "warning", solidHeader = TRUE,
                         plotOutput("class_FDiv"))
                     
              ))
    ),
    
    tabItem(tabName = "hv_diverg",
      
            box(title = "Functional divergence inputs", status = "primary", solidHeader = TRUE,
                selectInput("function.disp", label = "Function to compute divergence",
                             choices = c("Divergence" = "divergence", 
                                         "Dissimilarity" = "dissimilarity",
                                         "Regression" = "regression"),
                             selected = "divergence"),
                sliderInput("fraction1", "Proportion of random points to be used", 
                            min = 0, max = 1, value = 0.1),
                actionButton("build.hv3", "Compute functional divergence"),
                ),
            
            box(title = "Functional divergence", status = "warning", solidHeader = TRUE,
                plotOutput("f.divergence")
                )
            ),
    
    tabItem(tabName = "spcontrib",
            
            fluidRow(
            box(title = "Contribution to functional richness inputs", status = "primary",
                solidHeader = TRUE,
                radioButtons("rich.contrib.method", label = "Method",
                             choices = c("Nearest neighbor" = "neighbor",
                                         "Leave-one-out approach" = "one out"),
                             select = "neighbor"),
                checkboxInput("compute.reg", "Compute contribution to functional
                              regularity? It may take a while", value = FALSE),
                ),
            
            box(title = "Functional originality inputs", status = "primary",
                solidHeader = TRUE,
                sliderInput("fraction2", "Proportion of random points to be used", 
                            min = 0, max = 1, value = 0.1),
                checkboxInput("rel.original", "Should originality be relative to the 
                               most original species in the community?", value = FALSE),
                selectInput("dist.metric6",
                            label = "Dissimilarity or correlation metric of the dendrograms",
                            choices = c("Euclidean" = "euclidean", 
                                        "Manhattan" = "manhattan",
                                        "Correlation" = "correlation"),
                            selected = "euclidean"),
                actionButton("build.hv4", "Compute species contributions") 
           )),
            
            box(title = "Contribution to functional richness", status = "warning", 
                solidHeader = TRUE,
                plotOutput("kernel.rich.contrib")
                ),
           
           box(title = "Contribution to functional regularity", status = "warning", 
               solidHeader = TRUE,
               plotOutput("kernel.eve.contrib")
               ),
            
            box(title = "Functional originality", status = "warning", 
                solidHeader = TRUE,
                plotOutput("kernel.originality")
                )
            ),
                                   
    tabItem(tabName = "corrFD",
            
            box(title = "Community level (alpha diversity)", 
                status = "warning", solidHeader = TRUE,
                plotOutput("alpha.comm.corr")
                ),
            
            box(title = "Community level (beta diversity)", 
                status = "warning", solidHeader = TRUE,
                plotOutput("beta.comm.corr")
            ),
            
            box(title = "Species level",
                status = "warning", solidHeader = TRUE,
                plotOutput("spp.corr")
                )
            
            )
      )
    ))

######################################################################################


server <- function(input, output, session) {

    # toDisplay <- eventReactive(input$step1, {
  #   choices <- c("Which is your research question?",
  #                "Indicate your main hypotheses and predictions")
  #   if (all(choices %in% input$step1)){
  #     return("")
  #   } else if (choices[1] %in% input$step1) {
  #     return("Your ecological question...")
  #   } else if (choices[2] %in% input$step1) {
  #     return("You should clearly state...")
  #   } else {}
  # })
  # output$step1 <- renderText({
  #   toDisplay()
  # })
  
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
                                            options = list(pageLength = 10)) 
  
  output$trait_table <- renderDataTable(trait_dataset(),
                                        options = list(pageLength = 10)) 
  
  # Tab "Summary": Create a summary of the data (only numeric variables)
  numeric_vars <- reactive({
    df <- community_dataset()
    vars <- colnames(df)[sapply(df, function(x) is.numeric(x))]
    df[, vars]
  })
  
  output$summary_community <- renderDataTable(data.frame(Species = colnames(numeric_vars()),
                                                         Mean = apply(numeric_vars(), 2, function(x) round(mean(x, na.rm = T), 2)),
                                                         Standard_deviation = apply(numeric_vars(), 2, function(x) round(sd(x, na.rm = T), 2)),
                                                         Coefficient_of_variation = apply(numeric_vars(), 2, function(x) round(sd(x, na.rm = T)/mean(x, na.rm = T), 2))
                                                         ))
  
  output$summary_trait <- renderDataTable(data.frame(Trait = colnames(trait_dataset()),
                                                     Mean = apply(trait_dataset(), 2, function(x) round(mean(x, na.rm = T), 2)),
                                                     Standard_deviation = apply(trait_dataset(), 2, function(x) round(sd(x, na.rm = T), 2)),
                                                     Coefficient_of_variation = apply(trait_dataset(), 2, function(x) round(sd(x, na.rm = T)/mean(x, na.rm = T), 2))
  ))
  
  output$nrow_community <- renderText({
    paste0("Number of sampling units = ", nrow(community_dataset()))
  })
  
  output$ncol_community <- renderText({
    paste0("Number of species = ", ncol(numeric_vars()))
  })
  
  output$nrow_traits <- renderText({
    paste0("Number of species or individuals = ", nrow(trait_dataset()))
  })
  
  output$ncol_traits <- renderText({
    paste0("Number of traits = ", ncol(trait_dataset())-1)
  })
  
  # Tab "Community data": Heatmap, rarefaction curves, rank-abundance curves and histograms
  output$heatmap_community <- renderPlot({
    if(input$LogX1 == TRUE){
    pheatmap(log(numeric_vars() + 1),
             clustering_distance_rows = input$dist.metric1,
             clustering_distance_columns = input$dist.metric1)
      if(input$savePlot)
      {
        name <- paste0('../output/',input$filename, "_log10.pdf")
        ggsave(name,pheatmap(log(numeric_vars() + 1,
                                 clustering_distance_rows = input$dist.metric1,
                                 clustering_distance_columns = input$dist.metric1)), device="pdf")
      }
    } else {
      pheatmap(numeric_vars(), 
               clustering_distance_rows = input$dist.metric1,
               clustering_distance_columns = input$dist.metric1)
      if(input$savePlot)
      {
        name <- paste0('../output/',input$filename, ".pdf")
        ggsave(name,pheatmap(numeric_vars(),
                             clustering_distance_rows = input$dist.metric1,
                             clustering_distance_columns = input$dist.metric1), device="pdf")
    }
    }
  })

  output$rarefaction_curves <- renderPlot({
    raref.curve <- rarecurve(numeric_vars())
    names(raref.curve) <- paste("site", 1:nrow(numeric_vars()), 
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
      geom_line(linewidth = 0.8) +
      xlab("Sample size") + ylab("Species richness")
  })
  
  output$rank_curve <- renderPlot({
    if(input$select_sites == "All combined"){
      abu <- sort(colSums(numeric_vars()), decreasing = TRUE)
      rank_abundance <- data.frame(species = as.factor(names(abu)), abundance = abu)
      rank_abundance$species <- reorder(rank_abundance$species, rank_abundance$abu, FUN = mean, decreasing = TRUE)
    
      ggplot(rank_abundance, aes(x = species, y = abundance, group = 1)) + geom_line() + 
        geom_point(size = 4) + xlab("Species") + ylab("Abundance") +
        theme(axis.text.x = element_text(angle = 90))
  
        } else {
      
      rank_abundance_list <- list()
      ggplot_list <- list()
      
      for(i in 1:nrow(numeric_vars())){
        rank_site <- sort(numeric_vars()[i, ], decreasing = TRUE)
        rank_abundance_list[[i]] <- data.frame(species = colnames(rank_site), abundance = as.numeric(rank_site))
        rank_abundance_list[[i]]$species <- reorder(rank_abundance_list[[i]]$species, rank_abundance_list[[i]]$abundance, FUN = mean, decreasing = TRUE)
        
        ggplot_list[[i]] <- ggplot(rank_abundance_list[[i]], aes(x = species, y = abundance, group = 1)) + geom_line() + 
          geom_point(size = 4) + xlab("Species") + ylab("Abundance") + theme_bw() +
          theme(axis.text.x = element_text(angle = 90))
        
      }
      
      ggarrange(plotlist = ggplot_list, labels = 1:nrow(numeric_vars()))
    }
  })
  
  output$richness <- renderPlot({
    nspp <- data.frame(richness = rowSums(numeric_vars()))
    if(input$density == TRUE){
    ggplot(data = nspp, aes(x = richness)) + 
      geom_histogram(aes(y=..density..), color = "black", fill = "white", bins = input$bins1) +
        geom_density(fill = "blue", alpha = 0.5, col = "blue") +
      xlab("Species richness") + ylab("Frequency")
    } else {
      ggplot(data = nspp, aes(x = richness)) + 
        geom_histogram(aes(y=..density..), color = "black", fill = "white", bins = input$bins1) +
        xlab("Species richness") + ylab("Frequency")
    }
  })
  
  output$prevalence <- renderPlot({
    PA.comm <- 1*(numeric_vars()>0)
    nsites <- nrow(PA.comm)
    abundance <- colSums(PA.comm)
    prev <- data.frame(prevalence = abundance/nsites)
    if(input$density == TRUE){
    ggplot(data = prev, aes(x = prevalence)) + 
      geom_histogram(aes(y=..density..), color = "black", fill = "white", bins = input$bins1) + 
        geom_density(fill = "blue", alpha = 0.5, col = "blue") +
        xlab("Prevalence") + ylab("Frequency")
    } else {
      ggplot(data = prev, aes(x = prevalence)) + 
        geom_histogram(aes(y=..density..), color = "black", fill = "white", bins = input$bins1) + 
        xlab("Prevalence") + ylab("Frequency")
    }
  })
  
  # Tab "Trait plot": Plot univariate graphs
  # Update non-numeric variables
  factorColumns <- reactive({
    df <- trait_dataset()
    colnames(df)[sapply(df, function(x) !is.numeric(x))]
  })
  
  observe({
    updateSelectInput(session, inputId = "group.var", 
                      choices = c("None", factorColumns()))
  })
  
  output$caption1 <- renderText({
    switch(input$plot.type,
           "boxplot" = "Boxplot",
           "histogram" = "Histogram",
           "density" = "Density plot")
  })
  
  output$invalid.trans1 <- renderText({
    req(input$trait)
    traits <- trait_dataset()[ , input$trait]
    neg <- traits[traits < 0]
    if(length(neg) > 0 && input$trans1 %in% c("Log", "Square-root")) {
      validate("Traits cannot be negative for this transformation")
    }
  })
  
  output$trait_plot <- renderPlot({
    req(input$trait)
    
    if(input$trans1 == "None"){
      tr <- trait_dataset()[, input$trait]
    } else {
      if(input$trans1 == "Log"){
        tr <- log(trait_dataset()[, input$trait] + 1)
      } else {
        tr <- sqrt(trait_dataset()[, input$trait])
      }
    }
    
    if(input$group.var == "None"){ 
      plot.type <- switch(input$plot.type,
                          "histogram" = geom_histogram(aes(y=..density..), color = "black", bins = input$bins2,
                                                       fill = "white", alpha = 0.5),
                          "density" = geom_density(fill = "blue", alpha = 0.5, 
                                                   col = "blue"),
                          "boxplot" = geom_boxplot())
      ggplot(trait_dataset(), aes(x = tr)) + plot.type + 
        xlab(input$trait) + ylab("Frequency")
      
    } else {
      
      group <- trait_dataset()[, input$group.var]
      plot.type <- switch(input$plot.type,
                          "histogram" = geom_histogram(alpha = 0.5, bins = input$bins2),
                          "density" = geom_density(alpha = 0.5),
                          "boxplot" = geom_boxplot())
      
      if(input$plot.type == "boxplot"){
        
        ggplot(trait_dataset(), aes(x = group, y = tr)) + plot.type +
          xlab(input$trait) + ylab("Frequency")
        
      } else {
        
        ggplot(trait_dataset(), aes(x = tr, group = group, fill = group)) + plot.type +
          xlab(input$trait) + ylab("Frequency")
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
    output$invalid.trans2 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy1]
    neg <- traits[traits < 0]
    if(length(neg) > 0 && input$trans2 %in% c("Log", "Square-root")) {
    validate("Traits cannot be negative for this transformation")
      }
    })
    
    output$select.more.traits1 <- renderText({
      traits <- trait_dataset()[ , input$traits_xy1]
      if(is.numeric(traits) == TRUE)
        validate("Please select 2 or more traits")
    })
  
  output$scatterplots <- renderPlot({
    req(input$traits_xy1)
    my_fn <- function(data, mapping, ...){
      p <- ggplot(data = trait_dataset(), mapping = mapping) + 
        geom_point(alpha = 0.5, size = 2) + 
        geom_smooth(method = input$model.scatt, fill = "blue", color = "blue", ...)
      p
    }
    
    traits <- trait_dataset()[, input$traits_xy1]
    switch(input$trans2,
           "None" = ggpairs(traits,
                            lower = list(continuous = my_fn),
                            upper = list(continuous = "cor")),
           "Log" = ggpairs(log(traits + 1),
                           lower = list(continuous = my_fn),
                           upper = list(continuous = "cor")),
           "Square-root" = ggpairs(sqrt(traits),
                                   lower = list(continuous = my_fn),
                                   upper = list(continuous = "cor")))
  })
  
  # Plot missing data
  output$caption2 <- renderText({
    ntraits.missing <- length(NAcolumns())
    paste0("You have ", ntraits.missing, " trait/s with missing values")
  })
  
  output$missing.data1 <- renderPlot({
    req(input$traits_na)
    aggr(trait_dataset())
    })
  
  output$data.imputation <- renderPlot({
    req(input$traits_na)
    traits <- trait_dataset()[, input$traits_na]
    marginplot(traits, alpha = 0.6, col = c("skyblue", "orange"), pch = 19)
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
  
  output$select.more.traits22 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy2]
    if(is.numeric(traits) == TRUE)
      validate("Please select 2 or more traits")
  })
  
    
  output$dendrogram <- renderPlot({
    req(input$traits_xy2)
    selected.traits <- trait_dataset()[, input$traits_xy2]
    
    if(input$standardize1 == TRUE){
      traits1 <- scale(selected.traits)
    } else {
      traits1 <- selected.traits
    }
    
    if(input$remove.na1 == TRUE){
      traits2 <- na.omit(traits1)
      rowsNA <- unique(which(is.na(selected.traits), arr.ind = TRUE)[, 1])
      rownames(traits2) <- make.names(trait_dataset()[-rowsNA, 1], unique = TRUE)
      if(sum(is.na(traits1)) == 0)
        validate("You have no missing data")
    } else {
      traits2 <- traits1
      rownames(traits2) <- make.names(trait_dataset()[, 1], unique = TRUE)
    }
    
    if(input$k.groups.optim == TRUE){
    dist.matrix <- dist(traits2, method = "euclidean")
    cluster <- hclust(dist.matrix, method = "average")
    optim.clusters <- NbClust(traits2, distance = "euclidean", method = "average", index = "all")
    n.clusters <- optim.clusters$Best.nc[1, input$optim.group.method]
    
    fviz_dend(cluster, cex = input$label.size, horiz = TRUE, main = "",
              k = n.clusters, color_labels_by_k = TRUE, 
              rect = input$rectangle, rect_fill = input$rectangle,
              xlab = "Species", ylab = "Euclidean distance (UPGMA)",
              ggtheme = theme_minimal(),
              sub = paste0("Optimal number of clusters = ", n.clusters))
      
  } else {
      
    dist.matrix <- vegdist(traits2, method = input$dist.metric2, na.rm = TRUE)
    cluster <- hclust(dist.matrix, method = input$cluster.method)
    fviz_dend(cluster, cex = input$label.size, horiz = TRUE, main = "",
              k = input$k.groups, color_labels_by_k = TRUE, 
              rect = input$rectangle, rect_fill = input$rectangle,
              xlab = "Species", ylab = "Dissimilarity",
              ggtheme = theme_minimal())
  }
    
  })
  
    output$select.more.traits23 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy3]
    if(is.numeric(traits) == TRUE)
      validate("Please select 2 or more traits")
  })
  
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy3",
                             choices = allColumns())
  })
  
  output$select.more.traits23 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy3]
    if(is.numeric(traits) == TRUE)
      validate("Please select 2 or more traits")
  })
  
  pcoa.function <- reactive({
    req(input$traits_xy3)
    selected.traits <- trait_dataset()[, input$traits_xy3]
    
    if(input$standardize2 == TRUE){
      traits1 <- na.omit(scale(selected.traits))
    } else {
      traits1 <- na.omit(selected.traits)
    }

    dist.matrix <- vegdist(traits1, method = input$dist.metric3, na.rm = TRUE)
    
    pco <- wcmdscale(dist.matrix, eig = TRUE, add = input$eig.correction)
    pcoa.axes <- as.data.frame(pco$points)
    efit <- envfit(ord = pco, env = traits1[, input$traits_xy3])
    vec.sp.df <- as.data.frame(efit$vectors$arrows*sqrt(efit$vectors$r))
    trait.names <- colnames(traits1[, input$traits_xy3])
    list(pcoa.eig = pco$eig, pcoa.axes = pcoa.axes, pcoa.vectors = vec.sp.df,
         trait.names = trait.names)
  })
    
  output$pcoa <- renderPlot({
    pco <- pcoa.function()
    pcoa.axes <- pco$pcoa.axes
    pcoa.vectors <- pco$pcoa.vectors
    trait.names <- pco$trait.names
    
    hull <- chull(pcoa.axes[, 1:2])
    
    ggplot() + 
      xlab("Principal Component 1") + ylab("Principal Component 2") +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1,
                 col = "gray") + 
      geom_vline(xintercept = 0, linetype = "dashed", size = 1,
                 col = "gray") +
      geom_point(data = pcoa.axes, aes(x = Dim1, y = Dim2), size = 4, 
                 col = "olivedrab3") +
      geom_segment(data = pcoa.vectors, aes(x = 0, xend = Dim1 + 0.01, 
                                         y = 0, yend = Dim2 + 0.01),
                   arrow = arrow(length = unit(0.2, "cm")),
                   col = "cornflowerblue") +
      geom_polygon(data = pcoa.axes[hull, ], aes(x = Dim1, y = Dim2), 
                   fill = "firebrick1", alpha = input$alpha1) +
      geom_density2d_filled(data = pcoa.axes, aes(x = Dim1, y = Dim2),
                            size = 0.8, alpha = input$alpha2, 
                            bins = input$bins3) +
      geom_text(data = pcoa.vectors, aes(x = Dim1, y = Dim2, label = trait.names),
                size = 4, check_overlap = TRUE) + theme_minimal() +
      xlim(min(pcoa.vectors$Dim1) - 0.2, max(pcoa.vectors$Dim1 + 0.2)) +
      ylim(min(pcoa.vectors$Dim2) - 0.2, max(pcoa.vectors$Dim2 + 0.2))
  })
  
  # % variance explained
  output$var.explained.pcoa <- renderTable({
    pco <- pcoa.function()
    pcoa.eig <- pco$pcoa.eig
    
    cum_var <- 100*cumsum(pcoa.eig)/sum(pcoa.eig)
    df <- data.frame(axis = 1:input$show.pcoa.axes, 
                     cum_var = cum_var[1:input$show.pcoa.axes])
    colnames(df) <- c("Principal component", "Cumulative variance (%)")
    df
  })
  
  # Screeplots
  output$scree <- renderPlot({
    
    if(input$which.screeplot == "raw_eig"){
    pco <- pcoa.function()
    pcoa.eig <- pco$pcoa.eig
    
    naxes <- input$axes.eigenvalues
    df <- data.frame(axis = 1:naxes, eig = pcoa.eig[1:naxes])
    ggplot(data = df, aes(x = axis, y = eig)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      xlab("Component") + ylab("Raw eigenvalue") +
      theme_minimal()
  
  } else {
  
    pco <- pcoa.function()
    pcoa.eig <- pco$pcoa.eig
    
    naxes <- input$axes.eigenvalues
    df <- data.frame(axis = 1:naxes, eig = 100*pcoa.eig[1:naxes]/sum(pcoa.eig[1:naxes]))
    ggplot(data = df, aes(x = axis, y = eig)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      xlab("Component") + ylab("Relative eigenvalue (%)") +
      theme_minimal()
  }
  })
  
  # Tab: "Hypervolume building"
  # Update variable selection
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy4",
                             choices = allColumns())
  })
  
  output$select.more.traits24 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy4]
    if(is.numeric(traits) == TRUE)
      validate("Please select 2 or more traits")
  })
  
  hypervolumes <- eventReactive(input$build.hv0, {
    trait <- as.matrix(trait_dataset()[, input$traits_xy4])
    rownames(trait) <- colnames(community_dataset())
    comm <- community_dataset()[1:input$hv.sites, ]
    if(input$remove.na3 == TRUE){
      trait <- na.omit(trait)
    } else { trait }
    
    khv <- list()
    
    withProgress(message = "Building hypervolumes", {
      
      for (i in 1:input$hv.sites) {
        khv <- hypervolume_join(khv,
          kernel.build(comm = comm[i, ], trait = trait, axes = input$hv.axes,
                     method = input$hv.method, abund = input$hv.abund,
                     samples.per.point = input$npoints))
        incProgress(1/input$hv.sites, detail = paste("Hypervolume", i))
      }
      
       khv
       
      })
  })
  
  output$hv <- renderPlot({
    req(input$community_dataset)
    req(input$traits_xy4)
    plot(hypervolumes())
    })
  
  output$hv.data <- renderText({
    if(is.null(input$community_dataset) && is.null(input$trait_dataset)){
      validate("Community and trait data are not given yet")
    } else {
      if(is.null(input$community_dataset) && !is.null(input$trait_dataset)){
        validate("Community data are not given yet")
      } else {
        if(!is.null(input$community_dataset) && is.null(input$trait_dataset)){
        validate("Trait data are not given yet")
          } else {
            validate("Now you can build hypervolumes!")
          }
        }
      }
  })
  
  ### Tab "Classical richness": Alpha and beta
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy5",
                             choices = allColumns())
  })
  
  output$select.more.traits25 <- renderText({
    traits <- trait_dataset()[ , input$traits_xy5]
    if(is.numeric(traits) == TRUE)
      validate("Please select 2 or more traits")
  })
  
  FRic.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    fd <- dbFD(x = traits, a = numeric_vars(), w.abun = input$w.abun, 
               stand.x = input$stand.x, ord = input$ord, 
               corr = input$corr, stand.FRic = input$stand.FRic, calc.FGR = FALSE,
               calc.CWM = FALSE, calc.FDiv = FALSE, messages = FALSE)
    fd_list <- list(df = data.frame(site = 1:nrow(numeric_vars()), FRic = fd$FRic),
                    qual.FRic = fd$qual.FRic)
    fd_list
  })
  
  FDdendro.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    dist.matrix <- vegdist(traits, method = input$dist.metric4a, na.rm = TRUE)
    cluster <- hclust(dist.matrix)
    fd <- treedive(comm = numeric_vars(), tree = cluster)
    data.frame(site = 1:nrow(numeric_vars()), FRic = fd)
  })
  
  output$class_FRic <- renderPlot({
    fd_list <- FRic.function()
    dat <- fd_list$df[1:input$class_rich.sites, ]
    if(input$class_rich.sites > 15){
      ggplot(data = dat, aes(x = FRic)) + geom_histogram(bins = 5) +
        xlab("Functional richness") + ylab("Frequency") +
        ggtitle(paste0("Quality of the reduced trait space = ", round(fd_list$qual.FRic, 2)))
    } else {
      ggplot(data = dat, aes(x = site, y = FRic)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Functional richness") + theme_bw() +
        ggtitle(paste0("Quality of the reduced trait space = ", round(fd_list$qual.FRic, 2)))
    }
  })
  
  output$dendro_FRic <- renderPlot({
    df <- FDdendro.function()
    if(input$class_rich.sites > 15){
      ggplot(data = df, aes(x = FRic)) + geom_histogram(bins = 5) +
        xlab("Sum of branch lengths") + ylab("Frequency")
    } else {
      ggplot(data = df[1:input$class_rich.sites, ], aes(x = site, y = FRic)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Sum of branch lengths") + theme_bw()
    }
  })
  
  FRic_beta.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    dist.matrix <- vegdist(traits, method = input$dist.metric4a, na.rm = TRUE)
    cluster <- hclust(dist.matrix)
    
    comm <- numeric_vars()[1:input$class_rich.sites, ]
    beta(comm = comm, tree = cluster, func = input$sim.beta.rich1) 
  })
  
  output$betaTot_FRic <- renderPlot({
    pheatmap(as.matrix(FRic_beta.function()$Btotal),
             clustering_distance_rows = input$dist.metric4b,
             clustering_distance_columns = input$dist.metric4b)
  })
  
  output$betaRep_FRic <- renderPlot({
    pheatmap(as.matrix(FRic_beta.function()$Brepl),
             clustering_distance_rows = input$dist.metric4b,
             clustering_distance_columns = input$dist.metric4b)
  })
  
  output$betaRich_FRic <- renderPlot({
    pheatmap(as.matrix(FRic_beta.function()$Brich),
             clustering_distance_rows = input$dist.metric4b,
             clustering_distance_columns = input$dist.metric4b)
  })
  
  ### Tab "Hypervolume richness": Alpha
  alpha.FD <- eventReactive(input$build.hv1, {

    kernelFD <- data.frame(site = numeric(0), FD = numeric(0))
    
    withProgress(message = "Computing alpha functional richness", {
      
      for (i in 1:input$hv.sites) {
        kernelFD <- rbind(kernelFD, 
                          data.frame(site = i, FD = kernel.alpha(hypervolumes()[[i]])))
        incProgress(1/input$hv.sites)
      }
      
    })
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
  
  # Tab "Hypervolume richness": Beta
  beta.FD <- eventReactive(input$build.hv1, {
    waiter <- Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    kernel.beta.hv <- kernel.beta(hypervolumes(), func = input$sim.beta.rich)
    kernel.beta.hv
  })
  
  output$total.beta <- renderPlot({
    pheatmap(as.matrix(beta.FD()$Btotal),
                       clustering_distance_rows = input$dist.metric4,
                       clustering_distance_columns = input$dist.metric4)
  })
    
  output$turnover.beta <- renderPlot({
    pheatmap(as.matrix(beta.FD()$Brepl),
                       clustering_distance_rows = input$dist.metric4,
                       clustering_distance_columns = input$dist.metric4)
  })
    
  output$richness.beta <- renderPlot({
    pheatmap(as.matrix(beta.FD()$Brich),
                       clustering_distance_rows = input$dist.metric4,
                       clustering_distance_columns = input$dist.metric4)
  })
  
  # Tab: "Regularity" classical metrics
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy5",
                             choices = allColumns())
  })

  FReg.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    fd <- dbFD(x = traits, a = numeric_vars(), w.abun = input$w.abun1, 
               stand.x = input$stand.x1, ord = input$ord1, 
               corr = input$corr1, calc.FRic = FALSE, calc.FGR = FALSE,
               calc.CWM = FALSE, calc.FDiv = FALSE, messages = FALSE)
    data.frame(site = 1:nrow(numeric_vars()), FEve = fd$FEve)
  })
  
  output$class_FEve <- renderPlot({
    df <- FReg.function()[1:input$class_eve.sites, ]
    if(input$class_eve.sites > 15){
      ggplot(data = df, aes(x = FEve)) + geom_histogram(bins = 5) +
        xlab("Functional evenness") + ylab("Frequency")
    } else {
      ggplot(data = df, aes(x = site, y = FEve)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Functional eveness") + theme_bw()
    }
  })
  
  FEve_beta.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    dist.matrix <- vegdist(traits, method = input$dist.metric4c, na.rm = TRUE)
    cluster <- hclust(dist.matrix)
    
    comm <- as.matrix(numeric_vars()[1:input$class_eve.sites, ])
    beta.evenness(comm = comm, tree = cluster, abund = input$w.abun2, 
                  method = input$methodEve, func = input$indexEve) 
  })
  
  output$betaEve <- renderPlot({
    pheatmap(as.matrix(FEve_beta.function()),
             clustering_distance_rows = input$dist.metric4d,
             clustering_distance_columns = input$dist.metric4d)
  })
  
  
  # Tab: "Regularity" hypervolumes
  alpha.reg.hv <- eventReactive(input$build.hv2, {
    
    kernel.reg.alpha <- data.frame(site = numeric(0), FD = numeric(0))
    
    withProgress(message = "Computing alpha functional regularity", {
      
      for (i in 1:input$hv.sites) {
        kernel.reg.alpha <- rbind(kernel.reg.alpha, 
                          data.frame(site = i, FD = kernel.evenness(hypervolumes()[[i]])))
        incProgress(1/input$hv.sites)
      }
      
    })
    kernel.reg.alpha
  })
  
  output$alpha.regularity <- renderPlot({
    df <- alpha.reg.hv()
    if(input$hv.sites > 15){
      ggplot(data = df, aes(x = FD)) + geom_histogram(bins = 5) +
        xlab("Alpha functional regularity") + ylab("Frequency")
    }
    else {
      ggplot(data = df, aes(x = site, y = FD)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Alpha functional regularity") + theme_bw()
    }
  })
  
  beta.reg.hv <- eventReactive(input$build.hv2, {
    kernel.reg.beta <- kernel.beta.evenness(hypervolumes())
    kernel.reg.beta
  })
  
  output$beta.regularity <- renderPlot({
    pheatmap(as.matrix(beta.reg.hv()),
             clustering_distance_rows = input$dist.metric5,
             clustering_distance_columns = input$dist.metric5)
  })
  
  # Tab: "Divergence" classical metrics
  observe({
    updateCheckboxGroupInput(session, inputId = "traits_xy5",
                             choices = allColumns())
  })
  
  FDiv.function <- reactive({
    req(input$traits_xy5)
    traits <- trait_dataset()[, input$traits_xy5]
    rownames(traits) <- colnames(numeric_vars())
    
    fd <- dbFD(x = traits, a = numeric_vars(), w.abun = input$w.abun3, 
               stand.x = input$stand.x3, ord = input$ord3, 
               corr = input$corr3, calc.FRic = TRUE, calc.FGR = FALSE,
               calc.CWM = FALSE, calc.FDiv = TRUE, messages = FALSE)
    data.frame(site = 1:nrow(numeric_vars()), FDiv = fd$FDiv)
  })
  
  output$class_FDiv <- renderPlot({
    df <- FDiv.function()[1:input$class_diverg.sites, ]
    if(input$class_diverg.sites > 15){
      ggplot(data = df, aes(x = FDiv)) + geom_histogram(bins = 5) +
        xlab("Functional divergence") + ylab("Frequency")
    } else {
      ggplot(data = df, aes(x = site, y = FDiv)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Functional divergence") + theme_bw()
    }
  })
  
  # Tab: "Divergence" hypervolumes
  div.hv <- eventReactive(input$build.hv3, {
    
    kernel.div <- data.frame(site = numeric(0), FD = numeric(0))
    
    withProgress(message = "Computing alpha functional divergence", {
      
      for (i in 1:input$hv.sites) {
        kernel.div <- rbind(kernel.div, 
                            data.frame(site = i, FD = kernel.dispersion(hypervolumes()[[i]],
                                                                        func = input$function.disp,
                                                                        frac = input$fraction1)))
        incProgress(1/input$hv.sites)
      }
      
    })
    kernel.div
  })
  
  output$f.divergence <- renderPlot({
    df <- div.hv()
    if(input$hv.sites > 15){
      ggplot(data = df, aes(x = FD)) + geom_histogram(bins = 5) +
        xlab("Functional divergence") + ylab("Frequency")
    }
    else {
      ggplot(data = df, aes(x = site, y = FD)) + 
        geom_bar(stat = "identity", fill = "steelblue") +
        xlab("Site") + ylab("Functional divergence") + theme_bw()
    }
  })
  
  # Tab: "Species contribution and originality"
  spp.contrib <- eventReactive(input$build.hv4, {
    waiter <- Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    if(input$compute.reg == TRUE){
    rich.contrib <- kernel.contribution(hypervolumes(), func = input$rich.contrib.method)
    rich.contrib[is.na(rich.contrib)] <- 0
    
    eve.contrib <- kernel.evenness.contribution(hypervolumes())
    original <- kernel.originality(hypervolumes(), frac = input$fraction2, 
                                   relative = input$rel.original)
    original[is.na(original)] <- 0
    list(rich.contrib = rich.contrib, eve.contrib = eve.contrib, original = original)
    
    } else {
      rich.contrib <- kernel.contribution(hypervolumes(), func = input$rich.contrib.method)
      rich.contrib[is.na(rich.contrib)] <- 0
      original <- kernel.originality(hypervolumes(), frac = input$fraction2, 
                                     relative = input$rel.original)
      original[is.na(original)] <- 0
      list(rich.contrib = rich.contrib, original = original)
    }
  })
  
  output$kernel.rich.contrib <- renderPlot({
    spp.contrib_list <- spp.contrib()
    pheatmap(spp.contrib_list$rich.contrib,
             clustering_distance_rows = input$dist.metric6,
             clustering_distance_columns = input$dist.metric6)
  })
  
  output$kernel.eve.contrib <- renderPlot({
  spp.contrib_list <- spp.contrib()
    pheatmap(spp.contrib_list$eve.contrib,
             clustering_distance_rows = input$dist.metric6,
             clustering_distance_columns = input$dist.metric6)
  })
  
  output$kernel.originality <- renderPlot({
    spp.contrib_list <- spp.contrib()
    pheatmap(spp.contrib_list$original,
             clustering_distance_rows = input$dist.metric6,
             clustering_distance_columns = input$dist.metric6)
  })
  
  # Tab: correlations among FD metrics
  
  # Create reactive values to store FD results
  #alpha_rv <- reactive({FRic = NULL, dendrogramFD = NULL, FEve = NULL, FDiv = NULL,
  #                           HV_FRic = NULL, HV_FReg = NULL, HV_FDiv = NULL})
 # beta_rv <- reactive({betaFRic = NULL, beta_turnover = NULL,
  #                          beta_richness = NULL, beta_FEve = NULL,
  #                          HV_beta_turnover = NULL, HV_beta_richness = NULL,
  #                          HV_beta_FEve = NULL, HV_beta_FReg = NULL})
  
  #observeEvent(input$simulate, {
 #   alpha_rv$FRic <- FRic.function()$fd_list$df$FRic
 #   alpha_rv$dendrogramFD <- FDdendro.function()$FRic
 #   alpha_rv$FEve <- FReg.function()$FEve
 #     alpha_rv$FDiv <- FDiv.function()$FDiv
 #     alpha_rv$HV_FRic <- alpha.FD()$FD
 #     alpha_rv$HV_FReg <- alpha.reg.hv()$FD
 #     alpha_rv$HV_FDiv <- div.hv()$FD
 #     beta_rv$betaFRic <- as.numeric(FRic_beta.function()$Btotal)
 #     beta_rv_$beta_turnover <- as.numeric(FRic_beta.function()$Brepl)
 #     beta_rv_$beta_richness <- as.numeric(FRic_beta.function()$Brich)
 #     beta_rv_$beta_FEve <- as.numeric(FEve_beta.function())
 #     beta_rv_$HV_beta_turnover <- 
 #     beta_rv_$HV_beta_richness <-
 #     beta_rv_$HV_beta_FEve <- 
 #     beta_rv_$HV_beta_FReg <- 
 # })
  
  
  
  output$alpha.comm.corr <- renderPlot({
    df <- data.frame(#FRic = FRic.function()$df$FRic,
                     #FDendrogram = FDdendro.function()$FRic,
                     #FEve = FReg.function()$FEve,
                     #FDiv = FDiv.function()$FDiv,
                     HV_FRic = alpha.FD()$FD, 
                     HV_FReg = alpha.reg.hv()$FD, 
                     HV_FDiv = div.hv()$FD)
    
   my_fn <- function(data, mapping, ...){
      p <- ggplot(data = df, mapping = mapping) + 
        geom_point(alpha = 0.5, size = 2) + 
        geom_smooth(method = lm, fill = "blue", color = "blue", ...)
      p
   }
   
   ggpairs(df, lower = list(continuous = my_fn), upper = list(continuous = "cor"))
  })
   
   output$beta.comm.corr <- renderPlot({
     df <- data.frame(FRic_beta_total = as.numeric(FRic_beta.function()$Brich),
                      FRic_beta_turnover = as.numeric(FRic_beta.function()$Brepl),
                      FRic_beta_richness = as.numeric(FRic_beta.function()$Brich),
                      HV_FRic_total = as.numeric(beta.FD()$Btotal),
                      HV_FRic_turnover = as.numeric(beta.FD()$Brepl),
                      HV_FRic_richness = as.numeric(beta.FD()$Brich),
                      FEve_beta = as.numeric(FEve_beta.function()),
                      HV_FReg = as.numeric(beta.reg.hv()))
     
     my_fn <- function(data, mapping, ...){
       p <- ggplot(data = df, mapping = mapping) + 
         geom_point(alpha = 0.5, size = 2) + 
         geom_smooth(method = lm, fill = "blue", color = "blue", ...)
       p
     }
   
    ggpairs(df, lower = list(continuous = my_fn), upper = list(continuous = "cor"))
  })
  
  output$spp.corr <- renderPlot({
    spp.contrib_list <- spp.contrib()
  
    if(input$compute.reg == TRUE){
      df <- data.frame(Rarity_richness = as.numeric(spp.contrib_list$rich.contrib),
                       Rarity_regularity = as.numeric(spp.contrib_list$eve.contrib),
                       Originality = as.numeric(spp.contrib_list$original))
    
    } else {
      df <- data.frame(Rarity_richness = as.numeric(spp.contrib_list$rich.contrib),
                       Originality = as.numeric(spp.contrib_list$original))
    }
    
    my_fn <- function(data, mapping, ...){
      p <- ggplot(data = df, mapping = mapping) + 
        geom_point(alpha = 0.5, size = 2) + 
        geom_smooth(method = lm, fill = "blue", color = "blue", ...)
      p
    }
    
    ggpairs(df, lower = list(continuous = my_fn), upper = list(continuous = "cor"))
  })
  
 formData <- reactive({
   data <- sapply(fieldsAll, function(x) input[[x]])
   data <- c(data, date = humanTime())#add escape characters to commas to avoid breaking up into more than 1 cell
  data<-gsub(",", "\\,", data)
   data <- t(data)
  colnames(data)<-c(fieldsAll, "date")
   data
 })
 saveData <- function(data) {
   fileName <- sprintf("FDprotocol_%s.csv",
                       humanTime())
   write.csv(x = data, file = file.path(responsesDir, fileName),
             row.names = FALSE)
 }
  # action to take when submit button is pressed
 observeEvent(input$submit, {
   shinyjs::show("thankyou_msg")
   saveData(formData())
 })
}

fieldsAll <- c("step1","hyp","nohyp", "scale", "unit","pow1", "pow2", "prer1", "prer2", "foc","reso", "ntax", "s_eff", "step4", "step5", "step6", "step7", "step8")
responsesDir <- file.path("../output")

humanTime <- function() format(Sys.time(), "%Y%m%d")


shinyApp(ui = ui, server = server) 


