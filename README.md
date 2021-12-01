# ***stepFD*** user's guide
**Software license**

The web-based application is entirely based on Shiny, an open source R package (R 4.0.2, R Core Team 2021). The full list and version of each R package used can be found in the code available at https://github.com/facuxpalacio/stepFD/tree/main/R. 

***stepFD*** **workflow**

*stepFD* is intended to aid researchers and students in performing functional diversity (FD) analyses from the inception of the main ecological question to the proper reporting of data, metadata and code. The layout is based on the eight steps proposed in Palacio et al. (2021), and works mainly as a checklist of what to report in an FD study and as an exploration data tool. Steps 1 and 2 relate to the question of interest and the sampling design, respectively, and Step 8 has information on what to report to fully reproduce a study. Steps 3–7 represent the major interactive sections of the application. Although Step 8 comes at last, we emphasize that this should not be the last thing that is done, and should be part of the research paradigm from the conception of the study.
 
**Example datasets**

*Bird dataset*. This dataset belongs to Palacio et al. (2020, 2021), who collected data on bird abundance and functional traits in nine sampling points of a forest and a shrubland area in east-central Argentina throughout one year. For the sake of simplicity, data were pooled by season. The bird_counts.csv is a matrix of sampling points by bird species, and the bird_traits.txt is a table of species by traits, including: body mass in g, the occurrence of items in the diet (vertebrates, invertebrates, leaves, fruits, seeds, and nectar), foraging substrata (water, ground, vegetation, air), and foraging behaviors (pursuit, gleaning, reaching, pecking, screening and probing). The full dataset is available at https://doi.org/10.5061/dryad.cvdncjt22. 

*Plant dataset*. This dataset (Australian_outcrops.csv) belongs to Ottaviani et al. (2016, 2019) and Ottaviani & Marcantonio (2021), who collected plant traits of granite outcrop vegetation in Southwestern Australia. It includes 481 individuals of 20 selected species (common and/or abundant) on and around the seven studied outcrops. The dataset indicates site name, habitat type (woodland, shrubland, herbfield), sample location (on the outcrop or in the landscape matrix), sun irradiance (exposed, sheltered), and four functional traits: plant height in cm, specific leaf area (SLA) in mm2 mg-1, leaf area in mm2, and leaf dry matter content (LDMC) in mg g-1. For this dataset, there is no species’ occurrence or abundance data.

**Step 3. Community data**

In this section, the user can upload a data frame of sampling units (sites) as rows and species data as columns (where the table contains data on species occurrence or abundance; in the ecological jargon termed ‘community matrix’), and see how the data looks. Once the data are loaded, a dynamic table with the first 10 (25, 50 or 100) rows will be displayed. The dimensions of this data frame (number of units and number of species) are also shown.
 
**Step 4. Trait data**

The user can then upload a data frame of species as rows and traits as columns, and see also how the raw data looks. Traits can be either invariant across sites (i.e., a single trait value per species) or not (i.e., several trait values per species). In both cases, the first column *must* have species names for appropriate plot labeling (these may be repeated if having several trait values per species; see the Australian_outcrops.csv file). Similar to Step 3, a dynamic table with the first 10 (25, 50 or 100) rows will be displayed. The dimensions of this table (number of species and number of traits) are also shown.
 
**Step 5. Explore your data!**

5.1. Community data

Data exploration is the first thing to do before carrying out any statistical analysis (Zuur et al. 2010). Therefore, it is highly recommended to spend some time exploring your dataset before jumping into the calculation of FD metrics. The **‘Data summary’** tab gives summaries of each variable (column) contained within the datasets (mean, median, range, quartiles). The **‘Community data’** tab produces several plots to visualize the community data. For instance, a heatmap of the community matrix shows occurrences (colored cells) across sampling units, along with dendrograms  in the margins. Occurrences can also be transformed (ln(x) + 1), something that is particularly useful for abundance data. Rarefaction curves for each site are also plotted, and are useful tools to assess taxonomic coverage (Magurran 2004). Each curve gives the expected species richness in random subsamples, following Hurlbert (1971). Here, the number of species are plotted as a function of the number of individuals. Thus, rarefaction curves aim to compare species richness between two sampling sites with the same number of individuals sampled (otherwise, a site could have more species than other simply because more individuals were detected). Also, as these curves flatten out at large sample sizes, they give an idea of sampling coverage. Finally, two histograms show the distribution of observed species richness and species prevalence across sampling units. Prevalence in this context is the proportion of sampling units occupied by each species, and describes species relative abundances (Ovaskainen & Abrego 2020). If abundance data are provided, these are transformed to presence-absence data.
 
5.2. Trait data

The **‘Trait plots’** tab allows inspecting individual traits through a set of widely used univariate plots, including boxplots, histograms and density plots. Traits can be transformed (ln(x + 1) and square-root) for better representation, as well as for checking for model assumptions. Grouping variables that account for different additional explanatory factors can also be used; e.g., environmental variables to explore relationships with traits, or species to account for intraspecific variation. The **‘Collinearity’** tab allows the user to inspect bivariate relationships between traits through scatterplots and Pearson’s correlations. Although collinearity could be an issue for model estimation (Dormann et al. 2013) or may not be (Morrissey et al. 2018), the user may also be interested in summarizing traits reflecting hypothesized ecosystem functions (e.g., through dimension reduction techniques; Segrestin et al. 2020, or the identification of functional groups; Palacio et al. 2018). Optionally, simple linear regressions and local regression functions (LOESS) can be added to the scatterplots to depict linear and (potentially) non-linear associations between traits. The ‘Missing data’ tab allows the user to see whether there are (and if so, where are) missing observations in the trait data. This is performed through a barplot, a heatmap, and scatterplots with missing data shown in the margins.

The **‘Trait data space’** tab is focused on building functional trait spaces for which multiple approaches exist (see Mammola et al. 2021 for a review). The first subtab (**‘Functional dendrogram’**) allows the user to represent the trait space using a functional dendrogram through hierarchical clustering (Petchey & Gaston 2002). The user can specify different dissimilarity metrics based on the types of traits analyzed, whether to standardize them (mean = 0, variance = 1) or include missing data, and the type of clustering method. Functional groups can also be depicted on the dendrogram, according to the number of clusters selected. The second subtab (**‘Functional ordination’**) allows building a trait space based on Principal Coordinate Analysis (PCoA). If the user is interested in doing a Principal Component Analysis (PCA) instead, a PCoA based on Euclidean distance will give the same results. The resulting ordination is shown in two dimensions. Here, a minimum convex polygon can be shown to delimit traits, and a Gaussian kernel density may be used to show the density of species across the trait space. The quality of the trait space can be assessed through the percentage of variation explained by the components and by screeplots of the eigenvalues.

Finally, the last subtab (**‘Hypervolume’**) relies on probabilistic hypervolumes (Blonder et al. 2014, 2018) to study functional diversity, as recently proposed by Mammola & Cardoso (2020). A trait-based probabilistic hypervolume represents a density-based estimation of the multidimensional trait space, based on the distribution of the species (or any other taxonomic entity) observed therein (Mammola & Cardoso 2020). In contrast to convex hulls, which assume that the trait space is homogeneous, probabilistic hypervolumes allow the detection of areas of lower or higher density in the trait space (Blonder 2016, Mammola et al. 2021). By contrast, the construction of hypervolumes is computationally demanding. Given the recent surge of hypervolumes in FD approaches, we believe that it is worth providing additional details to get the most out of the application. 

The general procedure to generate a probabilistic hypervolume consists of (i) generating random points around each species, (ii) resampling down to uniform density, (iii) computing a kernel density estimate (KDE) at each random point, and (iv) defining some threshold to retain points that will characterize the hypervolume (see Box 1 in Blonder et al. 2018). The random points delineating the hypervolume can be generated using three different algorithms (Blonder et al. 2018): Gaussian KDE, box KDE, and support vector machines (SVM). Gaussian KDE builds a KDE assuming a normal distribution on the random points wrapping around the original datapoints (species). Box KDE overlaps hyperbox kernels on each datapoint, and then samples random points uniformly from each kernel. SVM uses a one-class SVM to classify random points as ‘in’ or ‘out’ the hypervolume. Gaussian KDE is often the recommended choice under most situations, as the other two methods assume a constant probability density across the distribution (Blonder et al. 2018, Mammola & Cardoso 2020). SVM gives a smooth fit around the data that is insensitive to outliers, and it is computationally less consuming in high-dimensional spaces than the other two methods (Blonder et al. 2018). To assess computation times per sampling unit when building hypervolumes, the user can specify the number of sampling units to use (selected in order from the first to the last row in the community data frame). 

As the density and positions of random points in the trait space have a probabilistic nature, the output of the trait space, as well as the FD metrics computed later on, will inevitably depend on the quality of the hypervolumes. Therefore, it is important to have an enough number of points to characterize the trait space. By default, this number is set to 1000, but it is wise to reduce this number first (e.g., 100 points) to visualize the data and assess computation times. The number of dimensions to build the multivariate trait space is another important choice when dealing with hypervolumes. These dimensions result from a PCoA based on Gower dissimilarities and standardized to mean = 0 and variance = 1 (Cardoso et al. 2021). Although the original traits can be used as dimensions (by selecting 0 axes), this is not recommended, particularly if traits are correlated (Carvalho & Cardoso 2020). Besides, selecting a high number of dimensions is problematic, as it leads to the ‘curse of dimensionality’ and exponentially increases computation time (Blonder 2018, Mammola 2019); thus, it is recommended to keep the number of dimensions between 2 and 5 (Mammola & Cardoso 2020). If the community data frame has abundance data, abundances can be used as weights into the computation of the hypervolumes (yet, this only works for the Gaussian KDE). The resulting output is a plot of the first two dimensions depicting the species, the random points and the hypothetical boundaries of the hypervolumes for each sampling unit. Hypervolumes are stored to compute FD metrics in subsequent steps.
 
**Step 6. Functional diversity**
The world of FD metrics is vast. As *stepFD* is intended to enhance transparency and reproducibility in functional diversity studies, rather than to provide an overview of FD indices, we relied on the unifying framework of probabilistic hypervolumes to compute the three main FD dimensions: functional richness, functional regularity and functional divergence (Mammola & Cardoso 2020). The metrics computed here are the same as those included in the BAT package (Mammola & Cardoso 2020)—to compute alternative metrics, we refer the reader to Mammola et al. (2021) and packages listed in Palacio et al. (2021).

The **‘Richness’** tab gives a barplot (or a histogram if the number of sampling units > 15) of alpha functional richness, which measures the volume of the n-dimensional hypervolume. This tab also gives three heatmaps of beta functional richness among pairs of sampling units. The overall beta functional richness (βtotal) is decomposed into replacement (βreplacement; shift of space between hypervolumes) and richness (βrichness; difference between the amount of space enclosed by each hypervolume) components (see Fig. 2 in Mammola & Cardoso 2020). Beta diversity ranges from 0 (when hypervolumes are completely overlapped) to 1 (when hypervolumes are completely disjunct), and βtotal = βreplacement + βrichness.

The **‘Regularity’** tab also gives barplots (or a histogram if the number of sampling units > 15) of alpha and beta functional regularity. Alpha functional regularity describes the overlap between the hypervolume and a hypothetical hypervolume where traits and abundances are evenly distributed within the range of their values, and beta functional regularity is computed as the difference between alpha regularity values among pairs of sampling units (Mammola & Cardoso 2020).

The **‘Divergence’** tab gives a barplot (or a histogram if the number of sampling units > 15) of the average distance between a percentage (specified by the user) of random points (see Fig. 2 in Mammola & Cardoso 2020). To do this, the user has three options (Mammola & Cardoso 2020): (i) computing the average distance between random points and the centroid of these points (divergence), (ii) computing the average distance between all points (dissimilarity), or (iii) computing the average distance between random points and a regression line ﬁtted through the points (regression).

The **‘Species contribution and originality’** tab computes metrics that characterize the contribution of species to FD and the functional role of species. Functional originality (or rarity) quantifies the average dissimilarity between a species (or an individual) and a sample of random points (Mammola & Cardoso 2020). The contribution of each species to functional richness can be quantified through a nearest neighbor method or a leave-one-out approach (Cardoso et al. 2021). In the nearest neighbor method, each random point is attributed to the closest species, and the contribution is proportional to the number of its points. The leave-one-out approach builds two hypervolumes (one with all the species and another without the species of interest) and computes their difference (Mammola & Cardoso 2020). So, the contribution of a species to the volume can be either positive (when excluding the species reduces the volume) or negative (when excluding the species increases the volume). The contribution of species to functional evenness is only performed with the leave-one-out approach (Cardoso et al. 2021). Functional originality quantifies the average distance between a species and a sample of random points of the hypervolume. Heatmaps of species contributions to functional richness and functional regularity of each sampling unit, as well as species functional originalities in each sampling unit are shown. The user can specify the proportion of points to be used when estimating functional originality, and whether originality should be relativized to the most original species in the community (which takes the value of 1; Cardoso et al. 2021).

Finally, the **‘Correlation among metrics’** tab gives three scatterplot matrices, along with Pearson’s correlations and linear regressions, between the FD metrics computed in the previous tabs. These graphs are plotted both at the community (alpha and beta FD metrics) and species level (contributions to FD and originality).
 
**Step 7. Modelling**

**Step 8. Reproducibility**
 
**FAQ**

*¿Can my community matrix include variables that are not taxa (e.g., site identifier, habitat type, time replicates)?*
Yes, it can, but remember there is a specific section to upload data with predictors. However, if interested in temporal or intraspecific variation, it would be better to include a first column in the community matrix grouping replicates or species, respectively, to account for these factors while exploring the data. In this case, be aware that the number of species in the output will not be the true number of species in your matrix.
 
*¿Can my trait data include variables that are not traits (e.g., species identifier)?*
Yes, it can, and it should. Be aware that the number of traits in the output will not be the true number of traits in your table. 
 
*¿Can I have taxonomic entities other than species in the community and trait matrices?*
Yes, you can. The basic structure to compute FD metrics is similar, regardless of the type of taxa sampled.
 
*I don’t have community or trait data ¿Can I still use the app?*
Yes! As both matrices are independent from each other, you can explore the community matrix on the one hand (e.g., a classical community ecological study), and the trait data table (e.g., if the study aims to search for patterns in trait spaces). However, you won’t be able to compute FD, as this needs both matrices. So, feel free to use the app as it suits you best.
 
**Acknowledgements**

Gianluigi Ottaviani, Pedro Cardoso and Stefano Mammola provided valuable feedback on this guide.

**References**

Blonder, B. (2016). Do hypervolumes have holes?. American Naturalist, 187(4), E93-E105.

Blonder, B. (2018). Hypervolume concepts in niche‐and trait‐based ecology. Ecography, 41(9), 1441-1455.

Blonder, B., Lamanna, C., Violle, C., & Enquist, B. J. (2014). The n‐dimensional hypervolume. Global Ecology and Biogeography, 23(5), 595-609.

Blonder, B., Morrow, C. B., Maitner, B., Harris, D. J., Lamanna, C., Violle, C., ... & Kerkhoff, A. J. (2018). New approaches for delineating n‐dimensional hypervolumes. Methods in Ecology and Evolution, 9(2), 305-319.

Cardoso, P., Mammola, S., Rigal, F., & Carvalho, J. (2021). BAT: Biodiversity Assessment Tools. R package version 2.6.0. https://CRAN.R-project.org/package=BAT

Carvalho, J. C., & Cardoso, P. (2020). Decomposing the causes for niche differentiation between species using hypervolumes. Frontiers in Ecology and Evolution, 8, 243.

Dormann, C. F., Elith, J., Bacher, S., Buchmann, C., Carl, G., Carré, G., ... & Lautenbach, S. (2013). Collinearity: a review of methods to deal with it and a simulation study evaluating their performance. Ecography, 36(1), 27-46.

Hurlbert, S. H. (1971). The nonconcept of species diversity: a critique and alternative parameters. Ecology, 52(4), 577-586.

Magurran, A. E. (2013). Measuring biological diversity. John Wiley & Sons.

Mammola, S. (2019). Assessing similarity of n‐dimensional hypervolumes: Which metric to use? Journal of Biogeography, 46(9), 2012-2023.

Mammola, S., & Cardoso, P. (2020). Functional diversity metrics using kernel density n‐dimensional hypervolumes. Methods in Ecology and Evolution, 11(8), 986-995.

Mammola, S., Carmona, C. P., Guillerme, T., & Cardoso, P. (2021). Concepts and applications in functional diversity. Functional Ecology, 35(9), 1869-1885.

Morrissey, M. B., & Ruxton, G. D. (2018). Multiple regression is not multiple regressions: the meaning of multiple regression and the non-problem of collinearity. Philosophy, Theory, and Practice in Biology, 10(3).

Ottaviani, G., Keppel, G., Marcantonio, M., Mucina, L., & Wardell‐Johnson, G. (2019) Woody species in resource‐rich microrefugia of granite outcrops display unique functional signatures. Austral Ecology, 44: 575–580.

Ottaviani, G., Marcantonio, M., & Mucina, L. (2016) Soil depth shapes plant functional diversity in granite outcrops vegetation of Southwestern Australia. Plant Ecology & Diversity, 9: 263–276.

Ottaviani, G., & Marcantonio, M. (2021). Precipitation seasonality promotes acquisitive and variable leaf water-economics traits in southwest Australian granite outcrop species. Biological Journal of the Linnean Society, 133: 411–417.

Ovaskainen, O., & Abrego, N. (2020). Joint species distribution modelling: with applications in R. Cambridge University Press.

Palacio, F. X., Ibañez, L. M., Maragliano, R. E., & Montalti, D. (2018). Urbanization as a driver of taxonomic, functional, and phylogenetic diversity losses in bird communities. Canadian Journal of Zoology, 96(10), 1114-1121.

Palacio, F. X., Maragliano, R. E., & Montalti, D. (2020). The costs of ignoring species detectability on functional diversity estimation. Auk, 137(4), ukaa057.

Palacio, F.X., Callaghan, C.T., Cardoso, P., Hudgins, E.J., Jarzyna, M.A., Ottaviani, G., Riva, F., Graco-Roza, C., Shirey, V., & Mammola, S. (2021). A protocol for reproducible functional diversity analyses. EcoEvoRxiv https://doi.org/10.32942/osf.io/yt9sb

Petchey, O. L., & Gaston, K. J. (2002). Functional diversity (FD), species richness and community composition. Ecology Letters, 5(3), 402-411.

Segrestin, J., Sartori, K., Navas, M. L., Kattge, J., Díaz, S., & Garnier, E. (2021). PhenoSpace: A Shiny application to visualize trait data in the phenotypic space of the global spectrum of plant form and function. Ecology and Evolution, 11(4), 1526-1534.

Zuur, A. F., Ieno, E. N., & Elphick, C. S. (2010). A protocol for data exploration to avoid common statistical problems. Methods in Ecology and Evolution, 1(1), 3-14.
