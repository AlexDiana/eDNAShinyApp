# library(shiny)
# library(ggplot2)
# library(markdown)
# library(DT)
# library(shinythemes)
# library(shinycssloaders)
# library(MASS)
# library(reshape2)
# library(grid)
# library(MCMCpack)
# library(shinyalert)
# library(dplyr)
# # library(shinyAppFunctions)
# library(shinyjs)
# library(beepr)
# library(coda)

options(shiny.maxRequestSize=30*1024^2) 
options(warn = -1)

ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                tabsetPanel(
                  tabPanel(h1(strong("eDNA")),
                           tabsetPanel(
                             tabPanel(h3("Help"),icon = icon("book-open", lib = "font-awesome"),
                                      tabsetPanel(
                                        tabPanel(h4("Background"), 
                                                 p("This app implements the methods developed by Griffin, J. E., Matechou, E. Buxton, A. S., Bormpoudakis, D. and Griffiths, R. A., in ", tags$em("Modelling environmental DNA data; Bayesian variable selection accounting for false positive and false negative errors,"), "Journal of the Royal Statistical Society: Series C (Applied Statistics)."),
                                                 
                                                 p("The method is appropriate for modelling", strong("eDNA scores"), "(i.e. the number of positive qPCR replicates) and", strong("accounts for the probabilities of false positive and false negative errors"), "in the field (data collection - stage 1) and in the lab (data analysis - stage 2) when",  strong("estimating the probability of species presence using single-species eDNA data.")),
                                                 
                                                 p("The model is fitted using a", strong("Bayesian approach"), "and",  strong("all model parameters,"), "that is the probability of species presence and the probabilities of error in both stages,", strong("can be modelled as functions of covariates"), "with the important covariates for each probability identified using", strong("Bayesian variable selection.")),
                                                 
                                                 p("The following notation is introduced in the Griffin et al. (2019) paper and used throughout this app. All parameters are modelled as site-specific but we ommit the subscripts here for ease of notation."),
                                                 
                                                 p(HTML(paste("&#968;",": Probability of species presence",sep = ""))),
                                                 
                                                 p(HTML(paste("\u03B8", tags$sub(11), ": Probability of eDNA presence given species presence, also referred to as ", strong("stage 1 true positive."), sep = ""))),
                                                 
                                                 p(HTML(paste("\u03B8", tags$sub(10), ": Probability of eDNA presence given species absence, also referred to as ", strong("stage 1 false positive.") ,sep = ""))),
                                                 
                                                 #p(HTML(paste("\u03B8", tags$sub(0), tags$sub(1), ": Probability of eDNA absence given species presence",sep = ""))),
                                                 
                                                 #p(HTML(paste("\u03B8", tags$sub(0), tags$sub(0), ": Probability of eDNA absence species absence",sep = ""))),
                                                 
                                                 p(HTML(paste("p", tags$sub(11), ": Probability of eDNA detection given eDNA presence, also referred to as ", strong("stage 2 true positive."),sep = ""))),
                                                 
                                                 p(HTML(paste("p", tags$sub(10), ": Probability of eDNA detection given eDNA absence, also referred to as ", strong("stage 2 false positive."),sep = ""))),
                                                 
                                                 #p(HTML(paste("p", tags$sub(0), tags$sub(1), ": Probability of not detecting eDNA given eDNA presence",sep = ""))),
                                                 
                                                 #p(HTML(paste("p", tags$sub(0), tags$sub(0), ": Probability of not detecting eDNA eDNA absence",sep = ""))),
                                                 
                                                 p(HTML(paste0("We note that the complements of the probabilities defined above are also estimated",
                                                               #"\u03B8", tags$sub(0), tags$sub(1),"= 1-","\u03B8", tags$sub(1), tags$sub(1),", ",
                                                               #"\u03B8", tags$sub(1), tags$sub(1),"= 1-","\u03B8", tags$sub(1), tags$sub(0),", ",
                                                               #"p", tags$sub(0), tags$sub(1),"= 1 -","p", tags$sub(1), tags$sub(1),", ",
                                                               #"p", tags$sub(1), tags$sub(1),"= 1 -","p", tags$sub(1), tags$sub(0),
                                                               " and we have chosen to work with the probabilities of a positive (either true or false) result, but the conclusions drawn would be unchanged
                                                 if the analysis considered their complements instead.")))
                                        ),
                                        tabPanel(h4("Data"), 
                                                 p("Suppose that there are", strong("S sites"), " and ",strong("M water samples"), "collected from each site, with each sample resulting
in ", strong("K PCR replicates.")),
                                                 
                                                 p("The data need to be formatted in the following way and saved in a CSV file ", strong("before")," uploading them onto the app:"),
                                                 
                                                 p(" -	Create one row for each site and one column for each water sample, with each resulting cell denoting the ",strong("number of positive PCR replicates") ," obtained in each case."),
                                                 
                                                 p(" -	If data on  ",strong("confirmed species presence")," exist, then this information should be included in the column adjacent to the data corresponding to the Mth water sample from all sites, with a 1 indicating confirmed species presence and 0 otherwise for each site."),
                                                 
                                                 p(" -	Any available ",strong("covariates")," should be included in the columns to the right of the confirmed presences (or Mth water sample, as appropriate)."),
                                                 
                                                 p("We suggest", strong("standardising all continuous covariates"), "before uploading the file (subtracting the mean and dividing by the standard deviation of the corresponging column)."),
                                                 
                                                 p("Please note that rows with", strong("missing values"), "in the covariates or the eDNA scores should be removed."),
                                                 
                                                 p("To ",strong("upload your data")," , click on \"Browse...\" under \"Choose CSV file\" in the Data tab."),
                                                 
                                                 p("Please note that if your csv file includes ",strong("headers")," you should tick the Header box."),
                                                 
                                                 p("Once you have uploaded your data, you should check the ",strong("File Preview"),", which shows you the first six rows of your data set."),
                                                 
                                                 p("Indicate whether the data set includes a column with records of confirmed species presences by entering the column number in the corresponding box (use 0 if no such records exist)"),
                                                 
                                                 p("Indicate the columns where data on continuous covariates are stored in the corresponding box (and similarly for categorical covariates)."),
                                                 
                                                 p("Indicate whether you would like to consider interactions between any pairs of covariates in the corresponding interactions box. These should be specified using the standard R notation, eg, if you want to consider an interaction between the covariate included in column 4 and that included in column 5 then you should specify 4:5. Additional pairs of interactions can be considered and pairs should be separated by a comma, eg. 4:5, 4:6 etc."),
                                                 
                                                 p("Scroll to the bottom of the page and check that all three sections of the data set have been created correctly. For example, is the number of
columns in the ",strong("EDNA scores")," box correct (M), is the number of columns in the ",strong("Covariates")," box equal to the number of covariates?"),
                                                 
                                                 p("Once you are happy that everything has been read-in correctly, click on ",strong("Settings."))
                                        ),
                                        tabPanel(h4("Settings"), 
                                                 tags$h4("General settings"),
                                                 
                                                 p("Enter the number of PCR replicates per sample in the corresponding box and select which of the model parameters should be modelled as functions of covariates by ticking the corresponding boxes.
                                     Please refer to the schematic representation of the model for guidance."),
                                                 
                                                 tags$h4("Advanced settings"),
                                                 
                                                 p("Specify the number of iterations that should be discarded (burn-in) and the subsequent number of iterations that should be run. Note that we recommend the default values or more, but not less."),
                                                 
                                                 p("Here you can specify the posterior means for all model parameters (please refer to the Griffin et al. (2019) paper for more details). These are set to the default values and we advise against making changes unless you are confident you know what you are doing!"),
                                                 
                                                 p("Once you are happy that everything is set-up as it should, click on ",strong("Run"),".
                                     A box will appear in the bottom-right corner showing you the iteration number of the algorithm and a message will appear once the results are ready, at which stage you can click on ",strong("Results."))
                                        ),
                                        tabPanel(h4("Results"), 
                                                 p("Here you can find the results of the model fitting, including the results of variable selection, if appropriate."),
                                                 
                                                 p("You can access the results for each of the five model parameters by clicking on the corresponding tab on the left."),
                                                 
                                                 p("For example, clicking on the tab titled ",strong("Probability of species presence"),", you can access posterior summaries (posterior mean and 95% posterior credible intervals) of the site-specific probabilities of species presence (these are saved in a .csv file)."),
                                                 
                                                 p(" If you have performed variable selection on the probability of occupancy, then by clicking on the tab titled ",strong("Baseline
                                     probability of species presence and BVS results")," you can access i) posterior summaries of the baseline occupancy probability, that is the probability of occupancy when all covariate values are equal to 0, ii)  the posterior probabilities of inclusion for each of the available variables; generally we would say that probabilities greater than 0.5 suggest that the corresponding variable has an effect on the parameter, and iii) posterior summaries of the regression coefficients."),
                                                 
                                                 p("Similarly for all other parameters. Note that if you have not performed variable selection on a parameter, then the corresponding tab on BVS will not exist."),
                                                 
                                                 p("You can ",strong("download all of the available results")," by scrolling all the way to the bottom and clicking on Download.")
                                        ),
                                        tabPanel(h4("Diagnostics"), 
                                                 p("As the underlying model is a Bayesian model, we run a Markov Chain Monte Carlo (MCMC) algorithm to obtain inference. 
                                         The downside of MCMC is that for some type of data the algorithm might take a larger number of iterations to obtain meaningful summaries."),
                                                 
                                                 p("When the number of iterations used has not been large enough to reach convergence, a warning message is printed after fitting the model. 
                                         In that case, we suggest running the model again with more iterations."),
                                                 
                                                 p("In practice we have found that 2000 burnin iterations, 2000 iterations and 20 thinned iterations are usually sufficient to obtain sufficiently large effective sample sizes from the posterior distributions of all parameters, but for some data sets it may be necessary to increase these)."),
                                                 
                                                 p("If the user wants to take a more in-depth look at the convergence of the parameters, the diagnostics tab displays plots of the 
                                         main quantities of interest with the effective sample size for each parameter. In this way, the user can observe which parameters 
                                         are slower in convergence."),
                                                 
                                                 p("As convergence for the coefficients is slower to reach because of the variable selection step, we suggest to check for convergence
                                         only for the intercepts.")
                                        ),
                                        tabPanel(h4("Code for additional results"), 
                                                 
                                                 p("Since some results are not added to the results tab, we provide R code to generate them given the download output."),
                                                 
                                                 p("To generate site-specific probabilities of occupancies, eDNA presence given presence-absence, etc, based on the files 'download_psi.csv',
                                         'download_theta11.csv','download_theta10.csv', respectively, the following function, based on the ggplot2 package, can be used."),
                                                 
                                                 
                                                 p(em("createProbabilitiesPlots <- function(data, sites_subset){")),
                                                 
                                                 p(em("data <- data[,1:4]")),
                                                 
                                                 p(em("   S <- nrow(data)")),
                                                 
                                                 p(em("    ggplot() + geom_errorbar(data = NULL, aes(x = factor(sites_subset), ymax=data[sites_subset,4], ymin=data[sites_subset,2]),width=0.2, size=1, color='black') +")),
                                                 
                                                 p(em("geom_point(data = NULL, aes(x = factor(sites_subset), y=data[sites_subset,3]), size=2, shape=21, fill='white') +")),
                                                 
                                                 p(em("theme_bw() + scale_y_continuous(name = '') + xlab('Sites')}")),
                                                 
                                                 p("The plot can then be generated running (for example) the commands:"),
                                                 
                                                 p(em("data <- read.csv(file = 'download_psi.csv')")),
                                                 
                                                 p(em("sites_subset <- c(1,10,40,60)")),
                                                 
                                                 p(em("createProbabilitiesPlots(data, sites_subset)"))
                                                 
                                                 
                                        )
                                      )
                             ),
                             tabPanel(h3("Data"), icon = icon("table"),
                                      fileInput("file1", 
                                                tags$h2("Choose CSV file"),
                                                multiple = FALSE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      checkboxInput("header", "Header", FALSE),
                                      tags$h2("File Preview"),
                                      wellPanel(
                                        tableOutput("contents")
                                      ),
                                      tags$hr(),
                                      fluidRow(
                                        column(width = 4,
                                               tags$br()
                                        ),
                                        column(width = 4,
                                               numericInput("presence_column", "Column of confirmed presences (0 if absent)", 0, min = 0, max = 100)
                                        ),
                                        column(width = 4,
                                               # column(width = 3.2,
                                               #        numericInput("cov_column1", "Start column of covariates (0 if absent)", 0, min = 0, max = 100)
                                               # ),
                                               # column(width = 3.2,
                                               #        numericInput("cov_column2", "End column of covariates (0 if absent)", 0, min = 0, max = 100)
                                               # )
                                               column(width = 3.2,
                                                      textInput("num_covariates", "Columns of continuous covariates (0 if absent)", value = "0")
                                               ),
                                               column(width = 3.2,
                                                      textInput("fac_covariates", "Columns of categorical covariates (0 if absent)", value = "0")
                                               ),
                                               column(width = 3.2,
                                                      textInput("inter_covariates", "Interaction between covariates (write as 1:2 separated by comma, 0 if absent)", value = "0")
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 4,
                                               tags$h3("eDNA scores")
                                        ),
                                        column(width = 4,
                                               tags$h3("Confirmed presences")
                                        ),
                                        column(width = 4,
                                               tags$h3("Covariates")
                                               
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 4,
                                               wellPanel(
                                                 tableOutput("contents1")
                                               )
                                        ),
                                        column(width = 4,
                                               wellPanel(
                                                 tableOutput("contents2")
                                               )
                                        ),
                                        column(width = 4,
                                               wellPanel(
                                                 tableOutput("contents3")
                                               )
                                               
                                        )
                                      )
                             ),
                             tabPanel(h3("Settings"),icon = icon("laptop", lib = "font-awesome"),
                                      fluidRow(column(width = 3,
                                                      tags$h2("General settings"),
                                                      tags$br(),
                                                      numericInput("num_ednaSamples", "Number of independent eDNA qPCR replicates per sample", 12, min = 1, max = 100),
                                                      tags$hr(),
                                                      shinyalert::useShinyalert(),
                                                      actionButton(inputId = "go",
                                                                   label = tags$h4("Run")),
                                                      tags$h2("Advanced settings"),
                                                      tags$br(),
                                                      numericInput("num_burnin", "Number of burn-in iterations", 3000, min = 0, max = 10000),
                                                      numericInput("num_iter", "Number of iterations", 3000, min = 0, max = 10000),
                                                      numericInput("num_chain", "Number of chains", 1, min = 0, max = 10000),
                                                      numericInput("num_thin", "Number of thinned iterations", 1, min = 1, max = 10000)),
                                               column(width = 8,
                                                      tags$h4("Select which probabilities are to be considered as a function of covariates."),
                                                      fluidRow(column(width = 3,
                                                                      # checkboxInput("cov_using1", expression(paste("species presence/absence", psi)), FALSE),
                                                                      checkboxInput("cov_using1", HTML(paste("&#968;",sep = "")), FALSE),
                                                                      checkboxInput("cov_using2", HTML(paste("\u03B8", tags$sub(11),sep = "")), FALSE),
                                                                      checkboxInput("cov_using3", HTML(paste("\u03B8", tags$sub(10),sep = "")), FALSE),
                                                                      checkboxInput("cov_using4", HTML(paste("p", tags$sub(1), tags$sub(1), sep = "")), FALSE),
                                                                      checkboxInput("cov_using5", HTML(paste("p", tags$sub(1), tags$sub(0), sep = "")), FALSE),
                                                                      tags$hr()
                                                      ),
                                                      column(width = 3, 
                                                             # img(src='img0.jpg', width = 650)
                                                             imageOutput("DAG", height = 250)
                                                      )),
                                                      fluidRow(
                                                        tags$h4("Prior distributions"),
                                                        tags$h5("Below we list the means of the prior distributions for all probabilities as specified in the Griffin et al. (2019) paper. 
                                            Please note that you are advised against making substantial changes to these settings unless you fully understand the modelling 
                                                       approach or have consulted with a statistician."),
                                                        column(width = 4,
                                                               numericInput("mu_psi", "Prob. of site occupancy", .5, min = 0, max = 1),
                                                               numericInput("phi_psi_mu", "Variance of prob. of site occupancy", 4, min = 0, max = 10)
                                                        ),
                                                        column(width = 4,
                                                               numericInput("phi_psi_beta", "Variance of coefficients of prob. of site occupancy", 1/4, min = 0, max = 10),
                                                               numericInput("d_bar_prior", "Number of significant covariates", 2, min = 1, max = 100),
                                                        ),
                                                        column(width = 4,
                                                               numericInput("mu_theta11", "Prob. of eDNA presence given species presence", .9, min = 0, max = 1),
                                                               numericInput("mu_theta10", "Prob. of eDNA presence given species absence", .1, min = 0, max = 1),
                                                               numericInput("mu_p11", "Prob. of detecting eDNA given eDNA presence", .9, min = 0, max = 1),
                                                               numericInput("mu_p10", "Prob. of detecting eDNA given eDNA absence", .1, min = 0, max = 1))
                                                        
                                                      )
                                               )
                                      ),
                                      #,
                                      # useShinyjs(),
                                      # conditionalPanel(
                                      #   "false", # always hide the download button
                                      #   downloadButton(outputId = "download_all",
                                      #                  label = tags$h4("Download"))
                                      # ),
                                      
                                      
                             ),
                             tabPanel(h3("Results"), icon = icon("stats", lib = "glyphicon"),
                                      navlistPanel(id = "tabs",
                                                   tabPanel(HTML(paste("&#968;",sep = "")), value = "psiprobtab",
                                                            fluidRow(downloadButton("download_psi", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_psi"),
                                                                     tags$br(),
                                                                     downloadButton("download_psi_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("&#968;"," intercept and BVS results",sep = "")), value = "psitab",
                                                            fluidRow(downloadButton("download_beta_psi", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_beta0_psi"),
                                                                     downloadButton("download_beta0_psi_plot", "Download Plot"),
                                                                     plotOutput("plot_gamma_psi"),
                                                                     downloadButton("download_gamma_psi_plot", "Download Plot"),
                                                                     plotOutput("plot_beta_psi")),
                                                            downloadButton("download_beta_psi_plot", "Download Plot")
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(11),sep = "")), value = "theta11probtab",
                                                            fluidRow(downloadButton("download_theta11", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_theta11"),
                                                                     tags$br(),
                                                                     downloadButton("download_theta11_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(11), " intercept and BVS results",sep = "")),  value = "theta11tab",
                                                            fluidRow(downloadButton("download_beta_theta11", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_beta0_theta11"),
                                                                     downloadButton("download_beta0_theta11_plot", "Download Plot"),
                                                                     plotOutput("plot_gamma_theta11"),
                                                                     downloadButton("download_gamma_theta11_plot", "Download Plot"),
                                                                     plotOutput("plot_beta_theta11"),
                                                                     downloadButton("download_beta_theta11_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(10),sep = "")), value = "theta10probtab",
                                                            fluidRow(downloadButton("download_theta10", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_theta10"),
                                                                     tags$br(),
                                                                     downloadButton("download_theta10_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(10), " intercept and BVS results",sep = "")), value = "theta10tab",
                                                            fluidRow(downloadButton("download_beta_theta10", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_beta0_theta10"),
                                                                     downloadButton("download_beta0_theta10_plot", "Download Plot"),
                                                                     plotOutput("plot_gamma_theta10"),
                                                                     downloadButton("download_gamma_theta10_plot", "Download Plot"),
                                                                     plotOutput("plot_beta_theta10"),
                                                                     downloadButton("download_beta_theta10_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(1), sep = "")), value = "p11probtab",
                                                            fluidRow(downloadButton("download_p11", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_p11"),
                                                                     tags$br(),
                                                                     downloadButton("download_p11_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(1), " intercept and BVS results", sep = "")), value = "p11tab",
                                                            fluidRow(downloadButton("download_beta_p11", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_beta0_p11"),
                                                                     downloadButton("download_beta0_p11_plot", "Download Plot"),
                                                                     plotOutput("plot_gamma_p11"),
                                                                     downloadButton("download_gamma_p11_plot", "Download Plot"),
                                                                     plotOutput("plot_beta_p11"),
                                                                     downloadButton("download_beta_p11_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(0), sep = "")), value = "p10probtab",
                                                            fluidRow(downloadButton("download_p10", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_p10"),
                                                                     tags$br(),
                                                                     downloadButton("download_p10_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(0), " intercept and BVS results", sep = "")), value = "p10tab",
                                                            fluidRow(downloadButton("download_beta_p10", "Download Posterior Summaries"),
                                                                     tags$br(),
                                                                     plotOutput("plot_beta0_p10"),
                                                                     downloadButton("download_beta0_p10_plot", "Download Plot"),
                                                                     plotOutput("plot_gamma_p10"),
                                                                     downloadButton("download_gamma_p10_plot", "Download Plot"),
                                                                     plotOutput("plot_beta_p10"),
                                                                     downloadButton("download_beta_p10_plot", "Download Plot")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("Conditional probabilities tables", sep = "")), value = "condprobtab",
                                                            fluidRow(tableOutput('condProbTable'),
                                                                     downloadButton("download_condprob", "Download Table")
                                                            )
                                                   )
                                                   
                                      ),downloadButton(outputId = "download_all",
                                                       label = tags$h4("Download"))
                                      
                             ),
                             tabPanel(h3("Diagnostics"), icon = icon("stats", lib = "glyphicon"),
                                      navlistPanel(id = "tabs_diag",
                                                   tabPanel(HTML(paste("&#968;",sep = "")),
                                                            fluidRow(plotOutput("plot_diagnostics_beta0_psi"),
                                                                     tableOutput('diagnosticsTablePsi')
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("&#968;"," intercept and BVS results",sep = "")), value = "psitab_diag",
                                                            fluidRow(plotOutput("plot_diagnostics_beta_psi")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(11),sep = "")),
                                                            fluidRow(plotOutput("plot_diagnostics_beta0_theta11"),
                                                                     tableOutput('diagnosticsTableTheta11')
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(11), " intercept and BVS results",sep = "")),  value = "theta11tab_diag",
                                                            fluidRow(plotOutput("plot_diagnostics_beta_theta11")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(10),sep = "")),
                                                            fluidRow(plotOutput("plot_diagnostics_beta0_theta10"),
                                                                     tableOutput('diagnosticsTableTheta10')
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("\u03B8", tags$sub(10), " intercept and BVS results",sep = "")), value = "theta10tab_diag",
                                                            fluidRow(plotOutput("plot_diagnostics_beta_theta10")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(1), sep = "")),
                                                            fluidRow(plotOutput("plot_diagnostics_beta0_p11"),
                                                                     tableOutput('diagnosticsTableP11')
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(1), " intercept and BVS results", sep = "")), value = "p11tab_diag",
                                                            fluidRow(plotOutput("plot_diagnostics_beta_p11")
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(0), sep = "")),
                                                            fluidRow(plotOutput("plot_diagnostics_beta0_p10"),
                                                                     tableOutput('diagnosticsTableP10')
                                                            )
                                                   ),
                                                   tabPanel(HTML(paste("p", tags$sub(1), tags$sub(0), " intercept and BVS results", sep = "")), value = "p10tab_diag",
                                                            fluidRow(plotOutput("plot_diagnostics_beta_p10")
                                                            )
                                                   )
                                      )
                             )
                             #
                             # tabPanel(h3("Traceplots"),icon = icon("book-open", lib = "font-awesome"),
                             #          plotOutput("plot_diagnostics_beta0_psi"),
                             #          plotOutput("plot_diagnostics_beta_psi"),
                             #          plotOutput("plot_diagnostics_beta0_theta11"),
                             #          plotOutput("plot_diagnostics_beta_theta11"),
                             #          plotOutput("plot_diagnostics_beta0_theta10"),
                             #          plotOutput("plot_diagnostics_beta_theta10"),
                             #          plotOutput("plot_diagnostics_beta0_p11"),
                             #          plotOutput("plot_diagnostics_beta_p11"),
                             #          plotOutput("plot_diagnostics_beta0_p10"),
                             #          plotOutput("plot_diagnostics_beta_p10")
                             # )
                           )
                  )
                )
)

server <- function(input, output) {
  
  # EDNA FUNCTIONS
  {
    
    datasetInput <- reactive({
      req(input$file1)
      
      tryCatch(
        {
          df <- read.csv(input$file1$datapath,
                         header = input$header)
        },
        error = function(e) {
          
          stop(safeError(e))
        }
      )
      
      return(df)
    })
    
    ednaSamplesInput <- reactive({
      
      try(
        {
          df <- datasetInput()
          
          column_presence <- input$presence_column
          num_covariates_text <- input$num_covariates
          fac_covariates_text <- input$fac_covariates
          
          num_covariates <- sapply(strsplit(num_covariates_text, split = ",")[[1]], function(x){
            if(grepl("-",x)){
              as.numeric(seq(strsplit(x, split = "-")[[1]][1], strsplit(x, split = "-")[[1]][2]))
            } else {
              as.numeric(x)
            }
          })
          num_covariates <- as.vector(unlist(num_covariates))
          
          fac_covariates <- sapply(strsplit(fac_covariates_text, split = ",")[[1]], function(x){
            if(grepl("-",x)){
              as.numeric(seq(strsplit(x, split = "-")[[1]][1], strsplit(x, split = "-")[[1]][2]))
            } else {
              as.numeric(x)
            }
          })
          fac_covariates <- as.vector(unlist(fac_covariates))
          
          column_covariate <- c(num_covariates,fac_covariates)
          
          indexesToRemove <- c()
          
          if(column_presence != 0){
            indexesToRemove <- c(indexesToRemove, column_presence)
          }
          
          if(any(num_covariates != 0) | any(fac_covariates != 0)){
            indexesToRemove <- c(indexesToRemove, column_covariate)
          }
          
          if(any(column_covariate != 0) | column_presence != 0){
            df <- df[,-indexesToRemove]  
          }
        },
        silent = T
      )
      
      # try({
      #   
      #   
      # })
      
      return(df)
    })
    
    presenceInput <- reactive({
      
      try(
        {
          df <- datasetInput()
          
          column_presence <- input$presence_column
          
          return(df[,column_presence])
        },
        silent = T
      )
      
    })
    
    covariatesInput <- reactive({
      
      try({
        df <- datasetInput()
        
        num_covariates_text <- input$num_covariates
        fac_covariates_text <- input$fac_covariates
        
        num_covariates <- sapply(strsplit(num_covariates_text, split = ",")[[1]], function(x){
          if(grepl("-",x)){
            as.numeric(seq(strsplit(x, split = "-")[[1]][1], strsplit(x, split = "-")[[1]][2]))
          } else {
            as.numeric(x)
          }
        })
        num_covariates <- as.vector(unlist(num_covariates))
        
        fac_covariates <- sapply(strsplit(fac_covariates_text, split = ",")[[1]], function(x){
          if(grepl("-",x)){
            as.numeric(seq(strsplit(x, split = "-")[[1]][1], strsplit(x, split = "-")[[1]][2]))
          } else {
            as.numeric(x)
          }
        })
        fac_covariates <- as.vector(unlist(fac_covariates))
        
        column_covariate <- sort(c(num_covariates,fac_covariates))
        
        if(any(column_covariate != 0)){
          df <- df[,column_covariate,drop = FALSE]
        } else {
          df <- data.frame(c())
        }
        
        return(df)
      },
      silent = T
      )
      
    })
    
    output$contents <- renderTable({
      
      # tryCatch({
      df <- datasetInput()
      
      colnames(df) <- 1:ncol(df)
      
      return(head(df))  
      # })
      
    }, colnames  = TRUE)
    
    output$contents1 <- renderTable({
      
      # tryCatch({
      df <- ednaSamplesInput()
      
      return(head(df))
      # })
      
    }, colnames  = FALSE)
    
    output$contents2 <- renderTable({
      
      # tryCatch({
      column_presence <- input$presence_column
      
      if(column_presence != 0){
        df <- presenceInput()
      } else {
        df <- data.frame(c())
      }
      
      return(head(df))  
      # })
      
    }, colnames  = FALSE)
    
    output$contents3 <- renderTable({
      
      # tryCatch({
      df <- covariatesInput()
      
      return(head(df))  
      # })
      
      
    }, colnames  = FALSE)
    
    rv <- reactiveValues(results = c())
    
    rv_eDNA_plots <- reactiveValues(results = c())
    
    rv_eDNA_download <- reactiveValues(results = c())
    
    observeEvent(input$go, {
      
      # input data
      {
        data <- datasetInput()
        
        y_sm <- as.matrix(ednaSamplesInput())
        
        S <- nrow(y_sm)
        M <- ncol(y_sm)
        
        K <- as.numeric(input$num_ednaSamples)
        
        column_presence <- input$presence_column
        if(column_presence != 0){
          k_s <- as.vector(presenceInput())
        } else {
          k_s <- rep(0, S)
        }
        
        num_covariates_text <- input$num_covariates
        fac_covariates_text <- input$fac_covariates
        inter_covariates_text <- input$inter_covariates
        
        usingCov <- rep(F, 5)
        if(input$cov_using1){
          usingCov[1] <- T
        }
        if(input$cov_using2){
          usingCov[2] <- T
        }
        if(input$cov_using3){
          usingCov[3] <- T
        }
        if(input$cov_using4){
          usingCov[4] <- T
        }
        if(input$cov_using5){
          usingCov[5] <- T
        }
      }
      
      # clean data
      {
        num_covariates <- ExtractCovariatesFromText(num_covariates_text)
        fac_covariates <- ExtractCovariatesFromText(fac_covariates_text)
        
        column_covariate <- sort(c(num_covariates,fac_covariates))
        
        # create design matrix
        if(length(column_covariate) != 0){ # if there are covariates
          X <- data[,column_covariate,drop = F]
          nameVariables <- colnames(X)
          ncov <- ncol(X)
        } else {
          X <- as.matrix(rep(1, S))
          nameVariables <- colnames(X)
          ncov <- 0
        }
        
        if(length(column_covariate) != 0){ # if there are covariates available
          
          classCovariates <- ExtractClassCovariates(ncov, fac_covariates, column_covariate)
          
          # convert the categorical columns to factors
          for (i in 1:ncov) {
            if(!classCovariates[i]){
              X[,i] <- as.factor(X[,i])
            }
          }
          
        }
        
        # vector that assign to each dummy variable of the complete design matrix the 
        # index of its corresponding covariate (including intercept)
        indexes_covariates <- Extract_IndexesCovariates(X, column_covariate, ncov, classCovariates)
        
        originalX <- X # backup the original  matrix of covariates
        if(length(column_covariate) != 0){
          
          X <- model.matrix(~ ., X)
          
        } 
        
        ncov_noninter <- ncov
        
        if(inter_covariates_text != "0"){
          
          interactions <- ExtractInteractions(inter_covariates_text)
          
          list_inter <- createInteractionMatrix(interactions, column_covariate, originalX)
          X_inter <- list_inter$matrix
          namesVariables_inter <- list_inter$namesVariables
          
          indexes_interactions <- max(indexes_covariates) + 
            Extract_IndexesInteractions(interactions, originalX, num_covariates, fac_covariates,
                                        column_covariate, classCovariates)
          
          indexes_covariates <- c(indexes_covariates, indexes_interactions)
          
          X <- cbind(X, X_inter)
          
          numInteractions <- length(indexes_interactions)
          
          ncov <- ncov + length(namesVariables_inter)
          nameVariables <- c(nameVariables, namesVariables_inter)
          
        } else {
          numInteractions <- 0
        }
        
      }
      
      # chains and iterations
      nchain <- input$num_chain
      nburn <- input$num_burnin
      niter <- input$num_iter
      nthin <- input$num_thin
      
      rv$num_iter <- niter
      rv$num_chain <- nchain
      
      nstep <- 100
      
      # input priors
      {
        
        prior_psi <- input$mu_psi
        prior_theta11 <- input$mu_theta11
        prior_theta10 <- input$mu_theta10
        prior_p11 <- input$mu_p11
        prior_p10 <- input$mu_p10
        
        phi_mu <- input$phi_psi_mu
        phi_beta <- input$phi_psi_beta
        
        a_pi <- 1
        b_pi <- 1
        
        d_bar <- input$d_bar_prior        
        d_bar <- ifelse(d_bar <= ncov, d_bar, 2)
        
        epsilon <- pnorm(1)
        
        alpha0_theta <- 1
        alpha0_p <- 1
        
        delta <- qnorm(epsilon)
        
        mu_psi <- invLogit(prior_psi)
        mu_theta11 <- invLogit(prior_theta11)
        mu_theta10 <- invLogit(prior_theta10)
        mu_p11 <- invLogit(prior_p11)
        mu_p10 <- invLogit(prior_p10)
        
        # compute C matrix
        if(length(column_covariate) != 0){
          
          C <- matrix(0, nrow = ncol(X) - 1, ncol = ncol(X) - 1)
          l <- 0 # l loop through the covariates 
          for (i in 1:ncov_noninter) {
            if(classCovariates[i]){ # if it's a numerical covariates
              C[l + 1, l + 1] <- 1
              l <- l + 1
            } else { # if it's a categorical covariates
              num_levels <- length(unique(originalX[,i]))
              C[l + 1:(num_levels-1), l + 1:(num_levels-1)] <- .5 * (diag(1, nrow = (num_levels-1)) + 
                                                                       matrix(1, nrow = (num_levels-1), ncol = (num_levels-1)))
              l <- l + num_levels - 1
            }
          }
          
          Sigma <- cov(X[,-1, drop = F])
          sumCSigma <- sum(C * Sigma)
          
        }
        
        for (i in seq_len(numInteractions)) {
          C[l  + 1, l + 1] <- 1
          l <- l + 1
        }
        
        # prior for psi
        
        if(usingCov[1]){ 
          
          {
            b_psi <- c(mu_psi, rep(0, ncol(X) - 1))
            sigma_beta_psi <- C * phi_beta  
            
            B_psi <- matrix(0, nrow = ncol(X), ncol = ncol(X))
            B_psi[1, 1] <- phi_mu
            B_psi[2:(ncol(X)), 2:(ncol(X))] <- sigma_beta_psi
          }
          
        } else {
          
          {
            b_psi <- mu_psi
            B_psi <- matrix(0, nrow = 1, ncol = 1)
            B_psi[1, 1] <- phi_mu  
          }
          
        }
        
        # prior for theta11
        
        if(usingCov[2]){
          
          delta_theta <- (mu_theta11 - mu_theta10)^2 / (2 * delta^2 * (alpha0_theta + sumCSigma))
          sigma_theta11 <- alpha0_theta * delta_theta
          sigma_beta_theta11 <- C * delta_theta
          
          b_theta11 <- c(mu_theta11, rep(0, ncol(X) - 1))
          B_theta11 <- matrix(0, nrow = ncol(X), ncol = ncol(X))
          B_theta11[1, 1] <- sigma_theta11
          B_theta11[2:(ncol(X)), 2:(ncol(X))] <- sigma_beta_theta11
          
        } else {
          
          delta_theta <- (mu_theta11 - mu_theta10)^2 / (2 * delta^2)
          
          b_theta11 <- mu_theta11
          B_theta11 <- matrix(0, nrow = 1, ncol = 1)
          B_theta11[1, 1] <- delta_theta
          
        }
        
        # prior for theta10
        
        if(usingCov[3]){
          
          delta_theta <- (mu_theta11 - mu_theta10)^2 / (2 * delta^2 * (alpha0_theta + sumCSigma))
          sigma_theta10 <- alpha0_theta * delta_theta
          sigma_beta_theta10 <- C * delta_theta
          
          b_theta10 <- c(mu_theta10, rep(0, ncol(X) - 1))
          B_theta10 <- matrix(0, nrow = ncol(X), ncol = ncol(X))
          B_theta10[1, 1] <- sigma_theta10
          B_theta10[2:(ncol(X)), 2:(ncol(X))] <- sigma_beta_theta10
          
        } else {
          
          delta_theta <- (mu_theta11 - mu_theta10)^2 / (2 * delta^2)
          
          b_theta10 <- mu_theta10
          B_theta10 <- matrix(0, nrow = 1, ncol = 1)
          B_theta10[1, 1] <- delta_theta
          
        }
        
        # prior for p11
        
        if(usingCov[4]){
          
          delta_p <- (mu_p11 - mu_p10)^2 / (2 * delta^2 * (alpha0_p + sumCSigma))
          sigma_p11 <- alpha0_p * delta_p
          sigma_beta_p11 <- C * delta_p
          
          b_p11 <- c(mu_p11, rep(0, ncol(X) - 1))
          B_p11 <- matrix(0, nrow = ncol(X), ncol = ncol(X))
          B_p11[1, 1] <- sigma_p11
          B_p11[2:(ncol(X)), 2:(ncol(X))] <- sigma_beta_p11
          
          
        } else {
          
          delta_p <- (mu_p11 - mu_p10)^2 / (2 * delta^2)
          
          b_p11 <- mu_p11
          B_p11 <- matrix(0, nrow = 1, ncol = 1)
          B_p11[1, 1] <- delta_p
          
        }
        
        # prior for p10
        
        if(usingCov[5]){
          
          delta_p <- (mu_p11 - mu_p10)^2 / (2 * delta^2 * (alpha0_p + sumCSigma))
          sigma_p10 <- alpha0_p * delta_p
          sigma_beta_p10 <- C * delta_p
          
          b_p10 <- c(mu_p10, rep(0, ncol(X) - 1))
          B_p10 <- matrix(0, nrow = ncol(X), ncol = ncol(X))
          B_p10[1, 1] <- sigma_p10
          B_p10[2:(ncol(X)), 2:(ncol(X))] <- sigma_beta_p10
          
        } else {
          
          delta_p <- (mu_p11 - mu_p10)^2 / (2 * delta^2)
          
          b_p10 <- mu_p10
          B_p10 <- matrix(0, nrow = 1, ncol = 1)
          B_p10[1, 1] <- delta_p
          
        }
        
      }
      
      if(!usingCov[1]){
        hideTab(inputId = "tabs", target = "psitab")
        hideTab(inputId = "tabs_diag", target = "psitab_diag")
      } else {
        hideTab(inputId = "tabs", target = "psiprobtab")
      }
      if(!usingCov[2]){
        hideTab(inputId = "tabs", target = "theta11tab")
        hideTab(inputId = "tabs_diag", target = "theta11tab_diag")
      } else {
        hideTab(inputId = "tabs", target = "theta11probtab")
      }
      if(!usingCov[3]){
        hideTab(inputId = "tabs", target = "theta10tab")
        hideTab(inputId = "tabs_diag", target = "theta10tab_diag")
      } else {
        hideTab(inputId = "tabs", target = "theta10probtab")
      }
      if(!usingCov[4]){
        hideTab(inputId = "tabs", target = "p11tab")
        hideTab(inputId = "tabs_diag", target = "p11tab_diag")
      } else {
        hideTab(inputId = "tabs", target = "p11probtab")
      }
      if(!usingCov[5]){
        hideTab(inputId = "tabs", target = "p10tab")
        hideTab(inputId = "tabs_diag", target = "p10tab_diag")
      } else {
        hideTab(inputId = "tabs", target = "p10probtab")
      }
      
      # initialize output
      {
        z_output <- array(0, dim = c(nchain, niter, S))
        psi_output <- array(NA,  dim = c(nchain, niter, S))
        gamma_psi_output <- array(NA, dim = c(nchain , niter, ncov),
                                  dimnames = list(c(), c(), nameVariables))
        beta_psi_output <- array(NA, dim = c(nchain , niter, length(indexes_covariates)),
                                 dimnames = list(c(), c(), colnames(X)))
        theta11_output <- array(NA,  dim = c(nchain, niter, S))
        theta10_output <- array(NA,  dim = c(nchain, niter, S))
        beta_theta11_output <- array(NA, dim = c(nchain , niter, length(indexes_covariates)),
                                     dimnames = list(c(), c(), colnames(X)))
        beta_theta10_output <- array(NA, dim = c(nchain , niter, length(indexes_covariates)),
                                     dimnames = list(c(), c(), colnames(X)))
        gamma_theta11_output <- array(NA, dim = c(nchain , niter, ncov),
                                      dimnames = list(c(), c(), nameVariables))
        gamma_theta10_output <- array(NA, dim = c(nchain , niter, ncov),
                                      dimnames = list(c(), c(), nameVariables))
        p11_output <- array(NA,  dim = c(nchain, niter, S))
        p10_output <- array(NA,  dim = c(nchain, niter, S))
        beta_p11_output <- array(NA, dim = c(nchain , niter, length(indexes_covariates)),
                                 dimnames = list(c(), c(), colnames(X)))
        beta_p10_output <- array(NA, dim = c(nchain , niter, length(indexes_covariates)),
                                 dimnames = list(c(), c(), colnames(X)))
        gamma_p11_output <- array(NA, dim = c(nchain , niter, ncov),
                                  dimnames = list(c(), c(), nameVariables))
        gamma_p10_output <- array(NA, dim = c(nchain , niter, ncov),
                                  dimnames = list(c(), c(), nameVariables))
        
        oneminpsix <- array(NA, dim = c(nchain , niter, K + 1))
        qx <- array(NA, dim = c(nchain , niter, K + 1))
      }
      
      withProgress(message = 'Running', value = 0, min = 0, max = 1, {
        
        for (chain in 1:nchain) {
          
          # initialize parameters
          {
            
            # initialize beta (matrix of coefficients) and gamma (indicator variables of covariates in the model)
            # please note gamma includes also an index for the intercept, which obviously is always 1
            beta <- matrix(0, nrow = 5, ncol = length(indexes_covariates))
            gamma <- matrix(rbinom((ncov + 1) * 5, 1, d_bar / ncov), nrow = 5, ncol = ncov + 1)
            gamma[,1] <- 1 # index of the intercept
            
            if(usingCov[1]){ # if covariates are used
              beta[1,1] <- mu_psi
              X_gamma <- computeXgamma(X, indexes_covariates, gamma[1,])
              beta_gamma <- compute_betagamma(beta[1,,drop = F], indexes_covariates, gamma[1,])
              psi <- as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
            } else { 
              beta[1,1] <- mu_psi
              psi <- rep(logit(mu_psi), S)
            }
            
            if(usingCov[2]){
              beta[2,1] <- mu_theta11
              X_gamma <- computeXgamma(X, indexes_covariates, gamma[2,])
              beta_gamma <- compute_betagamma(beta[2,,drop = F], indexes_covariates, gamma[2,])
              theta11 <- as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
            } else {
              beta[2,1] <- mu_theta11
              theta11 <- rep(logit(mu_theta11), S)
            }
            
            if(usingCov[3]){
              beta[3,1] <- mu_theta10
              X_gamma <- computeXgamma(X, indexes_covariates, gamma[3,])
              beta_gamma <- compute_betagamma(beta[3,,drop = F], indexes_covariates, gamma[3,])
              theta10 <- as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
            } else {
              beta[3,1] <- mu_theta10
              theta10 <- rep(logit(mu_theta10), S)
            }
            
            if(usingCov[4]){
              beta[4,1] <- mu_p11
              X_gamma <- computeXgamma(X, indexes_covariates, gamma[4,])
              beta_gamma <- compute_betagamma(beta[4,,drop = F], indexes_covariates, gamma[4,])
              p11 <- as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
            } else {
              beta[4,1] <- mu_p11
              p11 <- rep(logit(mu_p11), S)
            }
            
            if(usingCov[5]){
              beta[5,1] <- mu_p10
              X_gamma <- computeXgamma(X, indexes_covariates, gamma[5,])
              beta_gamma <- compute_betagamma(beta[5,,drop = F], indexes_covariates, gamma[5,])
              p10 <- as.vector(logit(X_gamma %*% as.matrix(beta_gamma)))
            } else {
              beta[5,1] <- mu_p10
              p10 <- rep(logit(mu_p10), S)
            }
            
            pi <- 0.5
            
            w_sm <- matrix(NA, nrow = S, ncol = M)
            for (s in 1:S) {
              for (m in 1:M) {
                if(!is.na(y_sm[s, m])){
                  if(y_sm[s, m] >= floor(K * p11[s])){
                    w_sm[s, m] <-  1
                  } else {
                    w_sm[s, m] <-  0
                  }  
                } else {
                  w_sm[s, m] <-  NA
                }
              }
            }
            
            # initialize the occupied site as the confirmed presences
            z <- k_s
            
          }
          
          for (iter in seq_len(nburn + niter*nthin)) {
            
            if(iter %% ((nchain * (niter + nburn*nthin)) / nstep) == 0){
              if(iter > nburn & (iter - nburn) %% nthin == 0){
                incProgress(1 / nstep, detail = paste0(paste0("Chain ", chain), paste0(" - Iteration ", (iter - nburn)/nthin)))  
              } else {
                incProgress(1 / nstep, detail = paste0(paste0("Chain ", chain), paste0(" - Burn-in Iteration ", iter)))
              }
            }
            
            # sample z
            z <- sample_z(k_s, w_sm, psi, theta11, theta10, pi)
            
            # sample wsm
            w_sm <- sample_wsm(theta11, z, theta10, p11, p10, y_sm, K)
            
            # sample z and w_sm
            # list_z_wsm <- sample_z_wsm(psi, theta11, theta10, k_s, p11, p10, y_sm, K, pi)
            # w_sm <- list_z_wsm$w_sm
            # z <- list_z_wsm$z
            
            # sample p
            pi <- rbeta(1, 1 + sum(k_s * z), 1 + sum(z * (1 - k_s)))
            
            # sample psi
            list_psi <- update_psi(beta, gamma, z, X, indexes_covariates, b_psi, B_psi, usingCov[1], d_bar)
            psi <- list_psi$psi
            beta[1,] <- list_psi$beta
            if(usingCov[1]){
              gamma[1,] <- list_psi$gamma
            }
            
            # sample theta11 (only if there are > 0 occupied site)
            if(sum(z) != 0){
              list_theta11 <- update_theta(beta, gamma, w_sm, z, X, indexes_covariates, T, b_theta11, B_theta11, 
                                           usingCov[2], d_bar)
              theta11 <- list_theta11$theta
              beta[2,] <- list_theta11$beta
              if(usingCov[2]){
                gamma[2,] <- list_theta11$gamma
              }
              
            }
            
            # sample theta10 (only if there are > 0 unoccupied site)
            if(sum(z) != S){
              list_theta10 <- update_theta(beta, gamma, w_sm, z, X, indexes_covariates, F, b_theta10, B_theta10, 
                                           usingCov[3], d_bar)
              theta10 <- list_theta10$theta
              beta[3,] <- list_theta10$beta
              if(usingCov[3]){
                gamma[3,] <- list_theta10$gamma
              }
            }
            
            # sample p11 (only if there are > 0 sites with edna presences)
            if(sum(w_sm, na.rm = T) > 0){
              list_p11 <- update_p(beta, gamma, y_sm, w_sm, z, X, indexes_covariates, T, K, b_p11, B_p11, 
                                   usingCov[4], d_bar)
              p11 <- list_p11$p
              beta[4,] <- list_p11$beta
              if(usingCov[4]){
                gamma[4,] <- list_p11$gamma
              }
            }
            
            # sample p10 (only if there are > 0 sites with no edna presences)
            if(sum(w_sm, na.rm = T) != (S * M)){
              list_p10 <- update_p(beta, gamma, y_sm, w_sm, z, X, indexes_covariates, F, K, b_p10, B_p10, 
                                   usingCov[5], d_bar)
              p10 <- list_p10$p
              beta[5,] <- list_p10$beta
              if(usingCov[5]){
                gamma[5, ] <- list_p10$gamma
              }
            }
            
            if(iter > nburn & (iter - nburn) %% nthin == 0){
              trueIter <- (iter - nburn)/nthin
              z_output[chain,trueIter,] <- z
              psi_output[chain, trueIter,] <- psi
              beta_psi_output[chain,trueIter,] <- beta[1,]
              gamma_psi_output[chain,trueIter,] <- gamma[1,-1]
              theta11_output[chain, trueIter,] <- theta11
              theta10_output[chain, trueIter,] <- theta10
              beta_theta11_output[chain,trueIter,] <- beta[2,]
              beta_theta10_output[chain,trueIter,] <- beta[3,]
              gamma_theta11_output[chain,trueIter,] <- gamma[2,-1]
              gamma_theta10_output[chain,trueIter,] <- gamma[3,-1]
              p11_output[chain, trueIter,] <- p11
              p10_output[chain, trueIter,] <- p10
              beta_p11_output[chain,trueIter,] <- beta[4,]
              beta_p10_output[chain,trueIter,] <- beta[5,]
              gamma_p11_output[chain,trueIter,] <- gamma[4,-1]
              gamma_p10_output[chain,trueIter,] <- gamma[5,-1]
              
              # compute 1 - psi(x) and q(x)
              psi_0 <- logit(beta[1,1])
              theta11_0 <- logit(beta[2,1])
              theta10_0 <- logit(beta[3,1])
              p11_0 <- logit(beta[4,1])
              p10_0 <- logit(beta[5,1])
              pxgivenzequal1 <- psi_0 * ((theta11_0) * dbinom(0:K, K, prob = p11_0) + (1 - theta11_0) * dbinom(0:K, K, prob = p10_0))
              pxgivenzequal0 <- (1 - psi_0) * ((theta10_0) * dbinom(0:K, K, prob = p11_0) + (1 - theta10_0) * dbinom(0:K, K, prob = p10_0) )
              
              oneminpsix[chain,trueIter,] <- pxgivenzequal0 / (pxgivenzequal1 + pxgivenzequal0)
              qx[chain,trueIter,] <- dbinom(0:K, K, p11_0) * (theta11_0) + dbinom(0:K, K, p10_0) * (1 - theta11_0)
              
            }
            
          }
          
        }
        
      })
      
      for (chain in 1:nchain) {
        for (i in seq_along(indexes_covariates)) {
          beta_psi_output[chain,gamma_psi_output[chain,,indexes_covariates[i]-1]==0,i] <- 0
          beta_theta11_output[chain,gamma_theta11_output[chain,,indexes_covariates[i]-1]==0,i] <- 0
          beta_theta10_output[chain,gamma_theta10_output[chain,,indexes_covariates[i]-1]==0,i] <- 0
          beta_p11_output[chain,gamma_p11_output[chain,,indexes_covariates[i]-1]==0,i] <- 0
          beta_p10_output[chain,gamma_p10_output[chain,,indexes_covariates[i]-1]==0,i] <- 0
        }  
      }
      
      beep()
      
      rv$results <- list("z_output" = z_output,
                         "psi_output" = psi_output,
                         "beta_psi_output" = beta_psi_output,
                         "gamma_psi_output" = gamma_psi_output,
                         "theta11_output" = theta11_output,
                         "theta10_output" = theta10_output,
                         "beta_theta11_output" = beta_theta11_output,
                         "beta_theta10_output" = beta_theta10_output,
                         "gamma_theta11_output" = gamma_theta11_output,
                         "gamma_theta10_output" = gamma_theta10_output,
                         "p11_output" = p11_output,
                         "p10_output" = p10_output,
                         "beta_p11_output" = beta_p11_output,
                         "beta_p10_output" = beta_p10_output,
                         "gamma_p11_output" = gamma_p11_output,
                         "gamma_p10_output" = gamma_p10_output,
                         "indexes_covariates" = indexes_covariates)
      # print("ok bello here")
      # create plots
      {
        
        list_psi_plot <- createPlots(psi_output, beta_psi_output, gamma_psi_output, indexes_covariates,
                                     usingCov[1], S, niter, nchain, "\u03C8", "Baseline occupancy probability")
        psi_plot <- list_psi_plot$data_plot
        beta0_psi_plot <- list_psi_plot$beta0_data_plot
        beta_psi_plot <- list_psi_plot$beta_data_plot
        gamma_psi_plot <- list_psi_plot$gamma_data_plot
        
        list_theta11_plot <- createPlots(theta11_output, beta_theta11_output, gamma_theta11_output, indexes_covariates,
                                         usingCov[2], S, niter, nchain, expression(theta[1][1]), 
                                         "Baseline eDNA presence probability given species presence")
        theta11_plot <- list_theta11_plot$data_plot
        beta0_theta11_plot <- list_theta11_plot$beta0_data_plot
        beta_theta11_plot <- list_theta11_plot$beta_data_plot
        gamma_theta11_plot <- list_theta11_plot$gamma_data_plot
        
        list_theta10_plot <- createPlots(theta10_output, beta_theta10_output, gamma_theta10_output, indexes_covariates,
                                         usingCov[3], S, niter, nchain, expression(theta[1][0]), 
                                         "Baseline eDNA presence probability given species absence")
        theta10_plot <- list_theta10_plot$data_plot
        beta0_theta10_plot <- list_theta10_plot$beta0_data_plot
        beta_theta10_plot <- list_theta10_plot$beta_data_plot
        gamma_theta10_plot <- list_theta10_plot$gamma_data_plot
        
        list_p11_plot <- createPlots(p11_output, beta_p11_output, gamma_p11_output, indexes_covariates,
                                     usingCov[4], S, niter, nchain, expression(p[11]), 
                                     "Baseline eDNA detection probability given eDNA presence")
        p11_plot <- list_p11_plot$data_plot
        beta0_p11_plot <- list_p11_plot$beta0_data_plot
        beta_p11_plot <- list_p11_plot$beta_data_plot
        gamma_p11_plot <- list_p11_plot$gamma_data_plot
        
        list_p10_plot <- createPlots(p10_output, beta_p10_output, gamma_p10_output, indexes_covariates,
                                     usingCov[5], S, niter, nchain, expression(p[10]), 
                                     "Baseline eDNA detection probability given eDNA absence")
        p10_plot <- list_p10_plot$data_plot
        beta0_p10_plot <- list_p10_plot$beta0_data_plot
        beta_p10_plot <- list_p10_plot$beta_data_plot
        gamma_p10_plot <- list_p10_plot$gamma_data_plot
        
      }
      # print("ok bello here 2")
      rv_eDNA_plots$results <- list("psi_plot" = psi_plot,
                                    "beta0_psi_plot" = beta0_psi_plot,
                                    "beta_psi_plot" = beta_psi_plot,
                                    "gamma_psi_plot" = gamma_psi_plot,
                                    "theta11_plot" = theta11_plot,
                                    "beta0_theta11_plot" = beta0_theta11_plot,
                                    "beta_theta11_plot" = beta_theta11_plot,
                                    "gamma_theta11_plot" = gamma_theta11_plot,
                                    "theta10_plot" = theta10_plot,
                                    "beta0_theta10_plot" = beta0_theta10_plot,
                                    "beta_theta10_plot" = beta_theta10_plot,
                                    "gamma_theta10_plot" = gamma_theta10_plot,
                                    "p11_plot" = p11_plot,
                                    "beta0_p11_plot" = beta0_p11_plot,
                                    "beta_p11_plot" = beta_p11_plot,
                                    "gamma_p11_plot" = gamma_p11_plot,
                                    "p10_plot" = p10_plot,
                                    "beta0_p10_plot" = beta0_p10_plot,
                                    "beta_p10_plot" = beta_p10_plot,
                                    "gamma_p10_plot" = gamma_p10_plot)
      # print("ok bello here 2")
      # create results
      {
        
        # download psi
        {
          psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            psi_output2[(chain - 1)*niter + 1:niter,] <- psi_output[chain,,]
          }
          
          psi_means <- apply(psi_output2, 2, mean)
          ci95_psi <- apply(psi_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_psi <- apply(psi_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df <- data.frame("Site" = 1:S)
          
          column_presence <- input$presence_column
          if(column_presence != 0){
            k_s <- as.vector(presenceInput())
          } else {
            k_s <- rep(0, S)
          }
          
          df$`occupancy_probability_2.5Credible Interval` <- ci25_psi
          df$`occupancy_Posterior Mean` <- psi_means
          df$`occupancy_97.5Credible Interval` <- ci95_psi
          
          #
          
          z_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            z_output2[(chain - 1)*niter + 1:niter,] <- z_output[chain,,]
          }
          
          z_means <- apply(z_output2, 2, mean)
          ci95_z <- apply(z_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_z <- apply(z_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df_z <- data.frame("Site"= 1:S)
          
          column_presence <- input$presence_column
          if(column_presence != 0){
            k_s <- as.vector(presenceInput())
          } else {
            k_s <- rep(0, S)
          }
          z_means[k_s == 1] <- 1
          ci25_z[k_s == 1] <- 1
          ci95_z[k_s == 1] <- 1
          
          df_z$`presence_2.5Credible Interval` <- ci25_z
          df_z$`presence_Posterior Mean` <- z_means
          df_z$`presence_97.5Credible Interval` <- ci95_z
          
          df_z$Site <- NULL
          
          
          download_psi <- cbind(df, df_z)
          
        }
        
        # download beta psi
        {
          nchain <- dim(beta_psi_output)[1]
          niter <- dim(beta_psi_output)[2]
          ncov <- dim(gamma_psi_output)[3] 
          ncov_all <- dim(beta_psi_output)[3]
          
          beta_psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          for (chain in 1:nchain) {
            beta_psi_output2[(chain - 1)*niter + 1:niter,] <- beta_psi_output[chain,,]
          }
          gamma_psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          for (chain in 1:nchain) {
            gamma_psi_output2[(chain - 1)*niter + 1:niter,] <- gamma_psi_output[chain,,]
          }
          
          beta_psi_output2[,1] <- logit(beta_psi_output2[,1])
          colnames(beta_psi_output2) <- dimnames(beta_psi_output)[[3]]
          
          beta_psi_mean <- sapply(1:ncol(beta_psi_output2), function(i){
            if(i == 1){
              mean(beta_psi_output2[,i])
            } else {
              mean(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i]-1]!=0,i])  
            }
          })
          ci95_psi <- sapply(1:ncol(beta_psi_output2), function(i){
            if(i == 1){
              quantile(beta_psi_output2[,i], probs = .975)  
            } else {
              quantile(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
            }
          })
          ci25_psi <- sapply(1:ncol(beta_psi_output2), function(i){
            if(i == 1){
              quantile(beta_psi_output2[,i], probs = .025)
            } else {
              quantile(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
            }
          })
          
          df <- data.frame("2.5Credible Interval" = ci25_psi)
          colnames(df)[1] <- "2.5Credible Interval"
          df$`Posterior Mean` <- beta_psi_mean
          df$`97.5Credible Interval` <- ci95_psi
          df$PIP <- c(1,apply(gamma_psi_output2, 2, mean)[(indexes_covariates-1)[-1]])
          
          row.names(df) <- colnames(beta_psi_output2)
          
          download_beta_psi <- df
        }
        
        # download theta11
        {
          theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            theta11_output2[(chain - 1)*niter + 1:niter,] <- theta11_output[chain,,]
          }
          
          theta11_means <- apply(theta11_output2, 2, mean)
          ci95_theta11 <- apply(theta11_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_theta11 <- apply(theta11_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df <- data.frame("Site" = 1:S)
          
          df$`2.5Credible Interval` <- ci25_theta11
          df$`Posterior Mean` <- theta11_means
          df$`97.5Credible Interval` <- ci95_theta11
          
          download_theta11 <- df
        }
        
        # download beta_theta11
        {
          beta_theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          for (chain in 1:nchain) {
            beta_theta11_output2[(chain - 1)*niter + 1:niter,] <- beta_theta11_output[chain,,]
          }
          gamma_theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          for (chain in 1:nchain) {
            gamma_theta11_output2[(chain - 1)*niter + 1:niter,] <- gamma_theta11_output[chain,,]
          }
          
          beta_theta11_output2[,1] <- logit(beta_theta11_output2[,1])
          colnames(beta_theta11_output2) <- dimnames(beta_theta11_output)[[3]]
          
          beta_theta11_mean <- sapply(1:ncol(beta_theta11_output2), function(i){
            if(i == 1){
              mean(beta_theta11_output2[,i])
            } else {
              mean(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i]-1]!=0,i])  
            }
          })
          ci95_theta11 <- sapply(1:ncol(beta_theta11_output2), function(i){
            if(i == 1){
              quantile(beta_theta11_output2[,i], probs = .975)  
            } else {
              quantile(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
            }
          })
          ci25_theta11 <- sapply(1:ncol(beta_theta11_output2), function(i){
            if(i == 1){
              quantile(beta_theta11_output2[,i], probs = .025)
            } else {
              quantile(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
            }
          })
          
          df <- data.frame("2.5Credible Interval" = ci25_theta11)
          colnames(df)[1] <- "2.5Credible Interval"
          df$`Posterior Mean` <- beta_theta11_mean
          df$`97.5Credible Interval` <- ci95_theta11
          df$PIP <- c(1,apply(gamma_theta11_output2, 2, mean)[(indexes_covariates-1)[-1]])
          
          row.names(df) <- colnames(beta_theta11_output2)
          
          download_beta_theta11 <- df
        }
        
        # download theta10
        {
          theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            theta10_output2[(chain - 1)*niter + 1:niter,] <- theta10_output[chain,,]
          }
          
          theta10_means <- apply(theta10_output2, 2, mean)
          ci95_theta10 <- apply(theta10_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_theta10 <- apply(theta10_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df <- data.frame("Site" = 1:S)
          
          df$`2.5Credible Interval` <- ci25_theta10
          df$`Posterior Mean` <- theta10_means
          df$`97.5Credible Interval` <- ci95_theta10
          
          download_theta10 <- df
        }
        
        # download beta_theta10
        {
          beta_theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          for (chain in 1:nchain) {
            beta_theta10_output2[(chain - 1)*niter + 1:niter,] <- beta_theta10_output[chain,,]
          }
          gamma_theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          for (chain in 1:nchain) {
            gamma_theta10_output2[(chain - 1)*niter + 1:niter,] <- gamma_theta10_output[chain,,]
          }
          
          beta_theta10_output2[,1] <- logit(beta_theta10_output2[,1])
          colnames(beta_theta10_output2) <- dimnames(beta_theta10_output)[[3]]
          
          beta_theta10_mean <- sapply(1:ncol(beta_theta10_output2), function(i){
            if(i == 1){
              mean(beta_theta10_output2[,i])
            } else {
              mean(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i]-1]!=0,i])  
            }
          })
          ci95_theta10 <- sapply(1:ncol(beta_theta10_output2), function(i){
            if(i == 1){
              quantile(beta_theta10_output2[,i], probs = .975)  
            } else {
              quantile(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
            }
          })
          ci25_theta10 <- sapply(1:ncol(beta_theta10_output2), function(i){
            if(i == 1){
              quantile(beta_theta10_output2[,i], probs = .025)
            } else {
              quantile(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
            }
          })
          
          df <- data.frame("2.5Credible Interval" = ci25_theta10)
          colnames(df)[1] <- "2.5Credible Interval"
          df$`Posterior Mean` <- beta_theta10_mean
          df$`97.5Credible Interval` <- ci95_theta10
          df$PIP <- c(1,apply(gamma_theta10_output2, 2, mean)[(indexes_covariates-1)[-1]])
          
          row.names(df) <- colnames(beta_theta10_output2)
          
          download_beta_theta10 <- df
        }
        
        # download p11
        {
          p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            p11_output2[(chain - 1)*niter + 1:niter,] <- p11_output[chain,,]
          }
          
          p11_means <- apply(p11_output2, 2, mean)
          ci95_p11 <- apply(p11_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_p11 <- apply(p11_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df <- data.frame("Site" = 1:S)
          
          df$`2.5Credible Interval` <- ci25_p11
          df$`Posterior Mean` <- p11_means
          df$`97.5Credible Interval` <- ci95_p11
          
          download_p11 <- df
        }
        
        # downlaod beta_p11
        {
          beta_p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          for (chain in 1:nchain) {
            beta_p11_output2[(chain - 1)*niter + 1:niter,] <- beta_p11_output[chain,,]
          }
          gamma_p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          for (chain in 1:nchain) {
            gamma_p11_output2[(chain - 1)*niter + 1:niter,] <- gamma_p11_output[chain,,]
          }
          
          beta_p11_output2[,1] <- logit(beta_p11_output2[,1])
          colnames(beta_p11_output2) <- dimnames(beta_p11_output)[[3]]
          
          beta_p11_mean <- sapply(1:ncol(beta_p11_output2), function(i){
            if(i == 1){
              mean(beta_p11_output2[,i])
            } else {
              mean(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i]-1]!=0,i])  
            }
          })
          ci95_p11 <- sapply(1:ncol(beta_p11_output2), function(i){
            if(i == 1){
              quantile(beta_p11_output2[,i], probs = .975)  
            } else {
              quantile(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
            }
          })
          ci25_p11 <- sapply(1:ncol(beta_p11_output2), function(i){
            if(i == 1){
              quantile(beta_p11_output2[,i], probs = .025)
            } else {
              quantile(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
            }
          })
          
          df <- data.frame("2.5Credible Interval" = ci25_p11)
          colnames(df)[1] <- "2.5Credible Interval"
          df$`Posterior Mean` <- beta_p11_mean
          df$`97.5Credible Interval` <- ci95_p11
          df$PIP <- c(1,apply(gamma_p11_output2, 2, mean)[(indexes_covariates-1)[-1]])
          
          row.names(df) <- colnames(beta_p11_output2)
          
          download_beta_p11 <- df
        }
        
        # download p10
        {
          p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          for (chain in 1:nchain) {
            p10_output2[(chain - 1)*niter + 1:niter,] <- p10_output[chain,,]
          }
          
          p10_means <- apply(p10_output2, 2, mean)
          ci95_p10 <- apply(p10_output2, 2, function(x){
            quantile(x, probs = .975)
          })
          ci25_p10 <- apply(p10_output2, 2, function(x){
            quantile(x, probs = .025)
          })
          
          df <- data.frame("Site" = 1:S)
          
          df$`2.5Credible Interval` <- ci25_p10
          df$`Posterior Mean` <- p10_means
          df$`97.5Credible Interval` <- ci95_p10
          
          download_p10 <- df
        }
        
        # downlaod beta_p10
        {
          beta_p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          for (chain in 1:nchain) {
            beta_p10_output2[(chain - 1)*niter + 1:niter,] <- beta_p10_output[chain,,]
          }
          gamma_p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          for (chain in 1:nchain) {
            gamma_p10_output2[(chain - 1)*niter + 1:niter,] <- gamma_p10_output[chain,,]
          }
          
          beta_p10_output2[,1] <- logit(beta_p10_output2[,1])
          colnames(beta_p10_output2) <- dimnames(beta_p10_output)[[3]]
          
          beta_p10_mean <- sapply(1:ncol(beta_p10_output2), function(i){
            if(i == 1){
              mean(beta_p10_output2[,i])
            } else {
              mean(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i]-1]!=0,i])  
            }
          })
          ci95_p10 <- sapply(1:ncol(beta_p10_output2), function(i){
            if(i == 1){
              quantile(beta_p10_output2[,i], probs = .975)  
            } else {
              quantile(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
            }
          })
          ci25_p10 <- sapply(1:ncol(beta_p10_output2), function(i){
            if(i == 1){
              quantile(beta_p10_output2[,i], probs = .025)
            } else {
              quantile(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
            }
          })
          
          df <- data.frame("2.5Credible Interval" = ci25_p10)
          colnames(df)[1] <- "2.5Credible Interval"
          df$`Posterior Mean` <- beta_p10_mean
          df$`97.5Credible Interval` <- ci95_p10
          df$PIP <- c(1,apply(gamma_p10_output2, 2, mean)[(indexes_covariates-1)[-1]])
          
          row.names(df) <- colnames(beta_p10_output2)
          
          download_beta_p10 <- df
        }
        
        # conditional probability tables
        {
          condProbTable <- matrix(NA, nrow = 2, ncol = K + 1)
          condProbTable[1,] <- apply(oneminpsix[1,,], 2, mean)
          condProbTable[2,] <- apply(qx[1,,], 2, mean)
          
          colnames(condProbTable) <-  0:K
          rownames(condProbTable) <- c(paste0("1 - psi(x)"),"q(x)")
        }
        
      }
      
      rv_eDNA_download$results <- list("download_psi" = download_psi,
                                       # "download_z" = download_z,
                                       "download_beta_psi" = download_beta_psi,
                                       "download_theta11" = download_theta11,
                                       "download_beta_theta11" = download_beta_theta11,
                                       "download_theta10" = download_theta10,
                                       "download_beta_theta10" = download_beta_theta10,
                                       "download_p11" = download_p11,
                                       "download_beta_p11" = download_beta_p11,
                                       "download_p10" = download_p10,
                                       "download_beta_p10" = download_beta_p10,
                                       "condProbTable" = condProbTable)
      # print("ok bello here 3")
      # plots diagnostics
      {
        # beta0_psi_GW_diagn <- computeGewekeDiagnostics(beta_psi_output[1,,1])
        
        beta0_psi_diagnostics_plot <- qplot(1:niter, beta_psi_output[1,,1], geom = "line") + ggtitle("Intercept of psi") +
          theme_bw() + scale_y_continuous(name = "") +
          xlab("Iterations") 
        # ggtitle(paste0("Intercept of psi - Geweke Diagnostics = ", round(beta0_psi_GW_diagn,4))) 
        beta_psi_output_long <- melt(beta_psi_output[1,,-1])
        if(usingCov[1]){
          beta_psi_diagnostics_plot <- ggplot(beta_psi_output_long, aes(x = Var1, y = value, colour = Var2)) + geom_line() +
            ggtitle("Coefficients of psi") +
            theme_bw() + scale_y_continuous(name = "") +
            xlab("Iterations") 
        } else {
          beta_psi_diagnostics_plot <- NULL
        }
        
        # beta0_theta11_GW_diagn <- computeGewekeDiagnostics(beta_theta11_output[1,,1])
        
        beta0_theta11_diagnostics_plot <- qplot(1:niter, beta_theta11_output[1,,1], geom = "line") + ggtitle("Intercept of theta11") +
          theme_bw() + scale_y_continuous(name = "") +
          xlab("Iterations") 
        # ggtitle(paste0("Intercept of theta11 - Geweke Diagnostics = ", round(beta0_theta11_GW_diagn,4)))
        beta_theta11_output_long <- melt(beta_theta11_output[1,,-1])
        if(usingCov[2]){
          beta_theta11_diagnostics_plot <- ggplot(beta_theta11_output_long, aes(x = Var1, y = value, colour = Var2)) + geom_line() +
            ggtitle("Coefficients of theta11")   +
            theme_bw() + scale_y_continuous(name = "") +
            xlab("Iterations") 
        } else {
          beta_theta11_diagnostics_plot <- NULL
        }
        
        # beta0_theta10_GW_diagn <- computeGewekeDiagnostics(beta_theta10_output[1,,1])
        
        beta0_theta10_diagnostics_plot <- qplot(1:niter, beta_theta10_output[1,,1], geom = "line") + ggtitle("Intercept of theta10") +
          theme_bw() + scale_y_continuous(name = "") +
          xlab("Iterations")  
        # ggtitle(paste0("Intercept of theta10 - Geweke Diagnostics = ", round(beta0_theta10_GW_diagn,4)))
        beta_theta10_output_long <- melt(beta_theta10_output[1,,-1])
        if(usingCov[3]){
          beta_theta10_diagnostics_plot <- ggplot(beta_theta10_output_long, aes(x = Var1, y = value, colour = Var2)) + geom_line() +
            ggtitle("Coefficients of theta10")   +
            theme_bw() + scale_y_continuous(name = "") +
            xlab("Iterations") 
        } else {
          beta_theta10_diagnostics_plot <- NULL
        }
        
        # beta0_p11_GW_diagn <- computeGewekeDiagnostics(beta_p11_output[1,,1])
        
        beta0_p11_diagnostics_plot <- qplot(1:niter, beta_p11_output[1,,1], geom = "line") + ggtitle("Intercept of p11") +
          theme_bw() + scale_y_continuous(name = "") +
          xlab("Iterations") 
        # ggtitle(paste0("Intercept of p11 - Geweke Diagnostics = ", round(beta0_p11_GW_diagn,4)))
        beta_p11_output_long <- melt(beta_p11_output[1,,-1])
        if(usingCov[4]){
          beta_p11_diagnostics_plot <- ggplot(beta_p11_output_long, aes(x = Var1, y = value, colour = Var2)) + geom_line() +
            ggtitle("Coefficients of p11")  +
            theme_bw() + scale_y_continuous(name = "") +
            xlab("Iterations")  
        } else  {
          beta_p11_diagnostics_plot <- NULL
        }
        
        # beta0_p10_GW_diagn <- computeGewekeDiagnostics(beta_p10_output[1,,1])
        
        beta0_p10_diagnostics_plot <- qplot(1:niter, beta_p10_output[1,,1], geom = "line") + ggtitle("Intercept of p10") +
          theme_bw() + scale_y_continuous(name = "") +
          xlab("Iterations") 
        # ggtitle(paste0("Intercept of p10 - Geweke Diagnostics = ", round(beta0_p10_GW_diagn,4)))
        beta_p10_output_long <- melt(beta_p10_output[1,,-1])
        if(usingCov[5]){
          beta_p10_diagnostics_plot <- ggplot(beta_p10_output_long, aes(x = Var1, y = value, colour = Var2)) + geom_line() +
            ggtitle("Coefficients of p10")   +
            theme_bw() + scale_y_continuous(name = "") +
            xlab("Iterations") 
        } else {
          beta_p10_diagnostics_plot <- NULL
        }
        
      }
      
      # create diagnostics table
      {
        
        # psi
        {
          ESS_beta0psi <- effectiveSize(beta_psi_output[1,,1])
          if(usingCov[1]){
            ESS_betapsi <- effectiveSize(beta_psi_output[1,,-1])
            diagnostics_table_psi <- data.frame("Variable" = c("Intercept",names(ESS_betapsi)),
                                                "ESS" = c(ESS_beta0psi, ESS_betapsi),
                                                "Recommended ESS" = 500)
            
          } else {
            diagnostics_table_psi <- data.frame("Variable" = c("Intercept"),
                                                "ESS" = c(ESS_beta0psi),
                                                "Recommended ESS" = 500)
          } 
          
          
          rownames(diagnostics_table_psi) <- NULL
        }
        
        # theta11
        {
          ESS_beta0theta11 <- effectiveSize(beta_theta11_output[1,,1])
          if(usingCov[2]){
            ESS_betatheta11 <- effectiveSize(beta_theta11_output[1,,-1])
            
            diagnostics_table_theta11 <- data.frame("Variable" = c("Intercept",names(ESS_betatheta11)),
                                                    "ESS" = c(ESS_beta0theta11, ESS_betatheta11),
                                                    "Recommended ESS" = 500)
          } else {
            diagnostics_table_theta11 <- data.frame("Variable" = c("Intercept"),
                                                    "ESS" = c(ESS_beta0theta11),
                                                    "Recommended ESS" = 500)
          }
          
          rownames(diagnostics_table_theta11) <- NULL
        }
        
        # theta10
        {
          ESS_beta0theta10 <- effectiveSize(beta_theta10_output[1,,1])
          if(usingCov[3]){
            ESS_betatheta10 <- effectiveSize(beta_theta10_output[1,,-1])
            
            diagnostics_table_theta10 <- data.frame("Variable" = c("Intercept",names(ESS_betatheta10)),
                                                    "ESS" = c(ESS_beta0theta10, ESS_betatheta10),
                                                    "Recommended ESS" = 500)
            
          } else {
            diagnostics_table_theta10 <- data.frame("Variable" = c("Intercept"),
                                                    "ESS" = c(ESS_beta0theta10),
                                                    "Recommended ESS" = 500)
            
          }
          
          rownames(diagnostics_table_theta10) <- NULL
        }
        
        # p11
        {
          ESS_beta0p11 <- effectiveSize(beta_p11_output[1,,1])
          if(usingCov[4]){
            ESS_betap11 <- effectiveSize(beta_p11_output[1,,-1])
            
            diagnostics_table_p11 <- data.frame("Variable" = c("Intercept",names(ESS_betap11)),
                                                "ESS" = c(ESS_beta0p11, ESS_betap11),
                                                "Recommended ESS" = 500)  
          } else {
            diagnostics_table_p11 <- data.frame("Variable" = c("Intercept"),
                                                "ESS" = c(ESS_beta0p11),
                                                "Recommended ESS" = 500)  
          }
          
          
          rownames(diagnostics_table_p11) <- NULL
        }
        
        # p10
        {
          ESS_beta0p10 <- effectiveSize(beta_p10_output[1,,1])
          if(usingCov[5]){
            ESS_betap10 <- effectiveSize(beta_p10_output[1,,-1])
            
            diagnostics_table_p10 <- data.frame("Variable" = c("Intercept",names(ESS_betap10)),
                                                "ESS" = c(ESS_beta0p10, ESS_betap10),
                                                "Recommended ESS" = 500)
            
          } else {
            diagnostics_table_p10 <- data.frame("Variable" = c("Intercept"),
                                                "ESS" = c(ESS_beta0p10),
                                                "Recommended ESS" = 500)
          }
          
          rownames(diagnostics_table_p10) <- NULL
        }
        
        convergenceReached <- (ESS_beta0psi > 500) & (ESS_beta0theta11 > 500) & (ESS_beta0theta10 > 500) & 
          (ESS_beta0p11 > 500) & (ESS_beta0p10 > 500) 
      }
      
      if(convergenceReached){
        shinyalert("Fitting complete", "", type = "success")  
      } else {
        shinyalert("Convergence not reached", "We suggest running again the model with more iterations", type = "warning")
      }
      
      
      rv_eDNA_plots$results_diagnostics <- list("beta0_psi_diagnostics_plot" = beta0_psi_diagnostics_plot,
                                                "beta_psi_diagnostics_plot" = beta_psi_diagnostics_plot,
                                                "beta0_theta11_diagnostics_plot" = beta0_theta11_diagnostics_plot,
                                                "beta_theta11_diagnostics_plot" = beta_theta11_diagnostics_plot,
                                                "beta0_theta10_diagnostics_plot" = beta0_theta10_diagnostics_plot,
                                                "beta_theta10_diagnostics_plot" = beta_theta10_diagnostics_plot,
                                                "beta0_p11_diagnostics_plot" = beta0_p11_diagnostics_plot,
                                                "beta_p11_diagnostics_plot" = beta_p11_diagnostics_plot,
                                                "beta0_p10_diagnostics_plot" = beta0_p10_diagnostics_plot,
                                                "beta_p10_diagnostics_plot" = beta_p10_diagnostics_plot,
                                                "diagnostics_table_psi" = diagnostics_table_psi,
                                                "diagnostics_table_theta11" = diagnostics_table_theta11,
                                                "diagnostics_table_theta10" = diagnostics_table_theta10,
                                                "diagnostics_table_p11" = diagnostics_table_p11,
                                                "diagnostics_table_p10" = diagnostics_table_p10)
      
      # runjs("$('#download_all')[0].click();")
      
    })
    
    createPlots <- function(data_output, beta_data_output, gamma_data_output, indexes_covariates,
                            usingC, S, niter, nchain, nameVariable, VariableText ){
      
      data_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
      for (chain in 1:nchain) {
        data_output2[(chain - 1)*niter + 1:niter,] <- data_output[chain,,]
      }
      
      data_output_long <- melt(data_output2)
      
      CI_data  <- sapply(1:ncol(data_output2), function(i){
        c(quantile(data_output2[,i], probs = c(0.025,0.975)),
          mean(data_output2[,i]))
      })
      
      if(usingC){
        plot1 <- ggplot() + 
          geom_errorbar(data = NULL, aes(x = 1:S, ymax=CI_data[2,], 
                                         ymin=CI_data[1,]),
                        width=0.2, size=1, color="black") + 
          geom_point(data = NULL, aes(x = 1:S, 
                                      y=CI_data[3,]), size=2, shape=21, fill="white") +
          # theme(panel.background = element_rect(fill = "white")) +
          theme_bw() + scale_y_continuous(name = nameVariable) +
          xlab("Sites") 
      } else {
        plot1 <- ggplot(data = NULL, aes(x = "Site", y = data_output2[,1])) + geom_boxplot() +
          theme_bw() + scale_y_continuous(name = nameVariable) +
          xlab("")
      }
      
      data_plot <- plot1
      
      # beta_psi - gamma_psi
      if(usingC){
        
        {
          beta_data_output2 <- matrix(NA, nrow = niter * nchain, ncol = dim(beta_data_output)[3])
          for (chain in 1:nchain) {
            beta_data_output2[(chain - 1)*niter + 1:niter,] <- beta_data_output[chain,,]
          }
          gamma_data_output2 <- matrix(NA, nrow = niter * nchain, ncol = dim(gamma_data_output)[3])
          for (chain in 1:nchain) {
            gamma_data_output2[(chain - 1)*niter + 1:niter,] <- gamma_data_output[chain,,]
          }
          
          beta_data_output2[,1] <- logit(beta_data_output2[,1])
          colnames(beta_data_output2) <- dimnames(beta_data_output)[[3]]
          
          beta0_data_plot <- ggplot(data = NULL, aes(x = beta_data_output2[,1], y = ..density..)) + 
            geom_histogram(fill = "cornsilk", color = "black") + ylab("") + xlab("Probability") + 
            theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,5,0), size = 14, face = "bold"),
                  panel.background = element_rect(fill = "white"), 
                  panel.border = element_rect(fill = NA, colour = "grey20"),
                  panel.grid.major = element_line(colour = "grey92"), 
                  panel.grid.minor = element_line(colour = "grey92", size = 0.25), 
                  strip.background = element_rect(fill = "grey85", colour = "grey20"), 
                  legend.key = element_rect(fill = "white", colour = NA)) + 
            ggtitle(VariableText)
          
          CICoefficients_data  <- sapply(1:dim(beta_data_output2)[2], function(i){
            if(i == 1){
              c(quantile(beta_data_output2[,1], probs = c(0.025,0.975)),
                mean(beta_data_output2[,1]))
            } else {
              # print(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i])
              c(quantile(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i], probs = c(0.025,0.975)),
                mean(beta_data_output2[gamma_data_output2[,indexes_covariates[i]-1]!= 0,i]))
            }
          })
          
          PIP_data <- data.frame(name = dimnames(gamma_data_output)[[3]],
                                 prob = apply(gamma_data_output2, 2, mean))
          
          gamma_data_plot <- ggplot(PIP_data, aes(x=reorder(name, prob), y=prob)) +
            geom_point(size=3) + # Use a larger dot
            theme_bw() +
            ylab("PIP") + xlab("Variable") + 
            theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,5,0), size = 14, face = "bold"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(colour="grey60", linetype="dashed"),
                  axis.text.x = element_text(angle = 90)) +
            ylim(c(0,1)) + geom_hline(aes(yintercept = .5), color = "red")
          
          beta_data_plot <- ggplot() +
            geom_errorbar(data = NULL, aes(x = reorder(factor(colnames(beta_data_output2)[-1], 
                                                              levels = colnames(beta_data_output2)[-1]), 
                                                       rep(PIP_data$prob, table(indexes_covariates[-1]))), ymax=CICoefficients_data[1,-1], 
                                           ymin=CICoefficients_data[2,-1]),
                          width=0.2, size=1, color="black") +
            geom_point(data = NULL, aes(x = reorder(factor(colnames(beta_data_output2)[-1], 
                                                           levels = colnames(beta_data_output2)[-1]), 
                                                    rep(PIP_data$prob, table(indexes_covariates[-1]))), 
                                        y=CICoefficients_data[3,-1]), size=4, shape=21, fill="white") +
            theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,5,0), size = 14, face = "bold"),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(fill = NA, colour = "grey20"),
                  panel.grid.major = element_line(colour = "grey92"),
                  panel.grid.minor = element_line(colour = "grey92", size = 0.25),
                  strip.background = element_rect(fill = "grey85", colour = "grey20"),
                  legend.key = element_rect(fill = "white", colour = NA),
                  axis.text.x = element_text(angle = 90))  +
            xlab("Variable")+ ylab("Coefficient") #+ coord_flip()
          
        }
        
      } else {
        beta0_data_plot <- NULL
        beta_data_plot <- NULL
        gamma_data_plot <- NULL
      }
      
      list("data_plot" = data_plot,
           "beta0_data_plot" = beta0_data_plot,
           "beta_data_plot" = beta_data_plot,
           "gamma_data_plot" = gamma_data_plot)
    }
    
    # plots for psi 
    {
      output$plot_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        psi_plot <- list_results$psi_plot
        
        psi_plot
      })
      
      output$plot_beta0_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta0_psi_plot <- list_results$beta0_psi_plot
        
        beta0_psi_plot
        
      })
      
      output$plot_beta_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta_psi_plot <- list_results$beta_psi_plot
        
        beta_psi_plot
        
      })
      
      output$plot_gamma_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        gamma_psi_plot <- list_results$gamma_psi_plot
        
        gamma_psi_plot
      })
      
      output$download_psi_plot <- downloadHandler(
        filename = function() { "plot_psi.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          psi_plot <- list_results$psi_plot
          
          ggsave(file, plot = psi_plot, device = "png")
        }
      )
      
      output$download_beta0_psi_plot <- downloadHandler(
        filename = function() { "beta0_psi_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta0_psi_plot <- list_results$beta0_psi_plot
          
          ggsave(file, plot = beta0_psi_plot, device = "png")
        }
      )
      
      output$download_beta_psi_plot <- downloadHandler(
        filename = function() { "beta_psi_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta_psi_plot <- list_results$beta_psi_plot
          
          ggsave(file, plot = beta_psi_plot, device = "png")
        }
      )
      
      output$download_gamma_psi_plot <- downloadHandler(
        filename = function() { "gamma_psi_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          gamma_psi_plot <- list_results$gamma_psi_plot
          
          ggsave(file, plot = gamma_psi_plot, device = "png")
        }
      )
      
    }
    
    # plots for theta11
    {
      
      output$plot_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        theta11_plot <- list_results$theta11_plot
        
        theta11_plot
      })
      
      output$plot_beta0_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta0_theta11_plot <- list_results$beta0_theta11_plot
        
        beta0_theta11_plot
        
      })
      
      output$plot_beta_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta_theta11_plot <- list_results$beta_theta11_plot
        
        beta_theta11_plot
        
      })
      
      output$plot_gamma_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        gamma_theta11_plot <- list_results$gamma_theta11_plot
        
        gamma_theta11_plot
      })
      
      output$download_theta11_plot <- downloadHandler(
        filename = function() { "plot_theta11.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          theta11_plot <- list_results$theta11_plot
          
          ggsave(file, plot = theta11_plot, device = "png")
        }
      )
      
      output$download_beta0_theta11_plot <- downloadHandler(
        filename = function() { "beta0_theta11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta0_theta11_plot <- list_results$beta0_theta11_plot
          
          ggsave(file, plot = beta0_theta11_plot, device = "png")
        }
      )
      
      output$download_beta_theta11_plot <- downloadHandler(
        filename = function() { "beta_theta11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta_theta11_plot <- list_results$beta_theta11_plot
          
          ggsave(file, plot = beta_theta11_plot, device = "png")
        }
      )
      
      output$download_gamma_theta11_plot <- downloadHandler(
        filename = function() { "gamma_theta11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          gamma_theta11_plot <- list_results$gamma_theta11_plot
          
          ggsave(file, plot = gamma_theta11_plot, device = "png")
        }
      )
    }
    
    # plots for theta10
    {
      
      output$plot_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        theta10_plot <- list_results$theta10_plot
        
        theta10_plot
      })
      
      output$plot_beta0_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta0_theta10_plot <- list_results$beta0_theta10_plot
        
        beta0_theta10_plot
        
      })
      
      output$plot_beta_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta_theta10_plot <- list_results$beta_theta10_plot
        
        beta_theta10_plot
        
      })
      
      output$plot_gamma_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        gamma_theta10_plot <- list_results$gamma_theta10_plot
        
        gamma_theta10_plot
      })
      
      output$download_theta10_plot <- downloadHandler(
        filename = function() { "plot_theta10.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          theta10_plot <- list_results$theta10_plot
          
          ggsave(file, plot = theta10_plot, device = "png")
        }
      )
      
      output$download_beta0_theta10_plot <- downloadHandler(
        filename = function() { "beta0_theta10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta0_theta10_plot <- list_results$beta0_theta10_plot
          
          ggsave(file, plot = beta0_theta10_plot, device = "png")
        }
      )
      
      output$download_beta_theta10_plot <- downloadHandler(
        filename = function() { "beta_theta10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta_theta10_plot <- list_results$beta_theta10_plot
          
          ggsave(file, plot = beta_theta10_plot, device = "png")
        }
      )
      
      output$download_gamma_theta10_plot <- downloadHandler(
        filename = function() { "gamma_theta10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          gamma_theta10_plot <- list_results$gamma_theta10_plot
          
          ggsave(file, plot = gamma_theta10_plot, device = "png")
        }
      )
    }
    
    # plot for p11
    {
      
      output$plot_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        p11_plot <- list_results$p11_plot
        
        p11_plot
      })
      
      output$plot_beta0_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta0_p11_plot <- list_results$beta0_p11_plot
        
        beta0_p11_plot
        
      })
      
      output$plot_beta_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta_p11_plot <- list_results$beta_p11_plot
        
        beta_p11_plot
        
      })
      
      output$plot_gamma_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        gamma_p11_plot <- list_results$gamma_p11_plot
        
        gamma_p11_plot
      })
      
      output$download_p11_plot <- downloadHandler(
        filename = function() { "plot_p11.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          p11_plot <- list_results$p11_plot
          
          ggsave(file, plot = p11_plot, device = "png")
        }
      )
      
      output$download_beta0_p11_plot <- downloadHandler(
        filename = function() { "beta0_p11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta0_p11_plot <- list_results$beta0_p11_plot
          
          ggsave(file, plot = beta0_p11_plot, device = "png")
        }
      )
      
      output$download_beta_p11_plot <- downloadHandler(
        filename = function() { "beta_p11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta_p11_plot <- list_results$beta_p11_plot
          
          ggsave(file, plot = beta_p11_plot, device = "png")
        }
      )
      
      output$download_gamma_p11_plot <- downloadHandler(
        filename = function() { "gamma_p11_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          gamma_p11_plot <- list_results$gamma_p11_plot
          
          ggsave(file, plot = gamma_p11_plot, device = "png")
        }
      )
    }
    
    # plots for p10
    {
      
      output$plot_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        p10_plot <- list_results$p10_plot
        
        p10_plot
      })
      
      output$plot_beta0_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta0_p10_plot <- list_results$beta0_p10_plot
        
        beta0_p10_plot
        
      })
      
      output$plot_beta_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        beta_p10_plot <- list_results$beta_p10_plot
        
        beta_p10_plot
        
      })
      
      output$plot_gamma_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results
        
        gamma_p10_plot <- list_results$gamma_p10_plot
        
        gamma_p10_plot
      })
      
      output$download_p10_plot <- downloadHandler(
        filename = function() { "plot_p10.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          p10_plot <- list_results$p10_plot
          
          ggsave(file, plot = p10_plot, device = "png")
        }
      )
      
      output$download_beta0_p10_plot <- downloadHandler(
        filename = function() { "beta0_p10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta0_p10_plot <- list_results$beta0_p10_plot
          
          ggsave(file, plot = beta0_p10_plot, device = "png")
        }
      )
      
      output$download_beta_p10_plot <- downloadHandler(
        filename = function() { "beta_p10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          beta_p10_plot <- list_results$beta_p10_plot
          
          ggsave(file, plot = beta_p10_plot, device = "png")
        }
      )
      
      output$download_gamma_p10_plot <- downloadHandler(
        filename = function() { "gamma_p10_plot.png" },
        content = function(file) {
          list_results <- rv_eDNA_plots$results
          
          gamma_p10_plot <- list_results$gamma_p10_plot
          
          ggsave(file, plot = gamma_p10_plot, device = "png")
        }
      )
    }
    
    # diagnostics plot
    {
      output$plot_diagnostics_beta0_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta0_psi_diagnostics_plot
      })
      
      output$plot_diagnostics_beta_psi <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta_psi_diagnostics_plot
      })
      
      output$plot_diagnostics_beta0_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta0_theta11_diagnostics_plot
      })
      
      output$plot_diagnostics_beta_theta11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta_theta11_diagnostics_plot
      })
      
      output$plot_diagnostics_beta0_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta0_theta10_diagnostics_plot
      })
      
      output$plot_diagnostics_beta_theta10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta_theta10_diagnostics_plot
      })
      
      output$plot_diagnostics_beta0_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta0_p11_diagnostics_plot
      })
      
      output$plot_diagnostics_beta_p11 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta_p11_diagnostics_plot
      })
      
      output$plot_diagnostics_beta0_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta0_p10_diagnostics_plot
      })
      
      output$plot_diagnostics_beta_p10 <- renderPlot({
        
        list_results <- rv_eDNA_plots$results_diagnostics
        
        list_results$beta_p10_diagnostics_plot
      })
      
      
    }
    
    # download results
    {
      
      output$download_psi <- downloadHandler(
        filename = function() {
          paste("occupancy_probabilities", ".csv", sep = "")
        },
        content = function(file) {
          
          list_results <- rv_eDNA_download$results
          download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          # list_results <- rv$results
          # psi_output <- list_results$psi_output
          # 
          # nchain <- dim(psi_output)[1]
          # niter <- dim(psi_output)[2]
          # S <- dim(psi_output)[3]
          # 
          # psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          # for (chain in 1:nchain) {
          #   psi_output2[(chain - 1)*niter + 1:niter,] <- psi_output[chain,,]
          # }
          # 
          # psi_means <- apply(psi_output2, 2, mean)
          # ci95_psi <- apply(psi_output2, 2, function(x){
          #   quantile(x, probs = .975)
          # })
          # ci25_psi <- apply(psi_output2, 2, function(x){
          #   quantile(x, probs = .025)
          # })
          # 
          # df <- data.frame("Site" = 1:S)
          # 
          # column_presence <- input$presence_column
          # if(column_presence != 0){
          #   k_s <- as.vector(presenceInput())
          # } else {
          #   k_s <- rep(0, S)
          # }
          # 
          # df$`2.5Credible Interval` <- ci25_psi
          # df$`Posterior Mean` <- psi_means
          # df$`97.5Credible Interval` <- ci95_psi
          
          write.csv(download_psi, file, row.names = FALSE)
        }
      )
      
      # output$download_z <- downloadHandler(
      #   filename = function() {
      #     paste("presence_probabilities", ".csv", sep = "")
      #   },
      #   content = function(file) {
      #     
      #     list_results <- rv_eDNA_download$results
      #     download_z <- list_results$download_z
      #     # download_beta_psi <- list_results$download_beta_psi
      #     # download_theta11 <- list_results$download_theta11
      #     # download_beta_theta11 <- list_results$download_beta_theta11
      #     # download_theta10 <- list_results$download_theta10
      #     # download_beta_theta10 <- list_results$download_beta_theta10
      #     # download_p11 <- list_results$download_p11
      #     # download_beta_p11 <- list_results$download_beta_p11
      #     # download_p10 <- list_results$download_p10
      #     # download_beta_p10 <- list_results$download_beta_p10
      #     
      #     # list_results <- rv$results
      #     # psi_output <- list_results$psi_output
      #     # 
      #     # nchain <- dim(psi_output)[1]
      #     # niter <- dim(psi_output)[2]
      #     # S <- dim(psi_output)[3]
      #     # 
      #     # psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
      #     # for (chain in 1:nchain) {
      #     #   psi_output2[(chain - 1)*niter + 1:niter,] <- psi_output[chain,,]
      #     # }
      #     # 
      #     # psi_means <- apply(psi_output2, 2, mean)
      #     # ci95_psi <- apply(psi_output2, 2, function(x){
      #     #   quantile(x, probs = .975)
      #     # })
      #     # ci25_psi <- apply(psi_output2, 2, function(x){
      #     #   quantile(x, probs = .025)
      #     # })
      #     # 
      #     # df <- data.frame("Site" = 1:S)
      #     # 
      #     # column_presence <- input$presence_column
      #     # if(column_presence != 0){
      #     #   k_s <- as.vector(presenceInput())
      #     # } else {
      #     #   k_s <- rep(0, S)
      #     # }
      #     # 
      #     # df$`2.5Credible Interval` <- ci25_psi
      #     # df$`Posterior Mean` <- psi_means
      #     # df$`97.5Credible Interval` <- ci95_psi
      #     
      #     write.csv(download_z, file, row.names = FALSE)
      #   }
      # )
      
      output$download_beta_psi <- downloadHandler(
        filename = function() {
          paste("coefficients_psi", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # beta_psi_output <- list_results$beta_psi_output
          # gamma_psi_output <- list_results$gamma_psi_output
          # indexes_covariates <- list_results$indexes_covariates
          # 
          # nchain <- dim(beta_psi_output)[1]
          # niter <- dim(beta_psi_output)[2]
          # ncov <- dim(gamma_psi_output)[3] 
          # ncov_all <- dim(beta_psi_output)[3]
          # 
          # beta_psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          # for (chain in 1:nchain) {
          #   beta_psi_output2[(chain - 1)*niter + 1:niter,] <- beta_psi_output[chain,,]
          # }
          # gamma_psi_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          # for (chain in 1:nchain) {
          #   gamma_psi_output2[(chain - 1)*niter + 1:niter,] <- gamma_psi_output[chain,,]
          # }
          # 
          # # beta_psi_output2[,1] <- logit(beta_psi_output2[,1])
          # colnames(beta_psi_output2) <- dimnames(beta_psi_output)[[3]]
          # 
          # beta_psi_mean <- sapply(1:ncol(beta_psi_output2), function(i){
          #   if(i == 1){
          #     mean(beta_psi_output2[,i])
          #   } else {
          #     mean(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i]-1]!=0,i])  
          #   }
          # })
          # ci95_psi <- sapply(1:ncol(beta_psi_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_psi_output2[,i], probs = .975)  
          #   } else {
          #     quantile(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
          #   }
          # })
          # ci25_psi <- sapply(1:ncol(beta_psi_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_psi_output2[,i], probs = .025)
          #   } else {
          #     quantile(beta_psi_output2[gamma_psi_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
          #   }
          # })
          # 
          # df <- data.frame("2.5Credible Interval" = ci25_psi)
          # colnames(df)[1] <- "2.5Credible Interval"
          # df$`Posterior Mean` <- beta_psi_mean
          # df$`97.5Credible Interval` <- ci95_psi
          # df$PIP <- c(1,apply(gamma_psi_output2, 2, mean)[(indexes_covariates-1)[-1]])
          # 
          # row.names(df) <- colnames(beta_psi_output2)
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          write.csv(download_beta_psi, file)
        }
      )
      
      output$download_theta11 <- downloadHandler(
        filename = function() {
          paste("theta11", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # theta11_output <- list_results$theta11_output
          # 
          # nchain <- dim(theta11_output)[1]
          # niter <- dim(theta11_output)[2]
          # S <- dim(theta11_output)[3]
          # 
          # theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          # for (chain in 1:nchain) {
          #   theta11_output2[(chain - 1)*niter + 1:niter,] <- theta11_output[chain,,]
          # }
          # 
          # theta11_means <- apply(theta11_output2, 2, mean)
          # ci95_theta11 <- apply(theta11_output2, 2, function(x){
          #   quantile(x, probs = .975)
          # })
          # ci25_theta11 <- apply(theta11_output2, 2, function(x){
          #   quantile(x, probs = .025)
          # })
          # 
          # df <- data.frame("Site" = 1:S)
          # 
          # df$`2.5Credible Interval` <- ci25_theta11
          # df$`Posterior Mean` <- theta11_means
          # df$`97.5Credible Interval` <- ci95_theta11
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          download_theta11 <- list_results$download_theta11
          
          write.csv(download_theta11, file, row.names = F)
        }
      )
      
      output$download_beta_theta11 <- downloadHandler(
        filename = function() {
          paste("coefficients_theta11", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # beta_theta11_output <- list_results$beta_theta11_output
          # gamma_theta11_output <- list_results$gamma_theta11_output
          # indexes_covariates <- list_results$indexes_covariates
          # 
          # nchain <- dim(beta_theta11_output)[1]
          # niter <- dim(beta_theta11_output)[2]
          # ncov <- dim(gamma_theta11_output)[3] 
          # ncov_all <- dim(beta_theta11_output)[3]
          # 
          # beta_theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          # for (chain in 1:nchain) {
          #   beta_theta11_output2[(chain - 1)*niter + 1:niter,] <- beta_theta11_output[chain,,]
          # }
          # gamma_theta11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          # for (chain in 1:nchain) {
          #   gamma_theta11_output2[(chain - 1)*niter + 1:niter,] <- gamma_theta11_output[chain,,]
          # }
          # 
          # # beta_theta11_output2[,1] <- logit(beta_theta11_output2[,1])
          # colnames(beta_theta11_output2) <- dimnames(beta_theta11_output)[[3]]
          # 
          # beta_theta11_mean <- sapply(1:ncol(beta_theta11_output2), function(i){
          #   if(i == 1){
          #     mean(beta_theta11_output2[,i])
          #   } else {
          #     mean(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i]-1]!=0,i])  
          #   }
          # })
          # ci95_theta11 <- sapply(1:ncol(beta_theta11_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_theta11_output2[,i], probs = .975)  
          #   } else {
          #     quantile(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
          #   }
          # })
          # ci25_theta11 <- sapply(1:ncol(beta_theta11_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_theta11_output2[,i], probs = .025)
          #   } else {
          #     quantile(beta_theta11_output2[gamma_theta11_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
          #   }
          # })
          # 
          # df <- data.frame("2.5Credible Interval" = ci25_theta11)
          # colnames(df)[1] <- "2.5Credible Interval"
          # df$`Posterior Mean` <- beta_theta11_mean
          # df$`97.5Credible Interval` <- ci95_theta11
          # df$PIP <- c(1,apply(gamma_theta11_output2, 2, mean)[(indexes_covariates-1)[-1]])
          # 
          # row.names(df) <- colnames(beta_theta11_output2)
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          download_beta_theta11 <- list_results$download_beta_theta11
          
          write.csv(download_beta_theta11, file)
        }
      )
      
      output$download_theta10 <- downloadHandler(
        filename = function() {
          paste("theta10", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # theta10_output <- list_results$theta10_output
          # 
          # nchain <- dim(theta10_output)[1]
          # niter <- dim(theta10_output)[2]
          # S <- dim(theta10_output)[3]
          # 
          # theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          # for (chain in 1:nchain) {
          #   theta10_output2[(chain - 1)*niter + 1:niter,] <- theta10_output[chain,,]
          # }
          # 
          # theta10_means <- apply(theta10_output2, 2, mean)
          # ci95_theta10 <- apply(theta10_output2, 2, function(x){
          #   quantile(x, probs = .975)
          # })
          # ci25_theta10 <- apply(theta10_output2, 2, function(x){
          #   quantile(x, probs = .025)
          # })
          # 
          # df <- data.frame("Site" = 1:S)
          # 
          # df$`2.5Credible Interval` <- ci25_theta10
          # df$`Posterior Mean` <- theta10_means
          # df$`97.5Credible Interval` <- ci95_theta10
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_theta10, file, row.names = F)
        }
      )
      
      output$download_beta_theta10 <- downloadHandler(
        filename = function() {
          paste("coefficients_theta10", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # beta_theta10_output <- list_results$beta_theta10_output
          # gamma_theta10_output <- list_results$gamma_theta10_output
          # indexes_covariates <- list_results$indexes_covariates
          # 
          # nchain <- dim(beta_theta10_output)[1]
          # niter <- dim(beta_theta10_output)[2]
          # ncov <- dim(gamma_theta10_output)[3] 
          # ncov_all <- dim(beta_theta10_output)[3]
          # 
          # beta_theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          # for (chain in 1:nchain) {
          #   beta_theta10_output2[(chain - 1)*niter + 1:niter,] <- beta_theta10_output[chain,,]
          # }
          # gamma_theta10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          # for (chain in 1:nchain) {
          #   gamma_theta10_output2[(chain - 1)*niter + 1:niter,] <- gamma_theta10_output[chain,,]
          # }
          # 
          # # beta_theta10_output2[,1] <- logit(beta_theta10_output2[,1])
          # colnames(beta_theta10_output2) <- dimnames(beta_theta10_output)[[3]]
          # 
          # beta_theta10_mean <- sapply(1:ncol(beta_theta10_output2), function(i){
          #   if(i == 1){
          #     mean(beta_theta10_output2[,i])
          #   } else {
          #     mean(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i]-1]!=0,i])  
          #   }
          # })
          # ci95_theta10 <- sapply(1:ncol(beta_theta10_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_theta10_output2[,i], probs = .975)  
          #   } else {
          #     quantile(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
          #   }
          # })
          # ci25_theta10 <- sapply(1:ncol(beta_theta10_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_theta10_output2[,i], probs = .025)
          #   } else {
          #     quantile(beta_theta10_output2[gamma_theta10_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
          #   }
          # })
          # 
          # df <- data.frame("2.5Credible Interval" = ci25_theta10)
          # colnames(df)[1] <- "2.5Credible Interval"
          # df$`Posterior Mean` <- beta_theta10_mean
          # df$`97.5Credible Interval` <- ci95_theta10
          # df$PIP <- c(1,apply(gamma_theta10_output2, 2, mean)[(indexes_covariates-1)[-1]])
          # 
          # row.names(df) <- colnames(beta_theta10_output2)
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_beta_theta10, file)
        }
      )
      
      output$download_p11 <- downloadHandler(
        filename = function() {
          paste("p11", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # p11_output <- list_results$p11_output
          # 
          # nchain <- dim(p11_output)[1]
          # niter <- dim(p11_output)[2]
          # S <- dim(p11_output)[3]
          # 
          # p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          # for (chain in 1:nchain) {
          #   p11_output2[(chain - 1)*niter + 1:niter,] <- p11_output[chain,,]
          # }
          # 
          # p11_means <- apply(p11_output2, 2, mean)
          # ci95_p11 <- apply(p11_output2, 2, function(x){
          #   quantile(x, probs = .975)
          # })
          # ci25_p11 <- apply(p11_output2, 2, function(x){
          #   quantile(x, probs = .025)
          # })
          # 
          # df <- data.frame("Site" = 1:S)
          # 
          # df$`2.5Credible Interval` <- ci25_p11
          # df$`Posterior Mean` <- p11_means
          # df$`97.5Credible Interval` <- ci95_p11
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_p11, file, row.names = F)
        }
      )
      
      output$download_beta_p11 <- downloadHandler(
        filename = function() {
          paste("coefficients_p11", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # beta_p11_output <- list_results$beta_p11_output
          # gamma_p11_output <- list_results$gamma_p11_output
          # indexes_covariates <- list_results$indexes_covariates
          # 
          # nchain <- dim(beta_p11_output)[1]
          # niter <- dim(beta_p11_output)[2]
          # ncov <- dim(gamma_p11_output)[3] 
          # ncov_all <- dim(beta_p11_output)[3]
          # 
          # beta_p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          # for (chain in 1:nchain) {
          #   beta_p11_output2[(chain - 1)*niter + 1:niter,] <- beta_p11_output[chain,,]
          # }
          # gamma_p11_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          # for (chain in 1:nchain) {
          #   gamma_p11_output2[(chain - 1)*niter + 1:niter,] <- gamma_p11_output[chain,,]
          # }
          # 
          # # beta_p11_output2[,1] <- logit(beta_p11_output2[,1])
          # colnames(beta_p11_output2) <- dimnames(beta_p11_output)[[3]]
          # 
          # beta_p11_mean <- sapply(1:ncol(beta_p11_output2), function(i){
          #   if(i == 1){
          #     mean(beta_p11_output2[,i])
          #   } else {
          #     mean(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i]-1]!=0,i])  
          #   }
          # })
          # ci95_p11 <- sapply(1:ncol(beta_p11_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_p11_output2[,i], probs = .975)  
          #   } else {
          #     quantile(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
          #   }
          # })
          # ci25_p11 <- sapply(1:ncol(beta_p11_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_p11_output2[,i], probs = .025)
          #   } else {
          #     quantile(beta_p11_output2[gamma_p11_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
          #   }
          # })
          # 
          # df <- data.frame("2.5Credible Interval" = ci25_p11)
          # colnames(df)[1] <- "2.5Credible Interval"
          # df$`Posterior Mean` <- beta_p11_mean
          # df$`97.5Credible Interval` <- ci95_p11
          # df$PIP <- c(1,apply(gamma_p11_output2, 2, mean)[(indexes_covariates-1)[-1]])
          # 
          # row.names(df) <- colnames(beta_p11_output2)
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_beta_p11, file)
        }
      )
      
      output$download_p10 <- downloadHandler(
        filename = function() {
          paste("p10", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # p10_output <- list_results$p10_output
          # 
          # nchain <- dim(p10_output)[1]
          # niter <- dim(p10_output)[2]
          # S <- dim(p10_output)[3]
          # 
          # p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = S)
          # for (chain in 1:nchain) {
          #   p10_output2[(chain - 1)*niter + 1:niter,] <- p10_output[chain,,]
          # }
          # 
          # p10_means <- apply(p10_output2, 2, mean)
          # ci95_p10 <- apply(p10_output2, 2, function(x){
          #   quantile(x, probs = .975)
          # })
          # ci25_p10 <- apply(p10_output2, 2, function(x){
          #   quantile(x, probs = .025)
          # })
          # 
          # df <- data.frame("Site" = 1:S)
          # 
          # df$`2.5Credible Interval` <- ci25_p10
          # df$`Posterior Mean` <- p10_means
          # df$`97.5Credible Interval` <- ci95_p10
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          download_p10 <- list_results$download_p10
          # download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_p10, file, row.names = F)
        }
      )
      
      output$download_beta_p10 <- downloadHandler(
        filename = function() {
          paste("coefficients_p10", ".csv", sep = "")
        },
        content = function(file) {
          
          # list_results <- rv$results
          # 
          # beta_p10_output <- list_results$beta_p10_output
          # gamma_p10_output <- list_results$gamma_p10_output
          # indexes_covariates <- list_results$indexes_covariates
          # 
          # nchain <- dim(beta_p10_output)[1]
          # niter <- dim(beta_p10_output)[2]
          # ncov <- dim(gamma_p10_output)[3] 
          # ncov_all <- dim(beta_p10_output)[3]
          # 
          # beta_p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov_all)
          # for (chain in 1:nchain) {
          #   beta_p10_output2[(chain - 1)*niter + 1:niter,] <- beta_p10_output[chain,,]
          # }
          # gamma_p10_output2 <- matrix(NA, nrow = niter * nchain, ncol = ncov)
          # for (chain in 1:nchain) {
          #   gamma_p10_output2[(chain - 1)*niter + 1:niter,] <- gamma_p10_output[chain,,]
          # }
          # 
          # # beta_p10_output2[,1] <- logit(beta_p10_output2[,1])
          # colnames(beta_p10_output2) <- dimnames(beta_p10_output)[[3]]
          # 
          # beta_p10_mean <- sapply(1:ncol(beta_p10_output2), function(i){
          #   if(i == 1){
          #     mean(beta_p10_output2[,i])
          #   } else {
          #     mean(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i]-1]!=0,i])  
          #   }
          # })
          # ci95_p10 <- sapply(1:ncol(beta_p10_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_p10_output2[,i], probs = .975)  
          #   } else {
          #     quantile(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i]-1]!=0,i], probs = .975)  
          #   }
          # })
          # ci25_p10 <- sapply(1:ncol(beta_p10_output2), function(i){
          #   if(i == 1){
          #     quantile(beta_p10_output2[,i], probs = .025)
          #   } else {
          #     quantile(beta_p10_output2[gamma_p10_output2[,indexes_covariates[i] - 1]!=0,i], probs = .025)  
          #   }
          # })
          # 
          # df <- data.frame("2.5Credible Interval" = ci25_p10)
          # colnames(df)[1] <- "2.5Credible Interval"
          # df$`Posterior Mean` <- beta_p10_mean
          # df$`97.5Credible Interval` <- ci95_p10
          # df$PIP <- c(1,apply(gamma_p10_output2, 2, mean)[(indexes_covariates-1)[-1]])
          # 
          # row.names(df) <- colnames(beta_p10_output2)
          
          list_results <- rv_eDNA_download$results
          # download_psi <- list_results$download_psi
          # download_beta_psi <- list_results$download_beta_psi
          # download_theta11 <- list_results$download_theta11
          # download_beta_theta11 <- list_results$download_beta_theta11
          # download_theta10 <- list_results$download_theta10
          # download_beta_theta10 <- list_results$download_beta_theta10
          # download_p11 <- list_results$download_p11
          # download_beta_p11 <- list_results$download_beta_p11
          # download_p10 <- list_results$download_p10
          download_beta_p10 <- list_results$download_beta_p10
          
          write.csv(download_beta_p10, file)
        }
      )
      
      output$download_all <- downloadHandler(
        filename = function() {
          paste("output", "zip", sep=".")
        },
        content = function(fname) {
          
          tmpdir <- tempdir()
          setwd(tempdir())
          
          list_results <- rv_eDNA_download$results
          download_psi <- list_results$download_psi
          # download_z <- list_results$download_z
          download_beta_psi <- list_results$download_beta_psi
          download_theta11 <- list_results$download_theta11
          download_beta_theta11 <- list_results$download_beta_theta11
          download_theta10 <- list_results$download_theta10
          download_beta_theta10 <- list_results$download_beta_theta10
          download_p11 <- list_results$download_p11
          download_beta_p11 <- list_results$download_beta_p11
          download_p10 <- list_results$download_p10
          download_beta_p10 <- list_results$download_beta_p10
          
          fs <- c("download_psi.csv", "download_beta_psi.csv",
                  "download_theta11.csv", "download_beta_theta11.csv",
                  "download_theta10.csv", "download_beta_theta10.csv",
                  "download_p11.csv", "download_beta_p11.csv",
                  "download_p10.csv", "download_beta_p10.csv",
                  row.names = F)
          write.csv(download_psi, file = "download_psi.csv", sep =",", row.names = F)
          # write.csv(download_z, file = "download_z.csv", sep =",", row.names = F)
          write.csv(download_beta_psi, file = "download_beta_psi.csv", sep =",")
          write.csv(download_theta11, file = "download_theta11.csv", sep =",", row.names = F)
          write.csv(download_beta_theta11, file = "download_beta_theta11.csv", sep =",")
          write.csv(download_theta10, file = "download_theta10.csv", sep =",", row.names = F)
          write.csv(download_beta_theta10, file = "download_beta_theta10.csv", sep =",")
          write.csv(download_p11, file = "download_p11.csv", sep =",", row.names = F)
          write.csv(download_beta_p11, file = "download_beta_p11.csv", sep =",")
          write.csv(download_p10, file = "download_p10.csv", sep =",", row.names = F)
          write.csv(download_beta_p10, file = "download_beta_p10.csv", sep =",")
          print (fs)
          
          zip(zipfile=fname, files=fs)
        },
        contentType = "application/zip"
      )
      
      output$condProbTable  <- renderTable({
        list_results <- rv_eDNA_download$results
        list_results$condProbTable
      }, rownames = T, digits = 4)
      
      output$diagnosticsTablePsi  <- renderTable({
        list_results <- rv_eDNA_plots$results_diagnostics
        list_results$diagnostics_table_psi
      }, rownames = T, digits = 4)
      
      output$diagnosticsTableTheta11  <- renderTable({
        list_results <- rv_eDNA_plots$results_diagnostics
        list_results$diagnostics_table_theta11
      }, rownames = T, digits = 4)
      
      output$diagnosticsTableTheta10  <- renderTable({
        list_results <- rv_eDNA_plots$results_diagnostics
        list_results$diagnostics_table_theta10
      }, rownames = T, digits = 4)
      
      output$diagnosticsTableP11  <- renderTable({
        list_results <- rv_eDNA_plots$results_diagnostics
        list_results$diagnostics_table_p11
      }, rownames = T, digits = 4)
      
      output$diagnosticsTableP10  <- renderTable({
        list_results <- rv_eDNA_plots$results_diagnostics
        list_results$diagnostics_table_p10
      }, rownames = T, digits = 4)
      
      output$download_condprob <- downloadHandler(
        filename = function() {
          paste("condprob", ".csv", sep = "")
        },
        content = function(file) {
          
          list_results <- rv_eDNA_download$results
          
          
          condprob <- list_results$condProbTable
          
          write.csv(condprob, file, row.names = T)
        }
      )
    }
    
    # DAG
    {
      output$DAG <- renderImage({
        
        if(!input$cov_using1 & !input$cov_using2 & !input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img.jpg", package = "eDNAShinyApp"))  
          # filename <- normalizePath(file.path('./www',
          #                                     "img.jpg"))  
        } else if (input$cov_using1 & !input$cov_using2 & !input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img1.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & !input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img12.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img13.jpg", package = "eDNAShinyApp"))   
        } else if (input$cov_using1 & !input$cov_using2 & !input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img14.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & !input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img15.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img123.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & !input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img124.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & !input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img125.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img134.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img135.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & !input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img145.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img1234.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img1235.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & !input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img1245.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & !input$cov_using2 & input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img1345.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & !input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img2.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & !input$cov_using2 & input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img3.jpg", package = "eDNAShinyApp"))   
        } else if (!input$cov_using1 & !input$cov_using2 & !input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img4.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & !input$cov_using2 & !input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img5.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & input$cov_using3 & !input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img23.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & !input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img24.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & !input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img25.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & !input$cov_using2 & input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img34.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & !input$cov_using2 & input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img35.jpg", package = "eDNAShinyApp"))   
        } else if (!input$cov_using1 & !input$cov_using2 & !input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img45.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & input$cov_using3 & input$cov_using4 & !input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img234.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & input$cov_using3 & !input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img235.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & !input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img245.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & !input$cov_using2 & input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img345.jpg", package = "eDNAShinyApp"))  
        } else if (!input$cov_using1 & input$cov_using2 & input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img2345.jpg", package = "eDNAShinyApp"))  
        } else if (input$cov_using1 & input$cov_using2 & input$cov_using3 & input$cov_using4 & input$cov_using5){
          filename <- normalizePath(system.file('./www',
                                                "img12345.jpg", package = "eDNAShinyApp"))  
        }
        
        
        # Return a list containing the filename and alt text
        list(src = filename,
             width = 700,
             alt = paste("Image number", input$n))
        
      }, deleteFile = FALSE)
    }
    
    
    
  }
  
}

app <- shinyApp(ui = ui, server = server)

