# Installing the packages for utilizing a different theme
# Install.packages("shinythemes")

# install.packages("glue")
# install.packages("digest")

# Install package to grab NBA data
# devtools::install_github("abresler/nbastatR")

# Install package for biplot
#devtools::install_github("vqv/ggbiplot")

# Install package for plotly
# install.packages("plotly")

require(plotly)
require(ggbiplot)
library(nbastatR)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(caret)
library(randomForest)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  tabsetPanel(
                      tabPanel("Information Page", 
                               titlePanel(h1(strong("LeBron James"), style = "color:black")),
                               mainPanel(
                                   tags$img(src="LeBron_James_Lakers.png", height="25%", width="25%", align="right"),
                                   br(), br(),
                                   h3(strong("Purpose of App: ", style = "color:gold"), "This shiny app will go over LeBron's statistics over his career in the National Basketball Association (NBA). 
                                          Widely considered one of the greatest NBA players, James is frequently compared to Michael Jordan in debates over the greatest basketball player of all time. 
                                          To find out more about LeBron James, click on this", a("link here", href="https://en.wikipedia.org/wiki/LeBron_James", target="_blank")),
                                   br(), br(),
                                   h3(strong("Data Description: ", style = "color:black"), "This data utilizes the R package named 'nbastatR'. An interface for professional basketball data in R. 
                                       Data sources include, but are not limited to: NBA Stats API, Basketball Insiders, Basketball-Reference, HoopsHype, and RealGM.
                                          To see the full documentation on this R package", a("click here", href="https://www.rdocumentation.org/packages/nbastatR/versions/0.1.03", target="_blank")),
                                   br(), br(),
                                   h3(strong("Navigation Guide: ", style = "color:purple"), "This is the section where I'll be describing each tab for the app. 
                                          The tab for Numerical and Graphical Summaries summarizes LeBrons total stats throughout his career. 
                                          The Principal Components Analysis tab reduces the dimensions but still accounts for most of the variability. 
                                          This is an unsupervised learning technique. The modeling tab is used for predicting per game stats for LeBron. The 
                                          last tab is for looking through the data and extracting it.")
                               )
                      ),
                      #---------------------------------------------------------------------                      
                      tabPanel("Data Exploration", 
                               titlePanel("Numerical and Graphical Summaries"),
                               sidebarLayout(
                                   sidebarPanel(
                                       h3("LeBron has accumulated a big amount of stats throughout his career. The 
                                        radio button below breaks down the historical chart and is broken down by the different 
                                        teams he has played on."),
                                       radioButtons("trend", label = "Regular Season Numeric Trend",
                                                    choices = c("Points", "Rebounds", "Assists", "Steals", "Blocks"),
                                                    selected = "Points"),
                                       h3("To select the specific you want to see, click the drop down below.", strong("Select 
                                                                                                                     championship year to see playoff stats accumulated.", style = "color:red")),
                                       selectInput("var", label = "Regular Season Totals", 
                                                   choices = c("Rookie Season", "02", "03", "04", "05", "06", "07", 
                                                               "08", "09 - Championship", "10 - Championship", "11", "12", "13 - Championship", "14", "15", 
                                                               "16", "17 - Championship"),
                                                   selected = "Rookie Season"),
                                       conditionalPanel(condition = "input.var=='09 - Championship'",
                                                        checkboxInput("Playoffs1", "Show Playoffs Stats?")),
                                       conditionalPanel(condition = "input.var=='10 - Championship'",
                                                        checkboxInput("Playoffs2", "Show Playoffs Stats?")),
                                       conditionalPanel(condition = "input.var=='13 - Championship'",
                                                        checkboxInput("Playoffs3", "Show Playoffs Stats?")),
                                       conditionalPanel(condition = "input.var=='17 - Championship'",
                                                        checkboxInput("Playoffs4", "Show Playoffs Stats?"))
                                   ),
                                   mainPanel(
                                       h5("From looking at the different options, LeBron has maintained his offense abilities 
                                        throughout his career. If we look at his steals and blocks, those have slowly declined 
                                        and I believe that comes with age. From the data, it looks like LeBron focuses more on his 
                                        offensive game and his passing ability has improved."),
                                       plotlyOutput("coolplot"), downloadButton("LeBron_TrendChart", "Download Plot"),
                                       h5("This table displays LeBrons total accumulated stats for a specific season."),
                                       dataTableOutput("results"),
                                       h5("Championship Playoffs Stats Accumulated by LeBron:"),
                                       dataTableOutput("rings")
                                   )
                               )                               
                      ),
                      #---------------------------------------------------------------------
                      tabPanel("Principal Component Analysis", 
                               titlePanel("Principal Component Analysis of LeBron"),
                               sidebarLayout(
                                   sidebarPanel(
                                       h3("Select regular season or playoffs for PCA."),
                                       radioButtons("career", label = "Totals Criteria",
                                                    choices = c("Career Playoff Totals", 
                                                                "Career Regular Season Totals"),
                                                    selected = "Career Regular Season Totals")
                                   ),
                                   mainPanel(
                                       h5("PCA stands for Principal Component Analysis. It is an unsupervised algorithm that is able to summarize a set of continuous variables into a lower-dimensional 
                representation that is hopefully easier to visualize. In some sense, we are compressing the data 
                into a compact form that captures most of the variability in the data. With PCA, we'll investigate which combination 
                of features best explain the variability of LeBron's performances across different seasons. PC1 explains 47% of the variability and what's 
                interesting is that it places most of it's weight on LeBron's age. Longevity has been LeBron's greatest strength and after 17 seasons, he still 
                performs like an MVP. PC2 explains 22.5% of the variability. This places most of its weight on 3 Point Field Goals Made. 
                The NBA has adapted into a 3 Point Shooting league so this makes sense."),
                                       br(),
                                       h5("Playoffs: From the Career Playoff Data, PC1 explains 56.5% of the variability. While many fields hold a lot of weight, 
                I found it interesting that pctFT or Free Three Percentage was one of the weaker weights. In the playoffs, Free Throw % has 
                been one of LeBron's weaknesses so this visual reall shows it. For PC2, it explains 17.5% of the variability. It holds most of its weight on 
                LeBrons Free Throw Attemps and Free Throws Made. With LeBron's physical playstyle, he goes to the free throw more than others in the league."),
                                       plotlyOutput("pca", height = 550, width = 550), downloadButton("LeBron_PCA", "Download Plot"),
                                       h5("The scree plot below shows us how much of the variability we lose. PC1 and PC2 retain 
                about 18% of the variability and the rest is lost. This is the negatives when plotting only 
                2 dimensions."),
                                       plotOutput("pca2"), downloadButton("LeBron_ScreePlot", "Download Plot")
                                   )
                               )                               
                      ),
                      #---------------------------------------------------------------------
                      tabPanel("Modeling", 
                               titlePanel("Modeling Techniques"),
                               sidebarLayout(
                                   sidebarPanel(
                                       h3("Simple Linear Regression"),
                                       radioButtons("predictors", label = "Choose Predictor Variable",
                                                    choices = c("Field Goal Made Per Game", 
                                                                "Free Throws Made Per Game",
                                                                "Field Goal 3 Point Made Per Game",
                                                                "Minutes Per Game",
                                                                "Age"),
                                                    selected = "Field Goal Made Per Game"),
                                       h3("Random Forest"),
                                       sliderInput("trees", "Number of Trees:", min = 1, max = 400, value = 200)
                                   ),
                                   mainPanel(
                                       h3("Simple Linear Regression"),
                                       h5("The below graph represents the linear relationship between 2 continuous variables. Here we 
                use numerous predictor variables to measure the relationship against LeBron's points per game. Field 
                goals made seems to have the strongest relationship and this makes sense because you have to make a field goal to 
                accumulate points. It also has the smallest confidence interval meaning the margin of error is smaller."),
                                       
                                       withMathJax(),
                                       helpText('Simple Linear Regression Formula: $$Y_i={\\beta_0}+{\\beta_1}x_i+E_i$$'),
                                       
                                       plotlyOutput("mlrmodel"), downloadButton("LeBron_LinearReg", "Download Plot"),
                                       h3("Multiple Linear Regression"),
                                       h5("With multiple linear regression, you can choose more than 1 predictor variable to measure 
                the relationship with the response variable. We'll be using 2 variables to try to predict 
                LeBron's Points Per Game throughout each season and comparing it to the actual with a 95% confidence 
                interval showing the upper and lower limits."),
                                       h6("Response Variable - Points Per Game"),
                                       h6("Predictor Variables - Field Goal Made Per Game, Season Number"),
                                       
                                       helpText('Multiple Linear Regression Formula: $$Y_i={\\beta_0}+{\\beta_1}x_i+{\\beta_2}x^2_i+E_i$$'),
                                       
                                       dataTableOutput("ppgpredict"), downloadButton("LeBron_Prediction", "Download Data"),
                                       h5(strong("Analysis of Variance Table")),
                                       h5("The anova table displays information about the multiple regression model. Here you can see degrees of freedom, 
                sum of squares, and mean square residuals (error). One interesting aspect is the p-value. Season Number is higher than 
                0.05 meaning it's not statistically significant for predicting Points Per Game. Field Goals Made Per Game is below 
                0.05 so this predictor is significant. Finding significant predictors greatly benefits your model when it comes to 
                multiple regression."),
                                       verbatimTextOutput("anova"),
                                       h5(strong("Adjusted R-Square")),
                                       h5("The adjusted r-square is diplayed below. This explains how much of the variability is explained in 
                our model. The closer to 1 it is, the better."),
                                       verbatimTextOutput("rsquared"),
                                       h3("Random Forest for Predicting"),
                                       h5("Random Forest is a supervised learning algorithm. It uses ensemble learning methods 
                for classification and regression. It is one of the best learning algorithms and 
                it finds the best predictor variables. We'll be using a test and training data set to 
                predict LeBron's Ponts Per Game. You can adjust the model settings."),
                                       dataTableOutput("ppgpredictrf"), downloadButton("LeBron_PredictionRF", "Download Data"),
                                       h5(strong("Root Mean Square Error")),
                                       h5("When utilizing many models, comparing RMSE is a good comparing."),
                                       verbatimTextOutput("ppgpredictrfrmse")
                                   )
                               )                               
                      ),
                      #---------------------------------------------------------------------
                      tabPanel("Data Set", 
                               mainPanel(
                                   h5("The table displays LeBrons career totals for the regular season."),
                                   dataTableOutput("lbjtotals"), downloadButton("LeBronRegSeasonTotals", "Download Data"),
                                   h5("The table displays LeBrons career totals for the playoffs."),
                                   dataTableOutput("lbjtotalspost"), downloadButton("LeBronRegPlayoffsTotals", "Download Data"),
                                   h5("The table displays LeBrons career stats per game."),
                                   dataTableOutput("lbjppg"), downloadButton("LeBron_ppg", "Download Data"),
                               )
                      ))
                  
                  
                  
                  
))
