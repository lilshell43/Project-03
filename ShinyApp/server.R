# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

lebron_totals <- players_careers(players = c("LeBron James"),
                                 modes = c("Totals"))

lebronpergame <- transmute(dataPlayerSeasonTotalsRegularSeason,
                           'Season' = slugSeason,
                           'Team' = slugTeam,
                           'Points Per Game' = round((ptsTotals / gp), digits = 2),
                           'Player Fouls Per Game' = round((pfTotals / gp), digits = 2),
                           'Turnovers Per Game' = round((tovTotals / gp), digits = 2),
                           'Blocks Per Game' = round((blkTotals / gp), digits = 2),
                           'Steals Per Game' = round((stlTotals / gp), digits = 2),
                           'Assist Per Game' = round((astTotals / gp), digits = 2),
                           'Total Rebounds Per Game' = round((trebTotals / gp), digits = 2),
                           'Defensive Rebounds Per Game' = round((drebTotals / gp), digits = 2),
                           'Offensive Rebounds Per Game' = round((orebTotals / gp), digits = 2),
                           'Free Throw Attempts Per Game' = round((ftaTotals / gp), digits = 2),
                           'Free Throws Made Per Game' = round((ftmTotals / gp), digits = 2),
                           'Minutes Per Game' = round((minutesTotals / gp), digits = 2),
                           'Age' = agePlayer,
                           'Field Goal 2 Point %' = pctFG2,
                           'Field Goal 2 Point Attempts Per Game' = round((fg2aTotals / gp), digits = 2),
                           'Field Goal 2 Point Made Per Game' = round((fg2mTotals / gp), digits = 2),
                           'Free Throw %' = pctFT,
                           'Three Point %' = pctFG3,
                           'Field Goal 3 Point Attempts Per Game' = round((fg3aTotals / gp), digits = 2),
                           'Field Goal 3 Point Made Per Game' = round((fg3mTotals / gp), digits = 2),
                           'Field Goal %' = pctFG,
                           'Field Goal Attempts Per Game' = round((fgaTotals / gp), digits = 2),
                           'Field Goal Made Per Game' = round((fgmTotals / gp), digits = 2),
                           'Games Started' = gs,
                           'Games Played' = gp,
                           'Season Number' = numberPlayerSeason + 1)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #------------------------------------------------------------------  
    
    filtered <- reactive({ 
        lebron_totals <- players_careers(players = c("LeBron James"),
                                         modes = c("Totals"))
        seasonstats <- filter(dataPlayerSeasonTotalsRegularSeason, 
                              numberPlayerSeason==
                                  (if (input$var=="Rookie Season"){0}
                                   else if (input$var=="02"){1}
                                   else if (input$var=="03"){2}
                                   else if (input$var=="04"){3}
                                   else if (input$var=="05"){4}
                                   else if (input$var=="06"){5}
                                   else if (input$var=="07"){6}
                                   else if (input$var=="08"){7}
                                   else if (input$var=="09 - Championship"){8}
                                   else if (input$var=="10 - Championship"){9}
                                   else if (input$var=="11"){10}
                                   else if (input$var=="12"){11}
                                   else if (input$var=="13 - Championship"){12}
                                   else if (input$var=="14"){13}
                                   else if (input$var=="15"){14}
                                   else if (input$var=="16"){15}
                                   else if (input$var=="17 - Championship"){16}
                                  ))
        seasonstats <- select(seasonstats, slugSeason, slugTeam, gp, fgmTotals, pctFG, fg3mTotals, pctFG3,
                              trebTotals, astTotals, stlTotals, ptsTotals) %>%
            rename(Year=slugSeason, Team=slugTeam, 'Games Played'=gp, 'Field Goals Made'=fgmTotals, 
                   'Field Goal %'=pctFG, '3 Point Field Goals Made'=fg3mTotals, '3 Point Field Goal %'=pctFG3, 
                   'Total Rebounds'=trebTotals, 'Total Assist'=astTotals, 'Total Steals'=stlTotals, 
                   'Total Points'=ptsTotals)
        
    })
    
    output$results <- renderDataTable({
        filtered()
    })
    #------------------------------------------------------------------
    
    filtered2 <- reactive({
        if ((input$Playoffs1)&(input$var=="09 - Championship")){
            playoffstats <- filter(dataPlayerSeasonTotalsPostSeason,
                                   numberPlayerSeason==6)
            
            playoffstats <- select(playoffstats, slugSeason, slugTeam, gp, fgmTotals, pctFG, fg3mTotals, pctFG3,
                                   trebTotals, astTotals, stlTotals, ptsTotals) %>%
                rename(Year=slugSeason, Team=slugTeam, 'Games Played'=gp, 'Field Goals Made'=fgmTotals, 
                       'Field Goal %'=pctFG, '3 Point Field Goals Made'=fg3mTotals, '3 Point Field Goal %'=pctFG3, 
                       'Total Rebounds'=trebTotals, 'Total Assist'=astTotals, 'Total Steals'=stlTotals, 
                       'Total Points'=ptsTotals)
        }
        
        else if ((input$Playoffs2)&(input$var=="10 - Championship")){
            playoffstats <- filter(dataPlayerSeasonTotalsPostSeason,
                                   numberPlayerSeason==7)
            
            playoffstats <- select(playoffstats, slugSeason, slugTeam, gp, fgmTotals, pctFG, fg3mTotals, pctFG3,
                                   trebTotals, astTotals, stlTotals, ptsTotals) %>%
                rename(Year=slugSeason, Team=slugTeam, 'Games Played'=gp, 'Field Goals Made'=fgmTotals, 
                       'Field Goal %'=pctFG, '3 Point Field Goals Made'=fg3mTotals, '3 Point Field Goal %'=pctFG3, 
                       'Total Rebounds'=trebTotals, 'Total Assist'=astTotals, 'Total Steals'=stlTotals, 
                       'Total Points'=ptsTotals)
        }
        
        else if ((input$Playoffs3)&(input$var=="13 - Championship")){
            playoffstats <- filter(dataPlayerSeasonTotalsPostSeason,
                                   numberPlayerSeason==10)
            
            playoffstats <- select(playoffstats, slugSeason, slugTeam, gp, fgmTotals, pctFG, fg3mTotals, pctFG3,
                                   trebTotals, astTotals, stlTotals, ptsTotals) %>%
                rename(Year=slugSeason, Team=slugTeam, 'Games Played'=gp, 'Field Goals Made'=fgmTotals, 
                       'Field Goal %'=pctFG, '3 Point Field Goals Made'=fg3mTotals, '3 Point Field Goal %'=pctFG3, 
                       'Total Rebounds'=trebTotals, 'Total Assist'=astTotals, 'Total Steals'=stlTotals, 
                       'Total Points'=ptsTotals)
        }
        
        
        else if ((input$Playoffs4)&(input$var=="17 - Championship")){
            playoffstats <- filter(dataPlayerSeasonTotalsPostSeason,
                                   numberPlayerSeason==13)
            
            playoffstats <- select(playoffstats, slugSeason, slugTeam, gp, fgmTotals, pctFG, fg3mTotals, pctFG3,
                                   trebTotals, astTotals, stlTotals, ptsTotals) %>%
                rename(Year=slugSeason, Team=slugTeam, 'Games Played'=gp, 'Field Goals Made'=fgmTotals, 
                       'Field Goal %'=pctFG, '3 Point Field Goals Made'=fg3mTotals, '3 Point Field Goal %'=pctFG3, 
                       'Total Rebounds'=trebTotals, 'Total Assist'=astTotals, 'Total Steals'=stlTotals, 
                       'Total Points'=ptsTotals)
        }
        
        
    })
    
    output$rings <- renderDataTable({
        filtered2()
    })
    
    #------------------------------------------------------------------ 
    output$coolplot <- renderPlotly({
        ggplotly(
            ggplot(data = dataPlayerSeasonTotalsRegularSeason, aes(x=slugSeason, y=
                                                                       (if (input$trend=="Points"){ptsTotals}
                                                                        else if (input$trend=="Rebounds"){trebTotals}
                                                                        else if (input$trend=="Assists"){astTotals}
                                                                        else if (input$trend=="Steals"){stlTotals}
                                                                        else if (input$trend=="Blocks"){blkTotals}
                                                                       ))) + 
                geom_bar(stat = "identity", aes(fill = as.factor(slugTeam)), colour = "purple") + 
                ggtitle("Historical Trend") + 
                labs(x = "Season", y=
                         (if (input$trend=="Points"){"Points"}
                          else if (input$trend=="Rebounds"){"Rebounds"}
                          else if (input$trend=="Assists"){"Assists"}
                          else if (input$trend=="Steals"){"Steals"}
                          else if (input$trend=="Blocks"){"Blocks"})) + 
                scale_fill_discrete(name = "Team")
        )
        
    })
    #------------------------------------------------------------------ 
    
    output$pca <- renderPlotly({
        
        PCs <- prcomp(select(if (input$career=="Career Regular Season Totals"){dataPlayerSeasonTotalsRegularSeason}
                             else if (input$career=="Career Playoff Totals"){dataPlayerSeasonTotalsPostSeason}, 
                             gp:ptsTotals) , scale = TRUE)
        PCs
        ggplotly(
            ggbiplot(PCs) + xlim(-2,2)
        )
    })
    
    #------------------------------------------------------------------  
    output$pca2 <- renderPlot({
        
        PCs <- prcomp(select(if (input$career=="Career Regular Season Totals"){dataPlayerSeasonTotalsRegularSeason}
                             else if (input$career=="Career Playoff Totals"){dataPlayerSeasonTotalsPostSeason}, 
                             gp:ptsTotals) , scale = TRUE)
        
        screeplot(PCs, type = "lines") #plot used for visual
        
    })
    #------------------------------------------------------------------ 
    
    lebron_pergame <- reactive({
        
        lebronpergame <- transmute(dataPlayerSeasonTotalsRegularSeason,
                                   'Season' = slugSeason,
                                   'Team' = slugTeam,
                                   'Points Per Game' = round((ptsTotals / gp), digits = 2),
                                   'Player Fouls Per Game' = round((pfTotals / gp), digits = 2),
                                   'Turnovers Per Game' = round((tovTotals / gp), digits = 2),
                                   'Blocks Per Game' = round((blkTotals / gp), digits = 2),
                                   'Steals Per Game' = round((stlTotals / gp), digits = 2),
                                   'Assist Per Game' = round((astTotals / gp), digits = 2),
                                   'Total Rebounds Per Game' = round((trebTotals / gp), digits = 2),
                                   'Defensive Rebounds Per Game' = round((drebTotals / gp), digits = 2),
                                   'Offensive Rebounds Per Game' = round((orebTotals / gp), digits = 2),
                                   'Free Throw Attempts Per Game' = round((ftaTotals / gp), digits = 2),
                                   'Free Throws Made Per Game' = round((ftmTotals / gp), digits = 2),
                                   'Minutes Per Game' = round((minutesTotals / gp), digits = 2),
                                   'Age' = agePlayer,
                                   'Field Goal 2 Point %' = pctFG2,
                                   'Field Goal 2 Point Attempts Per Game' = round((fg2aTotals / gp), digits = 2),
                                   'Field Goal 2 Point Made Per Game' = round((fg2mTotals / gp), digits = 2),
                                   'Free Throw %' = pctFT,
                                   'Three Point %' = pctFG3,
                                   'Field Goal 3 Point Attempts Per Game' = round((fg3aTotals / gp), digits = 2),
                                   'Field Goal 3 Point Made Per Game' = round((fg3mTotals / gp), digits = 2),
                                   'Field Goal %' = pctFG,
                                   'Field Goal Attempts Per Game' = round((fgaTotals / gp), digits = 2),
                                   'Field Goal Made Per Game' = round((fgmTotals / gp), digits = 2),
                                   'Games Started' = gs,
                                   'Games Played' = gp,
                                   'Season Number' = numberPlayerSeason + 1
        )
        
        lebronpergame
        
    })
    #------------------------------------------------------------------  
    output$mlrmodel <- renderPlotly({
        ggplotly(
            ggplot(lebronpergame, aes_string(x=
                                                 if (input$predictors=="Field Goal Made Per Game"){
                                                     lebronpergame$`Field Goal Made Per Game`}
                                             else if (input$predictors=="Free Throws Made Per Game"){
                                                 lebronpergame$`Free Throws Made Per Game`}
                                             else if (input$predictors=="Field Goal 3 Point Made Per Game"){
                                                 lebronpergame$`Field Goal 3 Point Made Per Game`}
                                             else if (input$predictors=="Minutes Per Game"){
                                                 lebronpergame$`Minutes Per Game`}
                                             else if (input$predictors=="Age"){
                                                 lebronpergame$`Age`}
                                             ,
                                             y=lebronpergame$`Points Per Game`)) + 
                geom_point() + 
                geom_smooth(method = "lm", formula = y ~ x) + 
                labs(y="Points Per Game", x=
                         if (input$predictors=="Field Goal Made Per Game"){
                             "Field Goal Made Per Game"}
                     else if (input$predictors=="Free Throws Made Per Game"){
                         "Free Throws Made Per Game"}
                     else if (input$predictors=="Field Goal 3 Point Made Per Game"){
                         "Field Goal 3 Point Made Per Game"}
                     else if (input$predictors=="Minutes Per Game"){
                         "Minutes Per Game"}
                     else if (input$predictors=="Age"){
                         "Age"})
        )
    })
    #------------------------------------------------------------------
    
    lebron_pergamepred <- reactive({
        
        fit <- lm(`Points Per Game` ~ `Season Number` + `Field Goal Made Per Game`, data = lebronpergame)
        
        pred <- as.data.frame(predict(fit, interval = "confidence"))
        pred <- pred %>% round(digits = 2)
        lebronpergame %>% mutate(pred) %>% select(Season, `Points Per Game`, `fit`, `lwr`, `upr`) %>% 
            rename(Predicted = `fit`, `Actual` = `Points Per Game`, `Lower Limit` = `lwr`, `Upper Limit` = `upr`)
        
    }) 
    #------------------------------------------------------------------
    
    output$ppgpredict <- renderDataTable({
        lebron_pergamepred()
    })
    
    #------------------------------------------------------------------ 
    output$anova <- renderPrint({
        fit <- lm(`Points Per Game` ~ `Season Number` + `Field Goal Made Per Game`, data = lebronpergame)
        anova(fit)
    })  
    #------------------------------------------------------------------  
    
    output$rsquared <- renderPrint({
        fit <- lm(`Points Per Game` ~ `Season Number` + `Field Goal Made Per Game`, data = lebronpergame)
        summary(fit)$adj.r.squared 
    })  
    
    #------------------------------------------------------------------
    
    lebron_pergamepredrf <- reactive({
        
        names(lebronpergame) <- make.names(names(lebronpergame))
        
        set.seed(50)
        train <- sample(1:nrow(lebronpergame), size = nrow(lebronpergame)*.95)
        test <- dplyr::setdiff(1:nrow(lebronpergame), train)
        
        lbjTrain <- lebronpergame[train, ]
        lbjTest <- lebronpergame[test, ]
        
        rfit <- randomForest(`Points.Per.Game` ~ ., data = lbjTrain, mtry = ncol(lbjTrain)/3,
                             ntree = input$trees, importance = TRUE)
        rfPred <- predict(rfit, newdata = dplyr::select(lbjTest, -`Points.Per.Game`))
        as.data.frame(rfPred)  %>% round(digits = 2) %>% rename(`Predicted Points Per Game` = rfPred)
        
        
    })
    #------------------------------------------------------------------
    
    output$ppgpredictrf <- renderDataTable({
        lebron_pergamepredrf()
        
    })
    #------------------------------------------------------------------  
    
    output$ppgpredictrfrmse <- renderPrint({
        
        names(lebronpergame) <- make.names(names(lebronpergame))
        
        set.seed(50)
        train <- sample(1:nrow(lebronpergame), size = nrow(lebronpergame)*.95)
        test <- dplyr::setdiff(1:nrow(lebronpergame), train)
        
        lbjTrain <- lebronpergame[train, ]
        lbjTest <- lebronpergame[test, ]
        
        rfit <- randomForest(`Points.Per.Game` ~ ., data = lbjTrain, mtry = ncol(lbjTrain)/3,
                             ntree = input$trees, importance = TRUE)
        rfPred <- predict(rfit, newdata = dplyr::select(lbjTest, -`Points.Per.Game`))
        as.data.frame(rfPred)  %>% round(digits = 2) %>% rename(`Predicted Points Per Game` = rfPred)
        
        
        rfRMSE<-sqrt(mean((rfPred-lbjTest$`Points.Per.Game`)^2))
        rfRMSE
        
    })
    
    #------------------------------------------------------------------  
    
    output$lbjtotals <- renderDataTable({
        dataPlayerSeasonTotalsRegularSeason
        
    }) 
    #------------------------------------------------------------------ 
    
    output$lbjtotalspost <- renderDataTable({
        dataPlayerSeasonTotalsPostSeason
        
    }) 
    #------------------------------------------------------------------
    
    output$lbjppg <- renderDataTable({
        lebronpergame
        
    })
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBronRegSeasonTotals <- downloadHandler(
        filename = function(){
            paste("LeBronRegularSeasonTotals","csv",sep = ".")
        },
        content = function(file){
            write.csv(dataPlayerSeasonTotalsRegularSeason, file)
        }
    )
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBronRegPlayoffsTotals <- downloadHandler(
        filename = function(){
            paste("LeBronPostSeasonTotals","csv",sep = ".")
        },
        content = function(file){
            write.csv(dataPlayerSeasonTotalsPostSeason, file)
        }
    )
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBron_ppg <- downloadHandler(
        filename = function(){
            paste("LeBron_ppg","csv",sep = ".")
        },
        content = function(file){
            write.csv(lebronpergame, file)
        }
    )
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBron_TrendChart <- downloadHandler(
        filename = function(){
            paste("LeBron_TrendChart","png",sep = ".")
        },
        content = function(file){
            png(file)
            
            plot(
                ggplot(data = dataPlayerSeasonTotalsRegularSeason, aes(x=slugSeason, y=
                                                                           (if (input$trend=="Points"){ptsTotals}
                                                                            else if (input$trend=="Rebounds"){trebTotals}
                                                                            else if (input$trend=="Assists"){astTotals}
                                                                            else if (input$trend=="Steals"){stlTotals}
                                                                            else if (input$trend=="Blocks"){blkTotals}
                                                                           ))) + 
                    geom_bar(stat = "identity", aes(fill = as.factor(slugTeam)), colour = "purple") + ggtitle("Historical Trend") + 
                    labs(y=
                             (if (input$trend=="Points"){"Points"}
                              else if (input$trend=="Rebounds"){"Rebounds"}
                              else if (input$trend=="Assists"){"Assists"}
                              else if (input$trend=="Steals"){"Steals"}
                              else if (input$trend=="Blocks"){"Blocks"})) + 
                    scale_fill_discrete(name = "Team")
            )
            dev.off()
        }
    )  
    #------------------------------------------------------------------
    # Downloading data and plots
    output$LeBron_PCA <- downloadHandler(
        filename = function(){
            paste("LeBron_PCA","png",sep = ".")
        },
        content = function(file){
            png(file)
            
            plot(
                
                
                ggbiplot(PCs) + xlim(-2,2)
                
            )
            dev.off()
        }
    )
    #------------------------------------------------------------------ 
    # Downloading data and plots
    output$LeBron_ScreePlot <- downloadHandler(
        filename = function(){
            paste("LeBron_ScreePlot","png",sep = ".")
        },
        content = function(file){
            png(file)
            
            
            
            screeplot(PCs, type = "lines")
            
            
            dev.off()
        }
    ) 
    #------------------------------------------------------------------
    # Downloading data and plots
    output$LeBron_LinearReg <- downloadHandler(
        filename = function(){
            paste("LeBron_LinearReg","png",sep = ".")
        },
        content = function(file){
            png(file)
            
            plot(
                
                ggplot(lebronpergame, aes_string(x=
                                                     if (input$predictors=="Field Goal Made Per Game"){
                                                         lebronpergame$`Field Goal Made Per Game`}
                                                 else if (input$predictors=="Free Throws Made Per Game"){
                                                     lebronpergame$`Free Throws Made Per Game`}
                                                 else if (input$predictors=="Field Goal 3 Point Made Per Game"){
                                                     lebronpergame$`Field Goal 3 Point Made Per Game`}
                                                 else if (input$predictors=="Minutes Per Game"){
                                                     lebronpergame$`Minutes Per Game`}
                                                 else if (input$predictors=="Age"){
                                                     lebronpergame$`Age`}
                                                 ,
                                                 y=lebronpergame$`Points Per Game`)) + 
                    geom_point() + 
                    geom_smooth(method = "lm", formula = y ~ x) + 
                    labs(y="Points Per Game", x=
                             if (input$predictors=="Field Goal Made Per Game"){
                                 "Field Goal Made Per Game"}
                         else if (input$predictors=="Free Throws Made Per Game"){
                             "Free Throws Made Per Game"}
                         else if (input$predictors=="Field Goal 3 Point Made Per Game"){
                             "Field Goal 3 Point Made Per Game"}
                         else if (input$predictors=="Minutes Per Game"){
                             "Minutes Per Game"}
                         else if (input$predictors=="Age"){
                             "Age"})
                
                
            )
            dev.off()
        }
    )
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBron_Prediction <- downloadHandler(
        filename = function(){
            paste("LeBron_Prediction","csv",sep = ".")
        },
        content = function(file){
            write.csv(lebron_pergamepred(), file)
        }
    ) 
    #------------------------------------------------------------------  
    # Downloading data and plots
    output$LeBron_PredictionRF <- downloadHandler(
        filename = function(){
            paste("LeBron_PredictionRF","csv",sep = ".")
        },
        content = function(file){
            write.csv(lebron_pergamepredrf(), file)
        }
    )
    
})
