library(shiny);library(DT);library(shinycustomloader);library(shinyhttr);library(shinyWidgets);library(data.table)
options(shiny.sanitize.errors = F)
options(shiny.maxRequestSize=30*1024^2)


ui <- navbarPage("CDM: estimation",
                 tabPanel("Data",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("file", "Upload compressed file"),
                              helpText(a("Example data", href="https://consultdata.s3.ap-northeast-2.amazonaws.com/example_CDMestimation")),
                              uiOutput("selectdata")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Original", 
                                                   withLoader(DTOutput("data_original"), type="html", loader="loader6"),
                                                   withLoader(imageOutput("flow_original"), type="html", loader="loader6")),
                                          tabPanel("Matching", 
                                                   withLoader(DTOutput("data_ps", width = "100%"), type="html", loader="loader6"),
                                                   withLoader(imageOutput("flow_ps"), type="html", loader="loader6"))
                              )
                            )
                          )
                 ),
                 tabPanel("Matching info",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("dec_bal", "Digits", value = 1, min = 1, max = 3)

                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Balance Table", 
                                                   withLoader(DTOutput("data_balance"), type="html", loader="loader6")
                                                   ),
                                          tabPanel("Balance Image",
                                                   radioButtons("bal_img", "Image select", choices = c("Top 20", "Scatterplot"), selected = "Top 20", inline = T),
                                                   withLoader(imageOutput("image_balance", width = "100%"), type="html", loader="loader6")
                                          ),
                                          tabPanel("PS distribution", 
                                                   radioButtons("psdist_img", "Image select", choices = c("Before", "After"), selected = "Before", inline = T),
                                                   withLoader(imageOutput("image_psdist", width = "100%"), type="html", loader="loader6")
                                          )
                              )
                            )
                          )
                 ),
                 tabPanel("Model outcome",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("typeOutcome")
                            ),
                            mainPanel(
                              withLoader(uiOutput("modelOutcome"), type="html", loader="loader6")
                            )
                          )
                 )
)


server <- function(input, output, session) {
  
  
  userFile <- eventReactive(input$file, {
    # If no file is selected, don't do anything
    #validate(need(input$file, message = FALSE))
    input$file
    })
  
  data <- eventReactive(input$file, {
    #validate(need((grepl("zip", userFile()$name) == T), message = "Please upload zip file"))
    #validate(need((grepl("csv", userFile()$name) == T), message = "Please upload csv/xlsx/sav/sas7bdat file"))
    #files <- unzip(userFile()$datapath, exdir = "ex")
    tmp <- tempfile()
    zip::unzip(userFile()$datapath, exdir = tmp)
    ref <- readRDS(paste0(tmp, "/result/outcomeModelReference.rds"))
    fig.att <- grep("attritionDiagram", list.files(paste0(tmp, "/result")), value = T)
    fig.attstrata <- grep("Strata", fig.att, value = T)
    fig.att <- list(setdiff(fig.att, fig.attstrata), fig.attstrata)
    
    fig.covbal <- grep("covBal", list.files(paste0(tmp, "/result")), value = T)
    fig.covalscatter <- grep("Scatter", fig.covbal, value = T)
    fig.covbal <-list(setdiff(fig.covbal, fig.covalscatter), fig.covalscatter)
      
    fig.psplot <- grep("propensityScorePlot", list.files(paste0(tmp, "/result")), value = T) 
    fig.psstrata <- grep("Strata", fig.psplot, value = T)
    fig.psplot <- list(setdiff(fig.psplot, fig.psstrata), fig.psstrata)
    
    fig.km <-  grep("kaplanMeier", list.files(paste0(tmp, "/result")), value = T) 
  
    return(list(dir = tmp, ref = ref, fig.att = fig.att, fig.covbal = fig.covbal, fig.psplot = fig.psplot, fig.km = fig.km))
    })
  
  
  observeEvent(data(), {
    output$selectdata <- renderUI({
      tagList(
        h4("Select study"),
        selectInput("tID", "targetId", choices = unique(data()$ref$targetId)),
        selectInput("cID", "comparatorId", choices = unique(data()$ref$comparatorId)),
        selectInput("oID", "outcomeId", choices = unique(data()$ref$outcomeId))
      
        )
    })
    
    output$typeOutcome <-renderUI({
      if (length(data()$fig.km) == 0){
        radioButtons("type_outcome", "Result select", choices = c("Hazard ratio",  "psModelCoef", "attrition", "Event", "Others"), selected = "Hazard ratio", inline = T)
      } else{
        radioButtons("type_outcome", "Result select", choices = c("kaplanMeier", "Hazard ratio", "psModelCoef",  "attrition", "Event", "Others"), selected = "kaplanMeier", inline = T)
      }
        
    })
  })
  
  dlist <- reactive({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    info <- subset(data()$ref, targetId == input$tID & comparatorId == input$cID & outcomeId == input$oID)
    return(info)
  })
  
  
  output$data_original <- renderDT({
    datatable(readRDS(paste0(data()$dir, dlist()$psFile)), rownames = F, filter = "top", extensions= "Buttons", caption = "Original cohort", 
              options = c(jstable::opt.data("data_original"), list(scrollX = TRUE)))
    
    })

  output$data_ps <- renderDT({
    datatable(readRDS(paste0(data()$dir, dlist()$strataFile)), rownames = F, filter = "top", extensions= "Buttons", caption = "PS matching cohort", 
              options = c(jstable::opt.data("data_ps"), list(scrollX = TRUE)))
    
  })
  
  output$flow_original <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , data()$fig.att[[1]], value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  output$flow_ps <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , data()$fig.att[[2]], value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  
  output$data_balance <- renderDT({
    out.bal <- readRDS(paste0(data()$dir, dlist()$covariateBalanceFile))
    it <- 2
    ic <- 3
    bal.before <- sapply(c(it, ic), function(x){paste0(out.bal[, x + 2], " (", round(100 * out.bal[, x], input$dec_bal), "%)")})
    bal.after <- sapply(c(it, ic), function(x){paste0(out.bal[, x + 7], " (", round(100 * out.bal[, x + 5], input$dec_bal), "%)")})
    
    out.tb <- data.frame(out.bal[, 12], bal.before, round(out.bal[, 15], input$dec_bal + 2), bal.after, round(out.bal[, 16], input$dec_bal + 2), out.bal[, c(1, 13, 14)])
    out.tb[out.tb == "NA (NA%)"] <- NA
    names(out.tb)[1:7] <- c("Covariate", "Treated_before", "Comparator_before", "SMD_before", "Treated_after", "Comparator_after", "SMD_after")

    datatable(out.tb, rownames = F, filter = "top", extensions= "Buttons", caption = "Balance table for PS matching", 
              options = c(jstable::opt.data("data_balance"), list(scrollX = TRUE))) 

    
  })
  
  
  output$image_balance <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
  
    nn <- ifelse(input$bal_img == "Top 20", data()$fig.covbal[[1]], data()$fig.covbal[[2]])
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , nn, value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  

  
  
  output$image_psdist <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    nn <- ifelse(input$psdist_img == "Before", data()$fig.psplot[[1]], data()$fig.psplot[[2]])
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , nn, value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  

  
  observeEvent(input$type_outcome, {
    if (input$type_outcome == "kaplanMeier"){
      output$modelOutcome <- renderUI(imageOutput("model_outcome1"))
      output$model_outcome1 <- renderImage({
        name.img <- "kaplanMeier.png"
        filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
        #filename <- paste("data", input$file_figure, sep="")
        list(src = filename,
             width = "100%",
             alt = "This is alternate text")
      }, deleteFile = F)
    } else{
      output$modelOutcome <- renderUI(DTOutput("model_outcome"))
      output$model_outcome <- renderDT({
        #userFile()
        res <- readRDS(paste0(data()$dir, dlist()$outcomeModelFile))
        if (input$type_outcome == "psModelCoef"){
          #out.df <- data.frame(Coefficients = res[[input$type_outcome]])
          
          datatable(data.frame(Covariates = names(res[[input$type_outcome]]), HR = exp(res[[input$type_outcome]])),  rownames = F, filter = "top", extensions= "Buttons", caption = "PS model coefficients", 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) %>% 
            formatRound(2, digits = 3)
        } else if (input$type_outcome == "Hazard ratio"){
          hr <- res$outcomeModelTreatmentEstimate
          pv <- 2 * (1 - pnorm(abs(hr$logRr/hr$seLogRr)))
          pv <- ifelse(pv < 0.001, "< 0.001", round(pv, 3))
          tb.hr <- data.table(`HR (95% CI)` = paste0(round(exp(hr$logRr), 3), " (", round(exp(hr$logLb95), 3), "-", round(exp(hr$logUb95), 3), ")"), `P value` = pv)
          datatable(tb.hr,  rownames = F, extensions= "Buttons", caption = "Hazard ratio", 
                    options = c(jstable::opt.tbreg("Hazard ratio"), list(scrollX = TRUE))) 
        } else if (input$type_outcome == "Others"){
          name.tb <- setdiff(names(res), c("targetId", "comparatorId", "attrition", "psModelCoef", "outcomeModelTreatmentEstimate", "populationCounts", "outcomeCounts", "timeAtRisk"))
          datatable(t(sapply(name.tb, function(x){res[[x]]})), rownames = F,  extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) 
        } else if (input$type_outcome == "Event"){
          tb.oth <- t(data.frame(as.integer(res$populationCounts[, 2:3]), as.integer(res$timeAtRisk), as.integer(res$outcomeCounts[5:6])))
          colnames(tb.oth) <- c("Treated", "Comparator")
          rownames(tb.oth) <- c("Population", "Time at Risk", "Event")
          datatable(tb.oth,  rownames = T, extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data("Event"), list(scrollX = TRUE))) 
        } else {
          datatable(res[[input$type_outcome]],  rownames = F, extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) 
        }
        
      }) 
    }
  })
  
  
  


}




shinyApp(ui, server)