library(shiny)
library(shinythemes)
library(data.table)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cyborg"),
                
                # Page header
                headerPanel('Possibility of Stroke?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("gender", label = "Gender:", 
                              choices = list("Male" = "Male", "Female" = "Female", "Other" = "Other"), 
                              selected = "Male"),
                  sliderInput("age", "Age:",
                              min = 10, max = 82,
                              value = 50),
                  selectInput("hypertension", label = "Hybertension:", 
                              choices = list("Yes" = 1, "No" = 0), 
                              selected = 1),
                  selectInput("heart_disease", label = "Heart Diseases:", 
                              choices = list("Yes" = 1, "No" = 0), 
                              selected = 1),
                  selectInput("ever_married", label = "Ever Married:", 
                              choices = list("Yes" = "Yes", "No" = "No"), 
                              selected = 1),
                  selectInput("work_type", label = "Work Type:", 
                              choices = list("Private" = "Private", "Self Employed" = "Self-employed","Govt. Job"= "Govt_job"), 
                              selected = "Private"),
                  selectInput("Residence_type", label = "Residence Type:", 
                              choices = list("Urban" = "Urban", "Rural" = "Rural"), 
                              selected = "Urban"),
                  sliderInput("avg_glucose_level", "Average Glucose Level:",
                              min = 55.12, max = 271.74,
                              value = 90),
                  sliderInput("bmi", "BMI:",
                              min = 11.5, max = 92,
                              value = 18.5),
                  selectInput("smoking_status", label = "Smoking Status:", 
                              choices = list("Smoker" = "smokes", "Formerly smoked" = "formerly smoked","Never smoked"="never smoked"), 
                              selected = "never smoked"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("gender",
               "age",
               "hypertension",
               "heart_disease",
               "ever_married",
               "work_type",
               "Residence_type",
               "avg_glucose_level",
               "bmi",
               "smoking_status"
      ),
      Value = as.character(c(input$gender,
                             input$age,
                             input$hypertension,
                             input$heart_disease,
                             input$ever_married,
                             input$work_type,
                             input$Residence_type,
                             input$avg_glucose_level,
                             input$bmi,
                             input$smoking_status)),
      stringsAsFactors = FALSE)
    
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$stroke <- factor(test$smoking_status, levels = c(1, 0))
    
    
    Output <- data.frame(Prediction=predict(final_model,test), round(predict(final_model,test,type="prob"), 2))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)