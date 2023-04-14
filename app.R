library(shiny)
library(tidyverse)
library(plotly)

unit_choices <- c("Cups", "Metric")

ui <- fluidPage(
  titlePanel("Baking Calorie Calculator"),
  sidebarLayout(
    sidebarPanel(
      h3("Ingredient Quantities"),
      selectInput(inputId = "unit", label = "Select units of measurement:", 
                  choices = unit_choices),
      uiOutput("selected_unit"),
      h4("Sugars", style = "text-align: center; font-weight: bold;"),
      fluidRow(
        column(4, numericInput("brown_sugar", "Brown Sugar", value = 0)),
        column(4, numericInput("sugar", "Sugar", value = 0)),
        column(4, numericInput("icing_sugar", "Icing Sugar", value = 0))
      ),
      h4("Dairy", style = "text-align: center; font-weight: bold;"),
      fluidRow(
        column(4, numericInput("butter", "Butter", value = 0)),
        column(4, numericInput("milk", "Milk", value = 0)),
        column(4, numericInput("cream_cheese", "Cream Cheese", value = 0)),
        column(4, numericInput("eggs", "Whole Eggs", value = 0))
      ),
      h4("Cereals", style = "text-align: center; font-weight: bold;"),
      fluidRow(
        column(6, numericInput("flour", "Flour", value = 0)),
        column(6, numericInput("flax", "Flax", value = 0)),
        column(6, numericInput("whole_wheat_flour", "Whole Wheat Flour", value = 0)),
        column(6, numericInput("bran", "Bran", value = 0)),
        column(6, numericInput("wheat_germ", "Wheat Germ", value = 0)),
        column(6, numericInput("oats", "Oats", value = 0))
        
      ),
      h4("Oils", style = "text-align: center; font-weight: bold;"),
      fluidRow(
       
        column(6, numericInput("vegetable_oil", "Vegetable Oil", value = 0))
      ),
      h4("Chocolate", style = "text-align: center; font-weight: bold;"),
      fluidRow(
        column(6, numericInput("cocoa_powder", "Cocoa Powder", value = 0)),
        column(6, numericInput("chocolate_chips", "Chocolate Chips", value = 0))
      ),
      h4("Fruit", style = "text-align: center; font-weight: bold;"),
      fluidRow(
        column(6, numericInput("bananas", "Whole Bananas", value = 0))
        
      ),
      numericInput("yield", "Yield", value = 1),
      br(),
      actionButton("calculate", "Calculate"),
      actionButton("clear", "Clear")
    ),
    mainPanel(
      h4("Total Calories"),
      p("Values are approximate and may differ by supplier.", style = "font-style: italic;"),
      verbatimTextOutput("calories"),
      h4("Macronutrients"),
      verbatimTextOutput("protein"),
      verbatimTextOutput("carbs"),
      verbatimTextOutput("fat"),
      plotOutput("macros_chart")
    )
  )
)


server <- function(input, output, session) {
  # Change text in UI to verify which units are selected.
  output$selected_unit <- renderUI({
    if (input$unit == "Cups") {
      tags$p("All measurents in cups except foods sold as one unit (e.g., eggs, bananas)")
    } else if (input$unit == "Metric") {
      tags$p("All measurements in grams (solids) or mililiters (liquids) except foods sold as one unit (e.g., eggs, bananas)")
    }
  })
  # Calculate calories based on units of measurement selected
  observeEvent(input$calculate, {
    if(input$unit == "Metric"){
    butter_calories <- input$butter * 7
    sugar_calories <- input$sugar * 4
    brown_sugar_calories <- input$brown_sugar * 3.81
    eggs_calories <- input$eggs * 78
    flour_calories <- input$flour * 4
    icing_sugar_calories <- input$icing_sugar * 4
    milk_calories <- input$milk * 0.5
    vegetable_oil_calories <- input$vegetable_oil * 9
    cocoa_powder_calories <- input$cocoa_powder * 2
    cream_cheese_calories <- input$cream_cheese * 3
    banana_calories <- input$bananas * 105
    chocolate_chips_calories <- input$chocolate_chips * 4.79
    flax_calories <- input$flax * 5
    whole_wheat_flour_calories <- input$whole_wheat_flour * 3
    bran_calories <- input$bran * 2
    wheat_germ_calories <- input$wheat_germ * 4
    oats_calories <- input$oats * 4
    
    total_calories <- butter_calories + sugar_calories + 
      brown_sugar_calories + eggs_calories + flour_calories + 
      icing_sugar_calories + milk_calories + vegetable_oil_calories + 
      cocoa_powder_calories + cream_cheese_calories + banana_calories + 
      chocolate_chips_calories + flax_calories + whole_wheat_flour_calories + 
      bran_calories + wheat_germ_calories + oats_calories
    total_calories_per_serving <- total_calories / input$yield
    
    output$calories <- renderText({
      paste("Total calories:", round(total_calories_per_serving, 2), "calories per serving")
     })
    
    # PROTEIN --
    
    butter_protein <- input$butter * 227 * 0
    sugar_protein <- input$sugar * 200 * 4
    brown_sugar_protein <- input$brown_sugar * 220 * 3.81
    eggs_protein <- input$eggs * 6
    flour_protein <- input$flour * 125 * 0.1
    icing_sugar_protein <- input$icing_sugar * 120 * 4
    milk_protein <- input$milk * 250 * 0.5
    vegetable_oil_protein <- input$vegetable_oil * 250 * 9
    cocoa_powder_protein <- input$cocoa_powder * 85 * 2
    cream_cheese_protein <- input$cream_cheese * 225 * 3
    banana_protein <- input$bananas * 1.3
    chocolate_chips_protein <- input$chocolate_chips * 170 * 4.79
    flax_protein <- input$flax * 120
    whole_wheat_flour_protein <- input$whole_wheat_flour * 120 * 3
    bran_protein <- input$bran * 47 * 2
    wheat_germ_protein <- input$wheat_germ * 105 * 4
    oats_protein <- input$oats * 80 * 4
    
    total_protein <- butter_protein + sugar_protein + brown_sugar_protein + 
      eggs_protein + flour_protein + icing_sugar_protein + milk_protein + 
      vegetable_oil_protein + cocoa_powder_protein + cream_cheese_protein + 
      banana_protein + chocolate_chips_protein + flax_protein + whole_wheat_flour_protein + 
      bran_protein + wheat_germ_protein + oats_protein
    total_protein_per_serving <- total_protein / input$yield
    
    output$protein <- renderText({
      paste("Total protein:", round(total_protein_per_serving, 2), "grams per serving")
    })
    
    # CARBS --
    
    butter_carbs <- input$butter * 227 * 0
    sugar_carbs <- input$sugar * 200 * 4
    brown_sugar_carbs <- input$brown_sugar * 220 * 3.81
    eggs_carbs <- input$eggs * 0.6
    flour_carbs <- input$flour * 125 * 0.76
    icing_sugar_carbs <- input$icing_sugar * 120 * 4
    milk_carbs <- input$milk * 250 * 0.5
    vegetable_oil_carbs <- input$vegetable_oil * 250 * 9
    cocoa_powder_carbs <- input$cocoa_powder * 85 * 2
    cream_cheese_carbs <- input$cream_cheese * 225 * 3
    banana_carbs <- input$bananas * 27
    chocolate_chips_carbs <- input$chocolate_chips * 170 * 4.79
    flax_carbs <- input$flax * 120
    whole_wheat_flour_carbs <- input$whole_wheat_flour * 120 * 0.72
    bran_carbs <- input$bran * 47 * 2
    wheat_germ_carbs <- input$wheat_germ * 105 * 4
    oats_carbs <- input$oats * 80 * 4
    
    total_carbs <- butter_carbs + sugar_carbs + brown_sugar_carbs + 
      eggs_carbs + flour_carbs + icing_sugar_carbs + milk_carbs + 
      vegetable_oil_carbs + cocoa_powder_carbs + cream_cheese_carbs + 
      banana_carbs + chocolate_chips_carbs + flax_carbs + whole_wheat_flour_carbs + 
      bran_carbs + wheat_germ_carbs + oats_carbs
    total_carbs_per_serving <- total_carbs / input$yield
    
    output$carbs <- renderText({
      paste("Total carbohydrates:", round(total_carbs_per_serving, 2), "grams per serving")
    })
    
    # FATS --
    
    butter_fats <- input$butter * 227 * 0.81
    sugar_fats <- input$sugar * 200 * 4
    brown_sugar_fats <- input$brown_sugar * 220 * 3.81
    eggs_fats <- input$eggs * 5
    flour_fats <- input$flour * 125 * 0.01
    icing_sugar_fats <- input$icing_sugar * 120 * 4
    milk_fats <- input$milk * 250 * 0.5
    vegetable_oil_fats <- input$vegetable_oil * 250 * 9
    cocoa_powder_fats <- input$cocoa_powder * 85 * 2
    cream_cheese_fats <- input$cream_cheese * 225 * 3
    banana_fats <- input$bananas * 0.4
    chocolate_chips_fats <- input$chocolate_chips * 170 * 4.79
    flax_fats <- input$flax * 120
    whole_wheat_flour_fats <- input$whole_wheat_flour * 120 * 0.01
    bran_fats <- input$bran * 47 * 2
    wheat_germ_fats <- input$wheat_germ * 105 * 4
    oats_fats <- input$oats * 80 * 4
    
    total_fats <- butter_fats + sugar_fats + brown_sugar_fats + 
      eggs_fats + flour_fats + icing_sugar_fats + milk_fats + 
      vegetable_oil_fats + cocoa_powder_fats + cream_cheese_fats + 
      banana_fats + chocolate_chips_fats + flax_fats + whole_wheat_flour_fats + 
      bran_fats + wheat_germ_fats + oats_fats
    total_fats_per_serving <- total_fats / input$yield
    
    
    
    output$fat <- renderText({
      paste("Total fat:", round(total_fats_per_serving, 2), "grams per serving")
    })
    
    df <- data.frame(total_fats_per_serving, total_carbs_per_serving, total_protein_per_serving)
    
    df <- df %>% 
      pivot_longer(names_to = "macro", values_to = "g", cols = everything()) %>% 
      mutate(g = as.numeric(g)) %>% 
      mutate(macro = replace(macro, macro == "total_fats_per_serving", "Fat"),
             macro = replace(macro, macro == "total_carbs_per_serving", "Carbohydrate"),
             macro = replace(macro, macro == "total_protein_per_serving", "Protein"))
    
    
    output$macros_chart <- renderPlot({
      
      ggplot(data = df, aes(x = macro, y = g, fill = macro)) +
        geom_col() +
        theme_bw() +
        labs(title = "Macronutrient Breakdown",
             x = "Macronutrient",
             y = "Quantity (g)") +
        theme(legend.position = "none")
      
    })
    
    } else if(input$unit == "Cups"){
      # Input (cups) x weight of 1 cup in grams * calories per 1 gram
      butter_calories <- input$butter * 227 * 7
      sugar_calories <- input$sugar * 200 * 4
      brown_sugar_calories <- input$brown_sugar * 220 * 3.81
      eggs_calories <- input$eggs * 78
      flour_calories <- input$flour * 125 * 4
      icing_sugar_calories <- input$icing_sugar * 120 * 4
      milk_calories <- input$milk * 250 * 0.5
      vegetable_oil_calories <- input$vegetable_oil * 250 * 9
      cocoa_powder_calories <- input$cocoa_powder * 85 * 2
      cream_cheese_calories <- input$cream_cheese * 225 * 3
      banana_calories <- input$bananas * 105
      chocolate_chips_calories <- input$chocolate_chips * 170 * 4.79
      flax_calories <- input$flax * 120
      whole_wheat_flour_calories <- input$whole_wheat_flour * 120 * 3
      bran_calories <- input$bran * 47 * 2
      wheat_germ_calories <- input$wheat_germ * 105 * 4
      oats_calories <- input$oats * 80 * 4
      
      total_calories <- butter_calories + sugar_calories + brown_sugar_calories + 
        eggs_calories + flour_calories + icing_sugar_calories + milk_calories + 
        vegetable_oil_calories + cocoa_powder_calories + cream_cheese_calories + 
        banana_calories + chocolate_chips_calories + flax_calories + whole_wheat_flour_calories + 
        bran_calories + wheat_germ_calories + oats_calories
      total_calories_per_serving <- total_calories / input$yield
      
      output$calories <- renderText({
        paste("Total calories:", round(total_calories_per_serving, 2), "calories per serving")
      })
      
      # PROTEIN --
      
      butter_protein <- input$butter * 227 * 0
      sugar_protein <- input$sugar * 200 * 4
      brown_sugar_protein <- input$brown_sugar * 220 * 3.81
      eggs_protein <- input$eggs * 6
      flour_protein <- input$flour * 125 * 0.1
      icing_sugar_protein <- input$icing_sugar * 120 * 4
      milk_protein <- input$milk * 250 * 0.5
      vegetable_oil_protein <- input$vegetable_oil * 250 * 9
      cocoa_powder_protein <- input$cocoa_powder * 85 * 2
      cream_cheese_protein <- input$cream_cheese * 225 * 3
      banana_protein <- input$bananas * 1.3
      chocolate_chips_protein <- input$chocolate_chips * 170 * 4.79
      flax_protein <- input$flax * 120
      whole_wheat_flour_protein <- input$whole_wheat_flour * 120 * 3
      bran_protein <- input$bran * 47 * 2
      wheat_germ_protein <- input$wheat_germ * 105 * 4
      oats_protein <- input$oats * 80 * 4
      
      total_protein <- butter_protein + sugar_protein + brown_sugar_protein + 
        eggs_protein + flour_protein + icing_sugar_protein + milk_protein + 
        vegetable_oil_protein + cocoa_powder_protein + cream_cheese_protein + 
        banana_protein + chocolate_chips_protein + flax_protein + whole_wheat_flour_protein + 
        bran_protein + wheat_germ_protein + oats_protein
      total_protein_per_serving <- total_protein / input$yield
      
      output$protein <- renderText({
        paste("Total protein:", round(total_protein_per_serving, 2), "grams per serving")
      })
      
      # CARBS --
      
      butter_carbs <- input$butter * 227 * 0
      sugar_carbs <- input$sugar * 200 * 4
      brown_sugar_carbs <- input$brown_sugar * 220 * 3.81
      eggs_carbs <- input$eggs * 0.6
      flour_carbs <- input$flour * 125 * 0.76
      icing_sugar_carbs <- input$icing_sugar * 120 * 4
      milk_carbs <- input$milk * 250 * 0.5
      vegetable_oil_carbs <- input$vegetable_oil * 250 * 9
      cocoa_powder_carbs <- input$cocoa_powder * 85 * 2
      cream_cheese_carbs <- input$cream_cheese * 225 * 3
      banana_carbs <- input$bananas * 27
      chocolate_chips_carbs <- input$chocolate_chips * 170 * 4.79
      flax_carbs <- input$flax * 120
      whole_wheat_flour_carbs <- input$whole_wheat_flour * 120 * 0.72
      bran_carbs <- input$bran * 47 * 2
      wheat_germ_carbs <- input$wheat_germ * 105 * 4
      oats_carbs <- input$oats * 80 * 4
      
      total_carbs <- butter_carbs + sugar_carbs + brown_sugar_carbs + 
        eggs_carbs + flour_carbs + icing_sugar_carbs + milk_carbs + 
        vegetable_oil_carbs + cocoa_powder_carbs + cream_cheese_carbs + 
        banana_carbs + chocolate_chips_carbs + flax_carbs + whole_wheat_flour_carbs + 
        bran_carbs + wheat_germ_carbs + oats_carbs
      total_carbs_per_serving <- total_carbs / input$yield
      
      output$carbs <- renderText({
        paste("Total carbohydrates:", round(total_carbs_per_serving, 2), "grams per serving")
      })
      
      # FATS --
      
      butter_fats <- input$butter * 227 * 0.81
      sugar_fats <- input$sugar * 200 * 4
      brown_sugar_fats <- input$brown_sugar * 220 * 3.81
      eggs_fats <- input$eggs * 5
      flour_fats <- input$flour * 125 * 0.01
      icing_sugar_fats <- input$icing_sugar * 120 * 4
      milk_fats <- input$milk * 250 * 0.5
      vegetable_oil_fats <- input$vegetable_oil * 250 * 9
      cocoa_powder_fats <- input$cocoa_powder * 85 * 2
      cream_cheese_fats <- input$cream_cheese * 225 * 3
      banana_fats <- input$bananas * 0.4
      chocolate_chips_fats <- input$chocolate_chips * 170 * 4.79
      flax_fats <- input$flax * 120
      whole_wheat_flour_fats <- input$whole_wheat_flour * 120 * 0.01
      bran_fats <- input$bran * 47 * 2
      wheat_germ_fats <- input$wheat_germ * 105 * 4
      oats_fats <- input$oats * 80 * 4
      
      total_fats <- butter_fats + sugar_fats + brown_sugar_fats + 
        eggs_fats + flour_fats + icing_sugar_fats + milk_fats + 
        vegetable_oil_fats + cocoa_powder_fats + cream_cheese_fats + 
        banana_fats + chocolate_chips_fats + flax_fats + whole_wheat_flour_fats + 
        bran_fats + wheat_germ_fats + oats_fats
      total_fats_per_serving <- total_fats / input$yield
      
      
      
      output$fat <- renderText({
        paste("Total fat:", round(total_fats_per_serving, 2), "grams per serving")
      })
      
      df <- data.frame(total_fats_per_serving, total_carbs_per_serving, total_protein_per_serving)
      
      df <- df %>% 
        pivot_longer(names_to = "macro", values_to = "g", cols = everything()) %>% 
        mutate(g = as.numeric(g)) %>% 
        mutate(macro = replace(macro, macro == "total_fats_per_serving", "Fat"),
               macro = replace(macro, macro == "total_carbs_per_serving", "Carbohydrate"),
               macro = replace(macro, macro == "total_protein_per_serving", "Protein"))
      
      
      output$macros_chart <- renderPlot({
        
          ggplot(data = df, aes(x = macro, y = g, fill = macro)) +
            geom_col() +
            theme_bw() +
            labs(title = "Macronutrient Breakdown",
                 x = "Macronutrient",
                 y = "Quantity (g)") +
            theme(legend.position = "none")
        
      })
    }
  })
  

  
  
  
  
  
  # Clear fields with clear radio button
  
  observeEvent(input$clear, {
    showModal(modalDialog(
      title = "Warning",
      "Are you sure you want to clear all fields?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmClear", "Yes, clear fields", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirmClear, {
    removeModal()
    updateNumericInput(session, "butter", value = 0)
    updateNumericInput(session, "sugar", value = 0)
    updateNumericInput(session, "brown_sugar", value = 0)
    updateNumericInput(session, "eggs", value = 0)
    updateNumericInput(session, "flour", value = 0)
    updateNumericInput(session, "icing_sugar", value = 0)
    updateNumericInput(session, "milk", value = 0)
    updateNumericInput(session, "vegetable_oil", value = 0)
    updateNumericInput(session, "cocoa_powder", value = 0)
    updateNumericInput(session, "cream_cheese", value = 0)
    updateNumericInput(session, "bananas", value = 0)
    updateNumericInput(session, "chocolate_chips", value = 0)
    updateNumericInput(session, "flax", value = 0)
    updateNumericInput(session, "whole_wheat_flour", value = 0)
    updateNumericInput(session, "bran", value = 0)
    updateNumericInput(session, "wheat_germ", value = 0)
    updateNumericInput(session, "oats", value = 0)
    updateNumericInput(session, "yield", value = 1)
    output$calories <- renderText("")
    output$protein <- renderText("")
    output$carbs <- renderText("")
    output$fat <- renderText("")
    output$macros_chart <- renderPlot(NULL)
  })
  observeEvent(input$clear, {
  showModal(modalDialog(
    title = "Warning",
    "Are you sure you want to clear all fields?",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmClear", "Yes, clear fields", class = "btn-danger")
    )
  ))
})

observeEvent(input$confirmClear, {
  removeModal()
  updateNumericInput(session, "butter", value = 0)
  updateNumericInput(session, "sugar", value = 0)
  updateNumericInput(session, "brown_sugar", value = 0)
  updateNumericInput(session, "eggs", value = 0)
  updateNumericInput(session, "flour", value = 0)
  updateNumericInput(session, "icing_sugar", value = 0)
  updateNumericInput(session, "milk", value = 0)
  updateNumericInput(session, "vegetable_oil", value = 0)
  updateNumericInput(session, "cocoa_powder", value = 0)
  updateNumericInput(session, "cream_cheese", value = 0)
  updateNumericInput(session, "bananas", value = 0)
  updateNumericInput(session, "chocolate_chips", value = 0)
  updateNumericInput(session, "flax", value = 0)
  updateNumericInput(session, "whole_wheat_flour", value = 0)
  updateNumericInput(session, "bran", value = 0)
  updateNumericInput(session, "wheat_germ", value = 0)
  updateNumericInput(session, "oats", value = 0)
  updateNumericInput(session, "yield", value = 1)
  output$calories <- renderText("")
  output$protein <- renderText("")
  output$carbs <- renderText("")
  output$fat <- renderText("")
  output$macros_chart <- renderPlot(NULL)
})

  
  # Clear fields with new unit selector
  rv <- reactiveValues(showDialog = FALSE)
  
  observeEvent(input$unit, {
    if (!is.null(input$unit) && input$unit != "" && rv$showDialog) {
      showModal(modalDialog(
        title = "Warning",
        "Changing the unit will clear all fields. Are you sure you want to continue?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("clearFields", "Yes, clear fields", class = "btn-danger")
        )
      ))
      rv$showDialog <- FALSE
    } else {
      rv$showDialog <- TRUE
    }
  })
  
  observeEvent(input$clearFields, {
    updateNumericInput(session, "butter", value = 0)
    updateNumericInput(session, "sugar", value = 0)
    updateNumericInput(session, "brown_sugar", value = 0)
    updateNumericInput(session, "eggs", value = 0)
    updateNumericInput(session, "flour", value = 0)
    updateNumericInput(session, "icing_sugar", value = 0)
    updateNumericInput(session, "milk", value = 0)
    updateNumericInput(session, "vegetable_oil", value = 0)
    updateNumericInput(session, "cocoa_powder", value = 0)
    updateNumericInput(session, "cream_cheese", value = 0)
    updateNumericInput(session, "bananas", value = 0)
    updateNumericInput(session, "chocolate_chips", value = 0)
    updateNumericInput(session, "flax", value = 0)
    updateNumericInput(session, "whole_wheat_flour", value = 0)
    updateNumericInput(session, "bran", value = 0)
    updateNumericInput(session, "wheat_germ", value = 0)
    updateNumericInput(session, "oats", value = 0)
    updateNumericInput(session, "yield", value = 1)
    output$calories <- renderText("")
    output$protein <- renderText("")
    output$carbs <- renderText("")
    output$fat <- renderText("")
    output$macros_chart <- renderPlot(NULL)
    rv$showDialog <- TRUE
    removeModal()
  })
  
  observeEvent(input$clearFields, {
    rv$showDialog <- TRUE
  })

}


shinyApp(ui = ui, server = server)
