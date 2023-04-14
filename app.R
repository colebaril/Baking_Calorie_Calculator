library(shiny)

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
        column(4, numericInput("eggs", "Eggs", value = 0))
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
        column(6, numericInput("bananas", "Bananas", value = 0))
        
      ),
      numericInput("yield", "Yield", value = 1),
      br(),
      actionButton("calculate", "Calculate"),
      actionButton("clear", "Clear")
    ),
    mainPanel(
      h4("Total Calories"),
      verbatimTextOutput("calories")
    )
  )
)


server <- function(input, output, session) {
  # Change text in UI to verify which units are selected.
  output$selected_unit <- renderUI({
    if (input$unit == "Cups") {
      tags$p("All measurents in cups.")
    } else if (input$unit == "Metric") {
      tags$p("All measurements in grams (solids) or mililiters (liquids)")
    }
  })
  # Calculate calories based on units of measurement selected
  observeEvent(input$calculate, {
    if(input$unit == "Metric"){
    butter_calories <- input$butter * 7.2
    sugar_calories <- input$sugar * 3.87
    brown_sugar_calories <- input$brown_sugar * 3.81
    eggs_calories <- input$eggs * 78
    flour_calories <- input$flour * 3.42
    icing_sugar_calories <- input$icing_sugar * 3.57
    milk_calories <- input$milk * 0.47
    vegetable_oil_calories <- input$vegetable_oil * 8.86
    cocoa_powder_calories <- input$cocoa_powder * 3.5
    cream_cheese_calories <- input$cream_cheese * 30.2
    banana_calories <- input$bananas * 105
    chocolate_chips_calories <- input$chocolate_chips * 14.2
    flax_calories <- input$flax * 5
    whole_wheat_flour_calories <- input$whole_wheat_flour * 3.24
    bran_calories <- input$bran * 11.5
    wheat_germ_calories <- input$wheat_germ * 26.8
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
    } else if(input$unit == "Cups"){
      butter_calories <- input$butter * 203.4
      sugar_calories <- input$sugar * 774.2
      brown_sugar_calories <- input$brown_sugar * 815.8
      eggs_calories <- input$eggs * 109
      flour_calories <- input$flour * 455
      icing_sugar_calories <- input$icing_sugar * 383
      milk_calories <- input$milk * 149
      vegetable_oil_calories <- input$vegetable_oil * 1915
      cocoa_powder_calories <- input$cocoa_powder * 393
      cream_cheese_calories <- input$cream_cheese * 982
      banana_calories <- input$bananas * 121
      chocolate_chips_calories <- input$chocolate_chips * 805
      flax_calories <- input$flax * 120
      whole_wheat_flour_calories <- input$whole_wheat_flour * 455
      bran_calories <- input$bran * 120
      wheat_germ_calories <- input$wheat_germ * 120
      oats_calories <- input$oats * 4 * 85
      
      total_calories <- butter_calories + sugar_calories + brown_sugar_calories + 
        eggs_calories + flour_calories + icing_sugar_calories + milk_calories + 
        vegetable_oil_calories + cocoa_powder_calories + cream_cheese_calories + 
        banana_calories + chocolate_chips_calories + flax_calories + whole_wheat_flour_calories + 
        bran_calories + wheat_germ_calories + oats_calories
      total_calories_per_serving <- total_calories / input$yield
      
      output$calories <- renderText({
        paste("Total calories:", round(total_calories_per_serving, 2), "calories per serving")
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
    rv$showDialog <- TRUE
    removeModal()
  })
  
  observeEvent(input$clearFields, {
    rv$showDialog <- TRUE
  })

}


shinyApp(ui = ui, server = server)