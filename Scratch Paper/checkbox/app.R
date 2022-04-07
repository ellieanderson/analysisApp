library(shiny)

js <- paste(
    "$(document).ready(function(){",
    "  $('body').on('click', \"#myCheckbox input[value='All']\", function(e) {",
    "    var isChecked = $(e.target).prop('checked');",
    "    $(\"#myCheckbox input[value!='All']\").prop('checked', isChecked);",
    "  });",
    "});",
    sep = "\n"
)

ui <- fluidPage(
    tags$head(tags$script(HTML(js))),
    uiOutput("checkbox_ui")
)

server <- function(input, output, session) {
    
    output$checkbox_ui <- renderUI({
        checkboxGroupInput(inputId = "myCheckbox",
                           label = NULL,
                           choices = c("All", "A", "B"))
    })
    
}

shinyApp(ui, server)