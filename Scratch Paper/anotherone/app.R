#library(shiny)
library(shinyWidgets)

js <- HTML("
$(function() {
  let observer = new MutationObserver(callback);

  function clickHandler(evt) {
    Shiny.setInputValue('group_select', $(this).children('span').text(), {priority: \"event\"});
  }

  function callback(mutations) {
    console.log('test');
    for (let mutation of mutations) {
      if (mutation.type === 'childList') {
        $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');

      }
    }
  }

  let options = {
    childList: true,
  };

  observer.observe($('.inner')[0], options);
})
")

# js <- HTML(paste(
#   "$(document).ready(function(){",
#   "  $('body').on('click', \"#test input[value='All']\", function(e) {",
#   "    var isChecked = $(e.target).prop('checked');",
#   "    $(\"#test input[value!='All']\").prop('checked', isChecked);",
#   "  });",
#   "});",
#   sep = "\n"
# ))

choices <- list("A" = c(1, 2, 3, 4, 5), "B" = c(6, 7, 8, 9, 10))

ui <- fluidPage(
    tags$head(tags$script(js)),
    pickerInput("test", choices = choices, multiple = TRUE),
    textOutput("testOutput")
)

server <- function(input, output, session) {
    output$testOutput <- renderText({paste(input$test)})
    
    observeEvent(input$group_select, {
        req(input$group_select)
      if (all(choices[[input$group_select]] %in% input$test)) {             
        sel <- input$test[!(input$test %in% choices[[input$group_select]])]         
      } else {             
          sel <- union(input$test, choices[[input$group_select]])         
      }
      updatePickerInput(session, "test", selected = sel)
    })
}

shinyApp(ui = ui, server = server)
