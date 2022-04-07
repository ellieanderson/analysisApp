$(function() {
  let observer = new MutationObserver(callback); // watches for change in DOM tree

  function clickHandler(evt) { // function is called in callback function - see next function
    Shiny.setInputValue('group_select', $(this).children('span').text()); 
    // shiny - used in shiny
    // setInputValue - group_select from R (or input$group_select in server) is set to the $(this).children().text()
    // $(this) - needed - couldn't select by group
    // .children('span') - still worked w/o it
    // 'span' - worked w/o it - probs has to do with style?
    // .text() - needed - broke w/o it
  }
  
  // another idea: addEventListener()

  function callback(mutations) {
    for (let mutation of mutations) {
      if (mutation.type === 'childList') {
        $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');

      }
    }
  }

  let options = {
    childList: true,
  };

  observer.observe($('.inner')[0], options); //The observe() method takes two arguments: The target, which should be the node or node tree on which to observe for changes; and an options object, which is a MutationObserverInit object that allows you to define the configuration for the observer.
})