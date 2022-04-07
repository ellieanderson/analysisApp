
$(function() {
  let observer = new MutationObserver(callback);

  function clickHandler(evt) {
    Shiny.setInputValue('group_select', $(this).children('span').text(), {priority: \"event\"});
  }

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

  observer.observe($('.inner')[0], options);
})
