



document.addEventListener('DOMContentLoaded', function() {
  console.log('on dom load');
  var tabs = document.querySelectorAll(".step .tab");
  for (i = 0; i < tabs.length; i++) 
  {
    console.log('add tab listener');

    tabs[i].addEventListener('click', function(e) 
    {
      var clickedElement = e.target;

      var tabElement = parentNode(clickedElement, "tab");
      var tabIndex = parseInt(tabElement.dataset.index);

      var tabsElement = parentNode(clickedElement, "tabs");
      for (j = 0; j < tabsElement.children.length; j++) {
        tabsElement.children[j].classList.remove("selected")
        if ((j +1) == tabIndex)
          tabsElement.children[j].classList.add("selected")
      }
      
      var stepElement = parentNode(clickedElement, "step");
      var images = stepElement.querySelectorAll("img");
      for (j = 0; j < images.length; j++) 
      {
        // var image = images[j]
        images[j].classList.remove("selected");
        if ((j + 1) === tabIndex) {
          images[j].classList.add("selected");
        }
      }
    });
  }

  var buttons = document.querySelectorAll(".code-example .button");
  for (i = 0; i < buttons.length; i++) 
  {
    buttons[i].addEventListener('mouseenter', function(e) {
      var pathElement = e.target.querySelector('svg path');
      pathElement.classList.add('selected');
    });
  }

  for (i = 0; i < buttons.length; i++) 
  {
    buttons[i].addEventListener('mouseleave', function(e) {
      var pathElement = e.target.querySelector('svg path');
      pathElement.setAttribute('fill', '#222222');
      pathElement.classList.remove('selected');
    });
  }


});




function parentNode(el, cls)
{
  while ((el = el.parentElement) && !el.classList.contains(cls));
  return el;
}



