
var tabs = document.querySelectorAll("#tutorial-tab-bar .tab");
for (var i = 0; i < tabs.length; i++) 
{
  console.log("add click event");
  tabs[i].addEventListener('click', function(e) 
  {
    var tabElement = e.target;
    var tabIndex = parseInt(tabElement.dataset.index) - 1;

    console.log("tab index ", tabIndex)

    var sections = document.querySelectorAll("#tutorial-index > .tutorials > .tutorial-list");
    for (var j = 0; j < sections.length; j++) {
      if (j == tabIndex)
        sections[j].classList.add("selected");
      else
        sections[j].classList.remove("selected");
    }

    var tabs = document.querySelectorAll("#tutorial-tab-bar .tab");
    for (var j = 0; j < tabs.length; j++) {
      if (j == tabIndex)
        tabs[j].classList.add("selected");
      else
        tabs[j].classList.remove("selected");
    }

  })
}

