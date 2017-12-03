

var buttons = document.querySelectorAll(".schema .button");
for (i = 0; i < buttons.length; i++) 
{
  buttons[i].addEventListener('mouseenter', function(e) {
    var pathElement = e.target.querySelector('svg path');
    pathElement.setAttribute('fill', '#35729E');
  });
}

for (i = 0; i < buttons.length; i++) 
{
  buttons[i].addEventListener('mouseleave', function(e) {
    var pathElement = e.target.querySelector('svg path');
    pathElement.setAttribute('fill', '#222222');
  });
}


