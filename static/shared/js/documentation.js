// Init sidebar
$(function() {
  
  // Prepare tabbed code snippets
  function TabClicked(ev) {
    var i;
    var ch = ev.target.parentNode.children;
    var tch = ev.target.parentNode.nextElementSibling.children;
    for (i = 0; i < ch.length; i++) {
      ch[i].className = tch[i].className = ch[i] == ev.target ? "selected" : "";
    }
  }
  var tabs = document.getElementsByClassName("code-samples-labels");
  for (var i=0; i < tabs.length; i++) {
    for (var j = 0; j < tabs[i].children.length; j++)
        tabs[i].children[j].addEventListener("click", TabClicked, false);
    TabClicked({target:tabs[i].children[0]});
  }
});
