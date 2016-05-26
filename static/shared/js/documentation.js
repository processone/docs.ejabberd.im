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
  
  // Append "select all code" action link
  function SelectText(text) {
      var doc = document;
      var range;
      var selection;
      ;    
      if (doc.body.createTextRange) {
          range = document.body.createTextRange();
          range.moveToElementText(text);
          range.select();
      } else if (window.getSelection) {
          selection = window.getSelection();        
          range = document.createRange();
          range.selectNodeContents(text);
          selection.removeAllRanges();
          selection.addRange(range);
      }
  }
  function SelectClicked(ev) {
    SelectText($(ev.target).prev().find(".code pre")[0]);
    return false;
  }
  
  $('<a href="#" class="code-action-labels pull-right">Select Code</a>').insertAfter("code[class|=language]");
  $('.code-action-labels').click(SelectClicked);
});
