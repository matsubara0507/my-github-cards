"use strict";

function loadJSON(callback) {
   var xobj = new XMLHttpRequest();
       xobj.overrideMimeType("application/json");
   xobj.open('GET', 'static/info.json', true);
   xobj.onreadystatechange = function () {
         if (xobj.readyState == 4 && xobj.status == "200") {
           callback(xobj.responseText);
         }
   };
   xobj.send(null);
}

loadJSON(function(response) {
  var json = JSON.parse(response);
  Elm.Main.init(
    { node: document.getElementById('main')
    , flags: { info: json }
    }
  );
});
