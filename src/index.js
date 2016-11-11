'use strict';

var Elm = require('./Main');

var main = Elm.Main.embed(document.getElementById('main'), {
    hotSwapped : false
});

main.hot.subscribe(function(event, context) {
    context.flags.hotSwapped = true;
});
