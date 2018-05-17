$(function() {
  $('.widget-lazyload').recliner({
    attrib: 'data-src', // selector for attribute containing the media src
    throttle: 300,      // millisecond interval at which to process events
    threshold: 1000,    // scroll distance from element before its loaded
    live: true          // auto bind lazy loading to ajax loaded elements
  });
});

$(document).on('lazyload', '.widget-lazyload', function() {
  $(this).prev().fadeOut(800, function() { $(this).remove(); });
});
