function handleSticky() {
  if($(window).width() > 979) {
    var stickBottom = true;
    if($(window).height() > $("#sidebar-col").height())
      stickBottom = false;
    $("#sidebar-col").stick_in_parent({offset_top: 60, bottoming: stickBottom});
  } else {
    $("#sidebar-col").trigger("sticky_kit:detach");
  }
}

$(document).ready(function() {
  handleSticky();
});

// let a user resize for 250ms before triggering actions
$(window).resize(function() {
  if(this.resizeTO) clearTimeout(this.resizeTO);
  this.resizeTO = setTimeout(function() {
    $(this).trigger('resizeEnd');
  }, 250);
});

$(window).bind('resizeEnd', function() {
  handleSticky();
});

// jQuery for page scrolling feature - requires jQuery Easing plugin
// same function is in pd-collapse-toc.js but with a callback
$(function() {
  $('#sidebar li a, .page-scroll').bind('click', function(event) {
    $(document).data("scroll", 1);
    var $anchor = $(this);
    var id = $anchor.attr('href').replace(/\./g, '\\.');
    id = id.replace(/\-/g, '\\-');
    $('html, body').stop().animate({
        scrollTop: $(id).offset().top - 80
    }, 300, 'easeInOutExpo');
    if (history && history.replaceState) {
      history.replaceState({}, "", $anchor.attr('href'));
    }
    $(this).blur(); // avoids ugly outline in firefox
    // window.location.hash = $anchor.attr('href');
    event.preventDefault();
  });
});
