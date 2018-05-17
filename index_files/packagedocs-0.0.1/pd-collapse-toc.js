
$(document).ready(function() {
  var firstLevelTOC = $("#sidebar>li");
  firstLevelTOC.each(function() {
    var id = $(this).find(">a").attr("href").replace("#", "");
    id = id + "-toc-collapse";
    $(this).find(">ul").attr("id", id);
  });

  $("#sidebar>li>ul").addClass("collapse");
  $("#sidebar>li>ul").removeClass("in");
  $("#sidebar>li>ul").attr("aria-expanded", "false");
  $("#sidebar>li>ul").css("height", "0px");
  $("#sidebar>li>ul").attr('data-toggle', '');

  $('body').scrollspy('refresh');
});

$(function () {
  var active = true;

  $('#sidebar').on('show.bs.collapse', function () {
    if (active) $('#sidebar .in').collapse('hide');
  });
});

// ensure that collapsible toc elements collapse at end of scroll
$(window).scroll(function() {
   if(this.scrollTO) clearTimeout(this.scrollTO);
   this.scrollTO = setTimeout(function() {
      $(this).trigger('scrollEnd');
   }, 100);
});
$(window).bind('scrollEnd', function() {
  if($(document).data("scroll") != 1) {
    var id = $("#sidebar>li.active>ul").attr("id");
    id = "#" + id;
    $(id).collapse("show");
  }
});

// jQuery for page scrolling feature - requires jQuery Easing plugin
$(function() {
  $('#sidebar li a, .page-scroll').bind('click', function(event) {
    $(document).data("scroll", 1);
    var $anchor = $(this);
    var id = $anchor.attr('href').replace(/\./g, '\\.');
    id = id.replace(/\-/g, '\\-');
    $('html, body').stop().animate({
        scrollTop: $(id).offset().top - 80
    }, 300, 'easeInOutExpo', function() {
      if (history && history.replaceState) {
        history.replaceState({}, "", $anchor.attr('href'));
      }
      // window.location.hash = $anchor.attr('href');
      $(document).data("scroll", 0);
      id = id + "-toc-collapse";
      $(id).collapse("show");
    });
    $(this).blur(); // avoids ugly outline in firefox
    event.preventDefault();
  });
});


