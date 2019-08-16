const amountScrolled = 300;

$(window).scroll(function() {
  if ($(window).scrollTop() > amountScrolled) {
    $('#scrollToTopBtn').fadeIn('slow');
  } else {
    $('#scrollToTopBtn').fadeOut('slow');
  }
});

$('#scrollToTopBtn').click(() => {
  $('html, body').animate({
    scrollTop: 0,
  }, 300);
  return false;
});
