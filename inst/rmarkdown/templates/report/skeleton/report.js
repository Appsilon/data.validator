function classToggle(object) {
    object.classList.toggle('active');
    object.nextElementSibling.classList.toggle('active');
};

$( window ).on( "load", function() {
  $('.ui.accordion')
    .accordion()
  ;
});
