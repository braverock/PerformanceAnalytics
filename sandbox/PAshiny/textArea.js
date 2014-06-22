$(document).on("click", "textarea.inputTextarea", function(evt) {

  // evt.target is the button that was clicked
  var el = $(evt.target);

  // Raise an event to signal that the value changed
  el.trigger("change");
});

var inputTextareaBinding = new Shiny.InputBinding();
$.extend(inputTextareaBinding, {
  find: function(scope) {
    return $(scope).find(".inputTextarea");
  },
  getValue: function(el) {
    return $(el).text();
  },
  setValue: function(el, value) {
    $(el).text(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputTextareaBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".inputTextareaBinding");
  }
});

Shiny.inputBindings.register(inputTextareaBinding);