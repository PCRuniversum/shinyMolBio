// URL input binding
// This input binding is very similar to textInputBinding from
// shiny.js.
var pcrPlateInputBinding = new Shiny.InputBinding();


// An input binding must implement these methods
$.extend(pcrPlateInputBinding, {

  // This returns a jQuery object with the DOM element
  find: function(scope) {
    return $(scope).find('.pcr-plate');
  },

  // return the ID of the DOM element
  getId: function(el) {
    return el.id;
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    return $(el).find('td.selected-well').map(function() { return this.id; }).get();
  },

  // Given the DOM element for the input, set the value
  setValue: function(el, value) {
    value = value.map(function(el) {return '#' + el});
    alert(value);
    $(el).find(value.join(',')).addClass('selected-well');
  },

  // Set up the event listeners so that interactions with the
  // input will result in data being sent to server.
  // callback is a function that queues data to be sent to
  // the server.
  subscribe: function(el, callback) {
    $(el).on('keyup.pcrPlateInputBinding input.pcrPlateInputBinding', function(event) {
      callback(true);
      // When called with true, it will use the rate policy,
      // which in this case is to debounce at 500ms.
    });
    $(el).on('change.pcrPlateInputBinding', function(event) {
      callback(false);
      // When called with false, it will NOT use the rate policy,
      // so changes will be sent immediately
    });
  },

  // Remove the event listeners
  unsubscribe: function(el) {
    $(el).off('.pcrPlateInputBinding');
  },

  // Receive messages from the server.
  // Messages sent by updatePcrPlateInput() are received by this function.
  receiveMessage: function(el, data) {
    alert("data");
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    if (data.hasOwnProperty('label'))
      $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

    $(el).trigger('change');
  },

  // This returns a full description of the input's state.
  // Note that some inputs may be too complex for a full description of the
  // state to be feasible.
  getState: function(el) {
  return {
  label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
  value: el.value
  };
  },

  // The input rate limiting policy
  getRatePolicy: function() {
  return {
  // Can be 'debounce' or 'throttle'
  policy: 'debounce',
  delay: 500
  };
  }
});

  Shiny.inputBindings.register(pcrPlateInputBinding, 'Kablag.pcrPlateInput');