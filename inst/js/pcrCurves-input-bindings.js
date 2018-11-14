// URL input binding
// This input binding is very similar to textInputBinding from
// shiny.js.
var pcrCurvesInputBinding = new Shiny.InputBinding();

// An input binding must implement these methods
$.extend(pcrCurvesInputBinding, {

  // This returns a jQuery object with the DOM element
  find: function(scope) {
    return $(scope).find('.pcr-curves');
  },

  // return the ID of the DOM element
  getId: function(el) {
    return el.id;
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    return true;
  },

  // Given the DOM element for the input, set the value
  setValue: function(el, hideCurves) {
    if (Array.isArray(hideCurves) === false)
      hideCurves = [hideCurves];
    hideCurves = hideCurves.map(function (curveId) { return curveId - 1; });

    var graphDiv = document.getElementsByClassName('plotly')[0];

    var ncurves = parseInt(el.dataset.ncurves);
    var showCq = (el.dataset.showcq === 'true');
    var showBaseline = (el.dataset.showbaseline === 'true');

    var hideCqs = [];
    if (showCq)
      hideCqs = hideCurves.map(function (curveId) { return curveId + ncurves; });

    var hideBaselines = [];
    if (showBaseline)
      hideBaselines = hideCurves.map(function (curveId) { return curveId + ncurves * 2; });

    hideTracks = hideCurves.concat(hideCqs).concat(hideBaselines);
    var visF = {
      visible: false
    };
    var visT = {
      visible: true
    };
    Plotly.restyle(graphDiv, visT);
    if (hideTracks.length)
      Plotly.restyle(graphDiv, visF, hideTracks);
  },

  // Set up the event listeners so that interactions with the
  // input will result in data being sent to server.
  // callback is a function that queues data to be sent to
  // the server.
  subscribe: function(el, callback) {
    $(el).on('change.pcrCurvesInputBinding', function(event) {
      callback(true);
    });
  },

  // Remove the event listeners
  unsubscribe: function(el) {
    $(el).off('.pcrCurvesInputBinding');
  },

  // Receive messages from the server.
  // Messages sent by updatePcrPlateInput() are received by this function.
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('hideCurves')){
      this.setValue(el, data.hideCurves)
    };

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
      delay: 250
    };
  }


});

Shiny.inputBindings.register(pcrCurvesInputBinding, 'Kablag.pcrCurvesInput');