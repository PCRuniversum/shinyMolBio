// URL input binding
// This input binding is very similar to textInputBinding from
// shiny.js.
var renderADplotBinding = new Shiny.InputBinding();

// An input binding must implement these methods
$.extend(renderADplotBinding, {

  // This returns a jQuery object with the DOM element
  find: function(scope) {
    return $(scope).find('.ad-plot');
  },

  // return the ID of the DOM element
  getId: function(el) {
    return el.id;
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    return true;
  },

  markPoints: function(el, toMarkPoints, markType) {
    if (Array.isArray(toMarkPoints) === false)
      toMarkPoints = [toMarkPoints];

    var graphDiv = el.getElementsByClassName('plotly')[0];
    var indecesToMark = [];
    graphDiv.data.forEach(function(point, i) {
      if (toMarkPoints.some(function(toMarkPoints) {
        return point.customdata[0] === toMarkPoints;
              }))
              {
                indecesToMark.push(i);
                if (markType === "highlight") {
                  point.opacity = 1;
                  point.marker.size = 10;
                } else {
                  point.visible = false;
                }
              } else {
                if (markType === "highlight") {
                  point.opacity = 0.4;
                  point.marker.size = 7;
                } else {
                  point.visible = true;
                }
        }
    });
    if (markType === "highlight") {
      var newIndeces = indecesToMark.map(function(_, i)
        { return graphDiv.data.length - i - 1; });
      Plotly.moveTraces(graphDiv, indecesToMark, newIndeces);
    } else {
      Plotly.redraw(graphDiv);
    }
  },

  removeHighlightPoints: function(el) {
    var graphDiv = el.getElementsByClassName('plotly')[0];
    graphDiv.data.forEach(function(point) {
          point.opacity = 1;
          point.marker.size = 7;
    });
    Plotly.redraw(graphDiv);
  },

  // Set up the event listeners so that interactions with the
  // input will result in data being sent to server.
  // callback is a function that queues data to be sent to
  // the server.
  subscribe: function(el, callback) {
    $(el).on('change.renderADplotBinding', function(event) {
      callback(true);
    });
  },

  // Remove the event listeners
  unsubscribe: function(el) {
    $(el).off('.renderADplotBinding');
  },

  // Receive messages from the server.
  // Messages sent by updateCurves() are received by this function.
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('hidePoints')){
      this.markPoints(el, data.hidePoints, "hide");
    };

    if (data.hasOwnProperty('highlightPoints')){
      if (data.highlightPoints.length === 0) {
        this.removeHighlightPoints(el);
      } else {
        this.markPoints(el, data.highlightPoints, "highlight");
      }
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

Shiny.inputBindings.register(renderADplotBinding, 'Kablag.renderADplotBinding');