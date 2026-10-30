(function($, Shiny) {
  "use strict";

  var plateSelector = ".pcr-plate-tbl.interactive";
  var wellSelector = "tbody td:not(.empty-well)";

  function getPlate(element) {
    return $(element).closest(".pcr-plate");
  }

  function getWells(plate) {
    return plate.find(plateSelector + " " + wellSelector);
  }

  function notifyChange(plate) {
    plate.trigger("change");
  }

  function setSelected(wells, selected) {
    wells.toggleClass("selected-well", selected);
  }

  function sendHover(plate, ids) {
    window.pcrPlateHighlightWells(plate[0], ids);
    Shiny.setInputValue(plate[0].id + "_hover", ids, {priority: "event"});
  }

  function getLabel(el) {
    return $(el).parent().find("label").filter(function() {
      return this.htmlFor === el.id;
    });
  }

  // Kept global for applications that already call this helper directly.
  window.pcrPlateHighlightWells = function(el, ids) {
    var registered = Shiny.inputBindings.getBindings();
    var entry = registered.find(function(binding) {
      return binding.binding.name === "Kablag.pcrPlateInput";
    });

    if (entry)
      entry.binding.setSomething(el, ids, "highlighted-well");
  };

  var drag = {
    active: false,
    plate: null,
    start: null,
    select: true,
    rectangle: false,
    baseline: null,
    suppressClick: false
  };

  function resetDrag() {
    drag.active = false;
    drag.plate = null;
    drag.start = null;
    drag.baseline = null;
    drag.rectangle = false;
  }

  function finishDrag(notify) {
    if (!drag.active)
      return;

    var plate = drag.plate;
    resetDrag();
    if (notify)
      notifyChange(plate);
  }

  function restoreBaseline() {
    getWells(drag.plate).each(function() {
      $(this).toggleClass("selected-well", drag.baseline.has(this.id));
    });
  }

  function rectangleWells(start, end) {
    var table = $(start).closest("table");
    var rowMin = Math.min(start.parentElement.rowIndex, end.parentElement.rowIndex);
    var rowMax = Math.max(start.parentElement.rowIndex, end.parentElement.rowIndex);
    var colMin = Math.min(start.cellIndex, end.cellIndex);
    var colMax = Math.max(start.cellIndex, end.cellIndex);
    var result = [];

    table.find("tbody tr").each(function() {
      var rowIndex = this.rowIndex;
      if (rowIndex < rowMin || rowIndex > rowMax)
        return;

      $(this).children("td").each(function() {
        if (this.cellIndex >= colMin && this.cellIndex <= colMax &&
            !this.classList.contains("empty-well"))
          result.push(this);
      });
    });

    return $(result);
  }

  function applyDrag(well) {
    if (drag.rectangle) {
      restoreBaseline();
      setSelected(rectangleWells(drag.start, well), drag.select);
    } else {
      setSelected($(well), drag.select);
    }
  }

  // Select or clear the complete plate.
  $(document).on("click.pcrPlate", plateSelector + " th.toggle-all", function() {
    var plate = getPlate(this);
    var wells = getWells(plate);
    if (!wells.length)
      return;

    setSelected(wells, wells.filter(".selected-well").length !== wells.length);
    notifyChange(plate);
  });

  // Select or clear a column. Excluding .toggle-all prevents the old double
  // handling of the top-left header as both "all" and a column.
  $(document).on("click.pcrPlate", plateSelector + " thead th:not(.toggle-all)", function() {
    var plate = getPlate(this);
    var cellIndex = this.cellIndex;
    var wells = getWells(plate).filter(function() {
      return this.cellIndex === cellIndex;
    });
    if (!wells.length)
      return;

    setSelected(wells, wells.filter(".selected-well").length !== wells.length);
    notifyChange(plate);
  });

  $(document).on("dblclick.pcrPlate", plateSelector + " thead th:not(.toggle-all)", function() {
    var plate = getPlate(this);
    var cellIndex = this.cellIndex;
    setSelected(getWells(plate), false);
    setSelected(getWells(plate).filter(function() {
      return this.cellIndex === cellIndex;
    }), true);
    notifyChange(plate);
  });

  // Select or clear a row.
  $(document).on("click.pcrPlate", plateSelector + " tbody tr th", function() {
    var plate = getPlate(this);
    var wells = $(this).siblings("td:not(.empty-well)");
    if (!wells.length)
      return;

    setSelected(wells, wells.filter(".selected-well").length !== wells.length);
    notifyChange(plate);
  });

  $(document).on("dblclick.pcrPlate", plateSelector + " tbody tr th", function() {
    var plate = getPlate(this);
    setSelected(getWells(plate), false);
    setSelected($(this).siblings("td:not(.empty-well)"), true);
    notifyChange(plate);
  });

  // Start brush selection. Shift changes the brush into an Excel-like
  // rectangle between the initial and current wells.
  $(document).on("mousedown.pcrPlate", plateSelector + " " + wellSelector, function(event) {
    if (event.button !== 0 || event.ctrlKey)
      return;

    var plate = getPlate(this);
    drag.active = true;
    drag.plate = plate;
    drag.start = this;
    drag.select = !this.classList.contains("selected-well");
    drag.rectangle = event.shiftKey;
    drag.baseline = new Set(
      getWells(plate).filter(".selected-well").map(function() { return this.id; }).get()
    );
    drag.suppressClick = true;
    applyDrag(this);
    event.preventDefault();
  });

  $(document).on("mouseenter.pcrPlate", plateSelector + " " + wellSelector, function(event) {
    var plate = getPlate(this);

    if (drag.active) {
      // `buttons` becomes zero if mouseup occurred outside the browser or the
      // window lost focus. Finalize instead of leaving a stuck drag gesture.
      if (event.buttons !== 1) {
        finishDrag(true);
      } else if (drag.plate[0] === plate[0]) {
        applyDrag(this);
      }
      return;
    }

    sendHover(plate, this.id);
  });

  $(document).on("mouseleave.pcrPlate", plateSelector + " " + wellSelector, function() {
    if (!drag.active)
      sendHover(getPlate(this), "");
  });

  $(document).on("mouseup.pcrPlate", function(event) {
    if (event.button === 0)
      finishDrag(true);
  });

  $(window).on("blur.pcrPlate", function() {
    finishDrag(true);
  });

  // Suppress the synthetic click following a completed drag. Ctrl+click keeps
  // the existing group-selection behavior.
  $(document).on("click.pcrPlate", plateSelector + " " + wellSelector, function(event) {
    if (drag.suppressClick && !event.ctrlKey) {
      drag.suppressClick = false;
      return;
    }

    var plate = getPlate(this);
    if (event.ctrlKey) {
      var group = this.getAttribute("group");
      var grouped = getWells(plate).filter(function() {
        return this.getAttribute("group") === group;
      });
      setSelected(grouped, grouped.filter(".selected-well").length !== grouped.length);
    } else {
      $(this).toggleClass("selected-well");
    }
    notifyChange(plate);
  });

  $(document).on("dblclick.pcrPlate", plateSelector + " " + wellSelector, function() {
    var plate = getPlate(this);
    setSelected(getWells(plate), false);
    setSelected($(this), true);
    notifyChange(plate);
  });

  // Header hover previews.
  $(document).on("mouseenter.pcrPlate", plateSelector + " th.toggle-all", function() {
    var plate = getPlate(this);
    sendHover(plate, getWells(plate).map(function() { return this.id; }).get());
  });

  $(document).on("mouseleave.pcrPlate", plateSelector + " th.toggle-all", function() {
    sendHover(getPlate(this), "");
  });

  $(document).on("mouseenter.pcrPlate", plateSelector + " thead th:not(.toggle-all)", function() {
    var plate = getPlate(this);
    var cellIndex = this.cellIndex;
    var ids = getWells(plate).filter(function() {
      return this.cellIndex === cellIndex;
    }).map(function() { return this.id; }).get();
    sendHover(plate, ids);
  });

  $(document).on("mouseleave.pcrPlate", plateSelector + " thead th:not(.toggle-all)", function() {
    sendHover(getPlate(this), "");
  });

  $(document).on("mouseenter.pcrPlate", plateSelector + " tbody tr th", function() {
    var plate = getPlate(this);
    var ids = $(this).siblings("td:not(.empty-well)").map(function() {
      return this.id;
    }).get();
    sendHover(plate, ids);
  });

  $(document).on("mouseleave.pcrPlate", plateSelector + " tbody tr th", function() {
    sendHover(getPlate(this), "");
  });

  var pcrPlateInputBinding = new Shiny.InputBinding();

  $.extend(pcrPlateInputBinding, {
    find: function(scope) {
      return $(scope).find(".pcr-plate");
    },

    getId: function(el) {
      return el.id;
    },

    getValue: function(el) {
      var plate = $(el);
      var selected = getWells(plate).filter(".selected-well").map(function() {
        return this.id;
      }).get();

      // Preserve the package's historical contract: no explicit selection
      // means that all non-empty wells are returned.
      return selected.length ? selected : getWells(plate).map(function() {
        return this.id;
      }).get();
    },

    setSomething: function(el, wells, className) {
      var ids = Array.isArray(wells) ? wells : [wells];
      var wanted = new Set(ids.filter(function(id) { return id !== ""; }));
      $(el).find("." + className).removeClass(className);
      getWells($(el)).filter(function() {
        return wanted.has(this.id);
      }).addClass(className);
    },

    subscribe: function(el, callback) {
      $(el).on("change.pcrPlateInputBinding", function() {
        callback(true);
      });
    },

    unsubscribe: function(el) {
      $(el).off(".pcrPlateInputBinding");
    },

    receiveMessage: function(el, data) {
      if (Object.prototype.hasOwnProperty.call(data, "selection"))
        this.setSomething(el, data.selection, "selected-well");

      if (Object.prototype.hasOwnProperty.call(data, "highlighting"))
        this.setSomething(el, data.highlighting, "highlighted-well");

      if (Object.prototype.hasOwnProperty.call(data, "label"))
        getLabel(el).text(data.label);

      notifyChange($(el));
    },

    getState: function(el) {
      return {
        label: getLabel(el).text(),
        value: this.getValue(el)
      };
    },

    getRatePolicy: function() {
      return {policy: "debounce", delay: 100};
    }
  });

  Shiny.inputBindings.register(pcrPlateInputBinding, "Kablag.pcrPlateInput");
})(jQuery, Shiny);
