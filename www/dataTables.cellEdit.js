// Minimal DataTables cellEdit plugin for use with Shiny
// Source pattern adapted from https://github.com/ejbeaty/CellEdit

(function ($) {
  $.fn.MakeCellsEditable = function (settings) {
    var table = this.DataTable();
    var defaults = {
      inputCss: 'form-control input-sm',
      onUpdate: null
    };
    var opts = $.extend(defaults, settings);

    table.on('click', 'tbody td', function () {
      var cell = table.cell(this);
      var colIdx = cell.index().column;

      // If columns array is provided, and this column is not editable, skip
      if (opts.columns && $.inArray(colIdx, opts.columns) === -1) return;

      var oldValue = cell.data();

      // Already editing? prevent nested editors
      if ($(this).find('input, select').length > 0) return;

      // Build editor element
      var editor;
      if (typeof opts.getEditor === 'function') {
        editor = opts.getEditor(oldValue, cell, colIdx);
      } else {
        editor = $('<input type="text"/>').val(oldValue);
      }

      editor.addClass(opts.inputCss);
      $(this).empty().append(editor);
      editor.focus();

      // Handle blur / Enter
      editor.on('blur keydown', function (e) {
        if (e.type === 'keydown' && e.which !== 13) return; // Enter only
        var newValue = editor.val();
        cell.data(newValue).draw();

        if (typeof opts.onUpdate === 'function') {
          var rowIdx = cell.index().row;
          opts.onUpdate(cell, rowIdx, colIdx, oldValue, newValue);
        }
      });
    });

    return this;
  };
})(jQuery);
