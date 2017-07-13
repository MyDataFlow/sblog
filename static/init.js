$(document).ready(function() {
  var testEditor;

  $(function() {
    testEditor = editormd("editor", {
        width   : "100%",
        height  : 640,
        syncScrolling : "single",
        path    : "/bower_components/editor.md/lib/"
    });

  });
});
