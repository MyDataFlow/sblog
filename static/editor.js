$(document).ready(function() {
  var testEditor;
  var md = $('#editor').text();
  $(function() {
    testEditor = editormd("editor", {
        width   : "100%",
        height  : 640,
        codeFold : true,
        markdown : md,
        //syncScrolling : false,
        saveHTMLToTextarea : true,    // 保存 HTML 到 Textarea
        searchReplace : true,
        watch : false,                // 关闭实时预览
        htmlDecode : "style,script,iframe|on*",            // 开启 HTML 标签解析，为了安全性，默认不开启
        //toolbar  : false,             //关闭工具栏
        //previewCodeHighlight : false, // 关闭预览 HTML 的代码块高亮，默认开启
        toolbarIcons : function() {
          return ["undo", "redo", "|", "bold", "hr", "|", "preview", "watch", "|", "fullscreen", "info", "testIcon", "testIcon2", "file", "faicon"]
        },
        path    : "/bower_components/editor.md/lib/"
    });

  });
  $('#test').dropdown({
    allowAdditions: true
  });
});
