$(document).ready(function() {
  var editor;
  var md = $('#editor').text();
  $(function() {
    editor = editormd("editor", {
        width   : "100%",
        height  : "100%",
        markdown : md,
        toolbarIcons : function() {
          return [
            "undo", "redo", "|",
            "bold", "del", "italic", "quote", "|",
            "h1", "h2", "h3", "h4", "h5", "h6", "|",
            "list-ul", "list-ol", "hr", "|",
            "watch", "fullscreen"
          ];
        },
        codeFold : true,
        saveHTMLToTextarea : true,    // 保存 HTML 到 Textarea
        searchReplace : true,
        watch : true,                // 关闭实时预览
        htmlDecode : false,            // 开启 HTML 标签解析，为了安全性，默认不开启
        toolbar  : true,             //关闭工具栏
        previewCodeHighlight : false, // 关闭预览 HTML 的代码块高亮，默认开启
        emoji : true,
        taskList : true,
        tocm            : false,         // Using [TOCM]
        tex : false,                   // 开启科学公式TeX语言支持，默认关闭
        flowChart : false,             // 开启流程图支持，默认关闭
        sequenceDiagram : true,       // 开启时序/序列图支持，默认关闭,
        imageUpload : false,
        path    : "/bower_components/editor.md/lib/"
    });
  });
  $('#tags').dropdown({
    allowAdditions: true
  });
});
