$(document).ready(function() {
  // config
  var options = {
    // toolbar: document.getElementById('custom-toolbar'),
    editor: document.getElementById('editor'),
    debug: true,
    list: [
      'insertimage', 'blockquote', 'h2', 'h3', 'p', 'code', 'insertorderedlist', 'insertunorderedlist', 'inserthorizontalrule',
      'indent', 'outdent', 'bold', 'italic', 'underline', 'createlink'
    ]
  };

  // create editor
  var pen = window.pen = new Pen(options);

  pen.focus();

  // toggle editor mode
  document.querySelector('#mode').addEventListener('click', function() {
    var text = this.textContent;

    if(this.classList.contains('disabled')) {
      this.classList.remove('disabled');
      pen.rebuild();
    } else {
      this.classList.add('disabled');
      pen.destroy();
    }
  });

  // export content as markdown
  document.querySelector('#save').addEventListener('click', function() {
    var text = pen.toMd();
    $.ajax({
      type: 'post',
      url: '/admin/article',
      data: {'content': text },
      success: function(data) {
          // your code
      }
    });
  });

  // toggle editor mode
  document.querySelector('#hinted').addEventListener('click', function() {
    var pen = document.querySelector('.pen')

    if(pen.classList.contains('hinted')) {
      pen.classList.remove('hinted');
      this.classList.add('disabled');
    } else {
      pen.classList.add('hinted');
      this.classList.remove('disabled');
    }
  });

});
