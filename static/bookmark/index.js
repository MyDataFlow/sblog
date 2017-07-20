$(document).ready(function() {
  $(".ui.negative.basic.button").click(function(){
    var id = $(this).attr('id');
    var approved = function(){
      $.post("/admin/bookmarks/remove/",{'id' : id})
        .done(function(data) { window.location.reload();})
        .fail(function(data) {
            if(data.status == '401'){
              window.location.reload();
            }else{
              window.alert('出错啦');
            }
          });
    };
    $('.ui.basic.modal').modal({
      closable  : false,
      onApprove : function() {
        approved();
      }
    }).modal('show');
  });
});
