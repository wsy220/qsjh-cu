// JavaScript Document
var AutoScroll=function(selector){ 
  $(selector).find("ul:first").animate({ 
    marginTop:"-50px" 
  },500,function(){ 
    $(this).css({ 
      marginTop:"0px" 
    }).find("li:first").appendTo(this) 
  }) 
} 
$(document).ready(function(){
  if($(".list_lh ul li").length>0){ 
    setInterval('AutoScroll(".list_lh")', 2000) 
  } 
  else{ 
    $(".list_lh").hide() 
  } 
})