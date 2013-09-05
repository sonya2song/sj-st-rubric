// Global Variable
    var ANNOTATION_DATA = {};
    ANNOTATION_DATA['annotations'] = [];

    $( "#submit-url-button" ).click(function() {
        // var article_url="http://seattletimes.com/html/localnews/2019484765_specialed21m.html";

        var article_url = $('#url-box').val();
        ANNOTATION_DATA['article_url'] = article_url;

        console.log(article_url);
        var readability_url = "https://www.readability.com/api/content/v1/parser?token=43e6e0e0b590f00a095a6a0e64f6c9da11783a5a&callback=?&url=";
        var url = readability_url + article_url;
        $.getJSON(url,function(json){
            // fix problem with <p> tags
            var content = json.content;
            content = content.replace(/<[^<>]+>/g, "####");
            content = content.replace(/((\#\#\#\#)(\s+)?)+/g, "<br><br>");
            ANNOTATION_DATA['content'] = content;
            console.log(json);
           $('#document-viewer-container').append(
              "<p> <a href=\"" + article_url + "\" target=\"_blank\"> Original Article </a> </p>" +
               content + "<br><br><br><br><br>"
              );
           $('#document-viewer-container').highlighter({complete:function(){
              $('.highlighter-container input[type=radio]').prop('checked', false);
              $('.highlighter-container').css('visibility', 'visible');

           }});
        });
           
    });
    $( "#submit-form" ).click(function() {
      
      // get tag name
      var tag = $('input[name=tag]:checked', '#pop-up-form').val();
      var article_url = $('#url-box').val();
      console.log(tag);
      // determine color
      var color = '#000';
      if (tag == 'problem') {
        color = "blue";
      } else if (tag == 'solution') {
        color =  "red";
      } else if (tag == 'response') {
        color =  "green";
      } else if (tag == 'result') {
        color =  "yellow";
      } else if (tag =='response_to_solution') {
        color =  "pink";
      } else if (tag == 'response_to_result') {
        color =  "orange";
      }

      var selection = window.getSelection();
      if (selection.toString()!=="") {
        var range = selection.getRangeAt(0);
        var fragment = range.cloneContents();
   
        var div = document.createElement('div');
        div.appendChild( fragment.cloneNode(true) );

        var span = document.createElement("span");
        span.innerHTML = div.innerHTML;
        span.style.backgroundColor = color;
        range.deleteContents();
        range.insertNode(span);
      }


      highlight_dictionary = {
        'tag' : tag,
        'text' : range.toString()
      };

      ANNOTATION_DATA['annotations'].push(highlight_dictionary);
      console.log(ANNOTATION_DATA);
    });  
  