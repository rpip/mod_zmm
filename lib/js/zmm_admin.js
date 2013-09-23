// global module manager config
window.zmm = window.zmm || {};
window.zmm.cache = null; //cache list of already installed modules

window.zmm.API_URL = window.location.protocol + '//' + window.location.host + '/api/admin_modules/list';
//load all already installed modules
if(!window.zmm.cache){
    $.get(window.zmm.API_URL, function(data){
    window.zmm.cache = data
  });
}

// paginate zmm table
function z_paginate_table(){
    var currentPage = 0;
    var numPerPage = 30;
    var table = $('#zmm_tbl');
    table.bind('repaginate', function() {
        table.find('tbody tr')
	.hide()
	.slice(currentPage * numPerPage, (currentPage + 1) * numPerPage)
	.show();
    });
    table.trigger('repaginate');
    var numRows = table.find('tbody tr').length;
    var numPages = Math.ceil(numRows / numPerPage);
    var pager = $('#zmm_tbl_pager');
    var page_ul = $("<ul></ul>");
    for (var page = 0; page < numPages; page++) {
        $('<li></li>').append($('<a></a>').text(page + 1).bind('click', {
            newPage: page
        }, function(event) {
            currentPage = event.data['newPage'];
            table.trigger('repaginate');
            $(this).parent().addClass('active');
	    $(this).parent().siblings().removeClass('active');
        })).appendTo(page_ul);
    }
    pager.empty();
    page_ul.appendTo(pager);
    pager.find('ul li:first')
	  .addClass('active');
};

// display modules on remote server
function z_populate_zmm_table(){
    window.zmm.to_install = [];
    $("#zmm_tbl_body").empty().html("Fetching modules...");
    var ZMR = "http://modules.zotonic.com/api/zmr/repositories?callback=?"
    $.getJSON(ZMR, {},function(data){
        $("#zmm_tbl_body").empty();
        $.each(data,function(index, data){
	if(window.zmm.cache.indexOf(data.title) != -1){
	    row_class = "class='zmm-installed z_module_link'";
	}else{
	    row_class = "class='z_module_link'";
	}
	       row_data = "<td >" + data.title + "</td>" + "<td>" + data.repository + "</td>";    
               $("#zmm_tbl_body").append("<tr " + row_class +  " data-id='" + data.title + "'" + 
                                      "data-repo='" + data.repository + "'" + ">" + row_data + "</tr>");
         });
	    z_paginate_table();
        });
}


function z_filter_zmm_table(){
    $('input#zmm_filter').live('keyup', function() {
    var val = $(this).val();
    var rex = new RegExp(val, 'i');
    $('#zmm_tbl tr:gt(0)').hide();
        $('#zmm_tbl tr').filter(function() {
            return rex.test($(this).text());
        }).show();
    // repaginate if the filter input is empty
    if(val.length == 0){
        $("#zmm_tbl").trigger('repaginate');
       }
   });
}


function z_refresh_zmm(){
    z_growl_add("Fetching the latest the modules...");
    z_populate_zmm_table();
}

function z_install_module(module){
         // triger postback notify event on the server
         z_notify('install-module', { 
             z_delegate: 'controller_admin_zmm', 
             module: module.title
     });
}

function z_zmm_queue_install(module){
    // add to list if it's not already added
    window.zmm.to_install = window.zmm.to_install || [];

    if($.grep(window.zmm.to_install, function(m, i){
         return m.title == module.title;
    }) == false ){ window.zmm.to_install.push(module)}
}

function z_zmm_unqueue_install(title){
    window.zmm.to_install = $.grep(window.zmm.to_install,
                                   function(m ,i){return m.title != title});
}


z_populate_zmm_table(); // Populate table with data from ZMR server
z_filter_zmm_table(); // add input filter


// CALLBACKS / EVENT LISTENERS
//attach event to the refresh button
$("button#zmm_refresh_btn").live('click', function(e){
    e.preventDefault();
    z_refresh_zmm();
});


$(".z_module_link").live('click', function(e){
    e.preventDefault();
    if(!$(this).hasClass("zmm-installed")){
    // mark module for installation
    $(this).toggleClass('to_install');
    
    // display install button if it's hidden
    if($("#zmm_install_btn").not(":visible")){
        $("#zmm_install_btn").show();
    }
    title = $(this).attr('data-id');

    // click highlight items to remove from install queue
    if($(this).hasClass("highlight")){
       return  z_zmm_unqueue_install(title);
    } 
    repository = $(this).attr('data-repo');
    module = {title:title, repository: repository};

    // add module to list of modules to install
    z_zmm_queue_install(module);
    }
    else{
	module_name = $(this).attr('data-id');
	z_growl_add(module_name + " is already installed");
    }
});


$("#zmm_install_btn").live('click', function(e){
    e.preventDefault();
    modules = '<ol id="zmm_to_install">';
    $.each(window.zmm.to_install, function(i, m){ 
	modules = modules +  '<li>' + m.title + '</li>'
    });
    install_confirm_text = '<h3>These modules will be installed: </h3> <br />' + modules + "</ol>";
    z_dialog_confirm({
        text: install_confirm_text,
        on_confirm: function(){
            $.each(window.zmm.to_install, function(index, module){
		z_install_module(module)
	    })
    }})
});

