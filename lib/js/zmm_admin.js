// global module manager config
window.zmm = window.zmm || {};

// add to list if it's not already added
window.zmm.to_install = window.zmm.to_install || [];

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
    // triger postback notify event on the server
    z_notify('refresh', { 
	z_delegate: 'controller_admin_zmm'});
}

function z_install_module(module){
         // triger postback notify event on the server
         z_notify('install-module', { 
             z_delegate: 'controller_admin_zmm', 
             module: module.title
     });
}

function z_zmm_queue_install(module){
    if($.grep(window.zmm.to_install, function(m, i){
         return m.title == module.title;
    }) == false ){ window.zmm.to_install.push(module)}
}

function z_zmm_unqueue_install(title){
    console.log("unqueue " + title);
    window.zmm.to_install = $.grep(window.zmm.to_install,
                                   function(m ,i){return m.title != title});
}

// CALLBACKS / EVENT LISTENERS
//attach event to the refresh button
$("button#zmm_refresh_btn").live('click', function(e){
    e.preventDefault();
    z_refresh_zmm();
});


$(".z_module_link").live('click', function(e){
    e.preventDefault();
    if(!$(this).hasClass("zmm-installed")){

    title = $(this).attr('data-id');

    // unqueue if already queued for installation
     if($(this).hasClass('to_install')){
	 $(this).toggleClass('to_install');
	 return z_zmm_unqueue_install(title);
    } 

    // mark module for installation
    $(this).toggleClass('to_install');
    
    // display install button if it's hidden
    if($("#zmm_install_btn").not(":visible") && window.zmm.to_install.length > 0){
        $("#zmm_install_btn").show();
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


$(document).ready(function(){
    z_paginate_table();
    z_filter_zmm_table(); // add input filter
});


