{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ Zotonic Module Manager _}</h2>
    <p>{_ This module is a web frontend for the command line zotonic module manager. Its primary use is the installation of modules, and browsing of the module repository. _}</p>
</div>

<div id="zmm_tbl_menu">
  <input id="zmm_filter" type="text" placeholder="I am looking for.." class="input-xlarge pull-left">
  <div class="pull-right">
       <span id="zmm-green-box" class="zmm-info-box">Already Installed</span> 
       <span id="zmm-yellow-box" class="zmm-info-box">Marked for Installation</span> 

       <button class="btn btn-primary" id="zmm_refresh_btn" type="submit">{_ Refresh _}</button>
       <button class="btn btn-primary" id="zmm_install_btn" type="submit">{_ Install _}</button>
  </div>
</div>



<table class="table table-bordered table-condensed" id="zmm_tbl">
  <thead>
  <tr>
   <th>Module</th>
   <th>Description</th>
  </tr>
 </thead>
 <tbody id="zmm_tbl_body"><tr><td>Fecthing modules...</tr></td></tbody>
</table>

<div id="zmm_tbl_pager"  class="pagination pagination-centered"></div>


{% lib "js/zmm_admin.js" %}
{% lib "css/zmm_admin.css" %}

{%  endblock %}



