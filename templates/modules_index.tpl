{% extends "admin_base.tpl" %}

{% block title %}{_ Modules _}{% endblock %}

{% block content %}

<div class="edit-header">
    <h2>{_ Zotonic Module Manager _}</h2>
    <p>{_ This module is a web frontend for the command line zotonic module manager. Its primary use is the installation of modules, and browsing of the module repository. _}</p>
</div>

<div id="zmm_tbl_menu" class="well">
  <input id="zmm_filter" type="text" placeholder="I am looking for.." class="input-xlarge pull-left">
  <div class="pull-right">
       <span id="zmm-info-box1" class="zmm-info-box">Already Installed</span>
       <span id="zmm-info-box2" class="zmm-info-box">Marked for Installation</span>

       <button class="btn btn-primary" id="zmm_refresh_btn" type="submit">{_ Refresh _}</button>
       <button class="btn btn-primary" id="zmm_install_btn" type="submit">{_ Install _}</button>
  </div>
</div>

<table class="table" id="zmm_tbl">
  <thead>
  <tr>
   <th><h3>Module</h3></th>
   <th><h3>Description</h3></th>
  </tr>
 </thead>
 <tbody id="zmm_tbl_body">
{% for module in m.zmm %}
{% if module.is_installed %}
<tr class="zmm-installed z_module_link" data-id={{ module.title }} data-repo={{ module.repository }} >
        <td>{{ module.title }}</td>
	<td>{{ module.repository }} </td>
                <td>
                   <div class="pull-right">
		   <div class="dropdown">
       		   <button class="btn dropdown-toggle" role="button" data-toggle="dropdown" data-target="#">
		       Actions <b class="caret"></b>
	            </button>
		       <ul class="dropdown-menu pull-right" role="menu" aria-labelledby="dLabel">
                        {% if module.is_active %}
			        {% wire id="st-" ++ module.title action={module_toggle module=module.title status_id=module.status} %}
			        <li><a tabindex="-1"  id={{ "st-" ++ module.title }}> Deactivate </a></li>
                        {% else %}
			        {% wire id="st-" ++ module.title action={module_toggle module=module.title status_id=module.status} %}
                                <li><a id={{ "st-" ++ module.title }} tabindex="-1" href="#"> Activate </a></li>
                        {% endif %}

                        {% with module.title ++ #id ++ "up", module.title ++ #id ++ "re", module.title ++ #id ++ "un" as updateid, reinstallid, uninstallid %}
			   {% wire id=updateid action={module_update module=module.title} %}
		           <li><a id={{ updateid }} tabindex="-1" href="#">Update</a></li>

			   {% wire id=reinstallid action={module_reinstall module=module.title} %}
		           <li><a id={{ reinstallid }} tabindex="-1" href="#">Reinstall</a></li>

			   <li class="divider"></li>
			   {% wire id=uninstallid action={module_uninstall module=module.title} %}
			   <li><a id={{ uninstallid }} tabindex="-1" href="#">Uninstall</a></li>
			{% endwith %}
		        </ul>
			</div>
                    </div>
                </td>
</tr>

{% else %}
<tr class="z_module_link" data-id={{ module.title }} data-repo={{ module.repository }} >
        <td>{{ module.title }}</td>
	<td>{{ module.repository }} </td>
        <td> </td>
</tr>
{% endif %}
{% endfor %}
</tbody>
</table>

<div id="zmm_tbl_pager"  class="pagination pagination-centered"></div>
{% lib "js/zmm_admin.js" %}
{% lib "css/zmm_admin.css" %}

{%  endblock %}
