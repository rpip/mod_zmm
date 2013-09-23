Mod_ZMM - Zotonic Module Manager
================================
This module is a web frontend for the command line zotonic module manager. 

Its primary use is the installation of modules, and browsing of the module repository.


How it Works
============
* The list of modules are pulled from http://modules.zotonic.com/api/zmr/repositories
* Modules are installed to 'priv/modules'.
* Module activation/deactivation/uninstallion and reinstallation are accessible on the site modules page: System > Modules.
* Depends on the CLI Zotonic Module Manager in bin/zmm for module related actions.


TODO
====
* Update CSS/JS to be compatible with boostrap 3
* Add actions for module activation/deactivation/uninstallion and reinstallation
* A site can specify a list of modules it needs (git url). We can install those modules if the site gets enabled
* Cross check which modules are used by which sites [Zotonic issue 648](https://github.com/zotonic/zotonic/issues/648)
* Unit testing