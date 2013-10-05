Mod_ZMM - Zotonic Module Manager
================================
This module is a web frontend for the command line zotonic module manager. 

Its primary use is the installation of modules, and browsing of the module repository.


How it Works
============
* The list of modules are pulled from [The Zotonic Module Repository](http://modules.zotonic.com/api/zmr/repositories)
* Modules are installed in `$ZOTONIC_ROOT/priv/modules`.
* Uses the `bin/zotonic modules` command for module installation, and other operations.

TODO 
====
* A site can specify a list of modules it needs (git url). We can install those modules if the site gets enabled
* Cross check which modules are used by which sites.  [See Zotonic issue 655](https://github.com/zotonic/zotonic/issues/655)
* Unit testing
