URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot for all erbot
information, and installation help. 



The files in the `contrib' directory are optional "third-party"
add-ons that are used for erbot.  Not all of them are strictly
required.  The versions here are provided for convenience, and are
also the versions known to work with erbot, but you might want to
fetch their latest versions from their respective authors' websites.


====================================================

For developers: 


Namespaces used by these files: fs-, fs.*-, erb.*-



Next, we attempt to describe the various abbreviations and terms used
in this package.


+---------+------------------------------------------------------------+
|botbbdb  |Name of the bbdb database file used by the bots.            |
+---------+------------------------------------------------------------+
|erb      |ErBot stands for erc robot, and started out from            |
|         |erc-robot.el.  We named the new file erbot, and most        |
|         |namespaces start with erb.                                  |
+---------+------------------------------------------------------------+
|         |                                                            |
+---------+------------------------------------------------------------+
|erbc-    |This referred to erbot-comands. Functions starting with this|
|         |name were availabel to public to frob as they like.  Same   |
|         |for variables.  This was replaced by fs-                    |
+---------+------------------------------------------------------------+
|erbnoc-  |This is like erbc-, except that these commands are NOT      |
|         |available to the general public (at this time).  This one is|
|         |still in use, unlike fs-.  These functions are NOT          |
|         |world-executable or world-writable, but are maintained      |
|         |alognside erbc- functions ..  erbnoc meansd: erb -          |
|         |no-commands... We have now shortened it to erbn-            |
+---------+------------------------------------------------------------+
|erbn-    |Shortening of erbnoc-                                       |
+---------+------------------------------------------------------------+
|fs-      |fsbot is a popular instance of erbot.  At some point, all   |
|         |erbc- prefixes were replaced by fs- for easier read.  Thus, |
|         |to reiterate, these functions are world-readable, weritable |
|         |writable and executable.  (The only exeptions are those that|
|         |are internally converted from fsi-, which are converted to  |
|         |fs- with a special disabled property.)  Summary: rwx for irc|
|         |users.                                                      |
+---------+------------------------------------------------------------+
|fsi-     |Like fs- but these functions and variables are only         |
|         |world-readable and world-executable, but NOT world-writable.|
|         |The "i" stands for immutable (or is it "internal"?).  These |
|         |functions are internally converted to fs- functionserbot    |
|         |usage through erbot-install-symbols.  Summary: r-x for irc  |
|         |users.                                                      |
+---------+------------------------------------------------------------+
|fsn-     |This "fs NOT" would be the logical "---" counterpart for the|
|         |fs.* prefixes above, butits similarity to fs will make      |
|         |reading difficult, so we stick with erbn-                   |
+---------+------------------------------------------------------------+
|All other|.. are also ---, and the only difference from erbn- is      |
|prefixes |aesthetical.                                                |
|         |                                                            |
+---------+------------------------------------------------------------+
|fsbot    | "Free software bot", an instance of fsbot.                 |
+---------+------------------------------------------------------------+
|         |                                                            |
|         |                                                            |
|         |                                                            |
|         |                                                            |
|         |                                                            |
+---------+------------------------------------------------------------+
