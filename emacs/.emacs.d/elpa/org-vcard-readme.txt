`org-vcard` is a package for exporting and importing vCards from
within Emacs' Org mode.

The main user commands are `org-vcard-export` and
`org-vcard-import`, which are intended to be called
interactively; you can press TAB at many of the minibuffer
prompts to get a list of the available options for a prompt.

Both `org-vcard-export` and `org-vcard-import `are wrappers
around the `org-vcard-transfer-helper` function.
`org-vcard-transfer-helper` can be used to export and import
programatically (i.e. via Emacs Lisp).

Enabling `org-vcard-mode` will add an 'Org-vCard' menu to the menu
bar, from which one can access the various export, import and
customisation options.

This package is working towards full compliance with the
vCard specifications:

vCard 4.0: https://tools.ietf.org/html/rfc6350
vCard 3.0: https://tools.ietf.org/html/rfc2426
vCard 2.1: http://www.imc.org/pdi/vcard-21.txt

If you find any apparent instances of non-compliance that aren't
already noted in the TODO section of the org-vcard README.md
document, please let the maintainers know.

Differences between 4.0 and 3.0 can be found in Appendix A of
RFC6350: https://tools.ietf.org/html/rfc6350#page-73
Note that vCard 3.0 'types' became vCard 4.0 'properties'.

Differences between 3.0 and 2.1 can be found in Section 5 of
RFC2426: https://tools.ietf.org/html/rfc2426#page-37

Point of amusement:
In section 7 of RFC2426, the authors of the standard don't
include the 'N' type in their supposed-version-3.0 vCards.

Please refer to the TODO section of the org-vcard README.md
document for known limitations and/or issues.
