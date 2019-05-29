**org-lookup-dnd.el** --- Reference the index of a D&D handbook pdf

This program was meant a) To help me run my Dungeons and Dragons sessions
better, and b) as a exercise for me to learn a bit of lisp.


## HOW TO USE IT
1. Load the program somehow, I have this in my ~/.emacs
(use-package org-lookup-dnd
    :bind ("C-c d" . org-lookup-dnd-at-point))
2. Customize the variable org-lookup-dnd-sources to point to
one or more pdf files you'd like to run this on. For example
to index the table of contents on page 4 of your players handbook,
and subtract 6 from all the page numbers in that index:
'(("~/Documents/DnD5ePlayersHandbook.pdf" -6 4 4))


## DEPENDENCIES
- emacs (obviously)
- pdftotext (from poppler-utils on ubuntu)


## LICENCE
- GPL
