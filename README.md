**org-lookup-dnd.el** --- Reference the index of a D&D handbook pdf

This program was meant a) To help me run my Dungeons and Dragons sessions
better, and b) as a exercise for me to learn a bit of lisp.


## HOW TO USE IT
1. Clone this repository into somewhere in your load-path
2. Load the program somehow, I have this in my ~/.emacs

```emacs-lisp
(use-package org-lookup-dnd
    :bind ("C-c d" . org-lookup-dnd-at-point))
```

3. Customize the variable org-lookup-dnd-sources to point to
one or more pdf files you'd like to run this on. For example
to index the table of contents on page 4 of your players handbook,
and subtract 6 from all the page numbers in that index:

```emacs-lisp
'(("~/Documents/DnD5ePlayersHandbook.pdf" -6 4 4))
```

## DEPENDENCIES
- emacs (obviously)
- pdftotext (from poppler-utils on ubuntu)

## HOW IT WORKS
This program extracts the text from the index of the pdf. You can then
search trough the terms and get an org-mode link to the appropriate page.


## LICENCE
Copyright (C) 2019 Malte Lau Petersen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
