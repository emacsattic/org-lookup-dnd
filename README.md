**org-lookup-dnd** --- Reference the index of a D&D handbook pdf

This program was meant a) To help me run my Dungeons and Dragons sessions
better, and b) as a exercise for me to learn a bit of lisp.

## WHY?
You've organized your DM notes in org mode and sketched out a plan for the session.
During play, the players are getting hung up on the pros and cons of helping this 
village, when you decide **ORCS ATTACK**. While they were discussing, you readied 
the page with the stat block for Orcs. But then in the first round of combat, your 
wizard asks you a question about the charmed condition. Then you want to skim 
the page in your module about *"what the orcs might know"* to decide what they 
respond to a taunt. Back to the stat page. Okay, this was too easy, let's have 
them get some assistance: the bugbear strike team is here! To the bugbear stat page. 
To the orc stat page. Back to the bugbears.........

I've been in this situation, and I wish I could have just put some links to 
the correct page in the sourcebooks into my planning document on the fly. 
*org-lookup-dnd* to the rescue.

## INSTALLATION
0. Install the dependencies
1. Clone this repository into somewhere in your load-path
2. Load the program somehow, I have this in my ~/.emacs

```emacs-lisp
(use-package org-lookup-dnd
    :bind ("C-c d" . org-lookup-dnd-at-point))
```

3. Customize the variable org-lookup-dnd-sources to point to
one or more pdf files you'd like to run this on.  For example
to index the table of contents on page 2 of the basic rules
(https://media.wizards.com/2018/dnd/downloads/DnD_BasicRules_2018.pdf)
and add 0 from all the page numbers in that index:

```
Path to pdf        : ~/Dowloads/DnD_BasicRules_2018.pdf
Page offset        : 0
First page of index: 2
Last page of index : 2
```

## HOW TO USE IT
Run `org-lookup-dnd-at-point`. If there is a word under the pointer, it will search for that term. Otherwise, write a search term in the minibuffer. If there are more than one matches, you get to pick which one to link to.

## DEPENDENCIES
- pdftotext (from poppler-utils on ubuntu)
- org-pdfview (from melpa)

## RECOMMENDED
Your completion framework of choice. See `M-x customize-variable RET org-lookup-dnd-chose RET`

## HOW IT WORKS
This program extracts the text from the index of the pdf with pdftotext. 
Then, when you want to find a link, it searches (inefficiently) through 
the entries it indexed.

## WHAT'S NEXT
- [ ] It currently only works to pdfs that are nice enough to have everything 
interesting in the index. I'd like to find an elegant way to index things 
semiautomatically. For example, the players handbook has "Feats" as an 
entry in the table of contents, but not the individual feat names. For now, 
you'd have to put that in the org-lookup-dnd-extra-index table.
- [x] ~~A hash table would have faster lookup than a list~~
- [x] ~~If you have multiple entries with the same name, you can't currently tell 
which comes from which source. Currently it inserts all of them after another.~~

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
