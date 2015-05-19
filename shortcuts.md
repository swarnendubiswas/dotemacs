## Useful keyboard shortcuts

This file lists useful keyboard shortcuts that might be difficult to remember. These keybindings are mostly from a
default installation of GNU Emacs, so the keybindings listed here should also be available in the reference cards. I am
maintaining this for **my** ease of reference.

Use `M-x describe-personal-keybindings` to see personal keybindings defined using `bind-key`. Use `C-h m
(describe-mode)` to view documentation for the current major mode, including a list of its key bindings.

#### Text manipulation

* `M-l` - Convert following word to lower case (`downcase-word`).
* `M-c` - Capitalize the following word (`capitalize-word`).
* `M-u` - Convert following word to upper case (`upcase-word`).
* `C-t` - Transpose two characters (`transpose-chars`).
* `C-x r t` - Prepend the selected region with characters.
* `M-k` - Kill forward to the end of the sentence (`kill-sentence`).

#### Key motion

* `C-M-n` - Move forward over a parenthetical group.
* `C-M-p` - Move backward over a parenthetical group.
* `C-M-a` - Goto the start of a function.
* `C-M-e` - Goto the end of a function.
* `M-a` - Move back to the beginning of the sentence.
* `M-e` - Move forward to the end of the sentence.
* `M-{` - Move back to the previous paragraph beginning.
* `M-}` - Move forward to the next paragraph end.

#### Ibuffer

* `S` - Save file.
* `R` - Rename file.
* `m` - Mark file.
* `u` - Unmark file.

#### Org mode

* `C-u C-c .` - Insert date and time.

#### Helm

* `C-SPC` - Mark buffer
* `M-D` - Kill marked buffers and quit Helm
* `C-c d` - Kill marked buffers and keep Helm session

#### Projectile

* `C-c p f` - Display a list of all files in the project.
* `C-c p d` - Display a list of all directories in the project.
* `C-c p b` - List buffers local to current project.
* `C-c p e` - Jump to recently visited files in project.
* `C-c p r` - Simple refactoring with text replace in current project.
* `C-c p S` - Save all project buffers.
* `C-c p a` - Switch between .h and .c or .cpp files, useful for C/C++ projects.

#### Reftex

* `C-c (` - Create a label.
* `C-c )` - Look up a reference.
* `C-c [` - Look up a bibliography reference.

#### Etags/Ctags

* `M-.` - Jump to tag underneath cursor.
* `M-*` - Pop back to where you previously invoked `M-.`.

#### Programming

* `C-M-h` - Mark function body (`c-mark-function`).

#### Use Cases

* ###### [Find and replace text in several files in a directory](http://stackoverflow.com/questions/270930/using-emacs-to-recursively-find-and-replace-in-text-files-not-already-open)

Suggestions are welcome.
