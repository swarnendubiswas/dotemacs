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
* `M-t` - Transpose words (`transpose-words` or `subword-transpose`).

#### Marking

* `C-M-h` - Mark function body (`c-mark-function`).
* `C-x h` - Mark whole buffer (`mark-whole-buffer`).
* `M-h` - Put region around current paragraph (`mark-paragraph`).
* `M-@` - Mark the current word (`mark-word` or `subword-mark`).
* `C-M-h` - Select whole function definition (`mark-defun`).
* `C-M-@` - Set mark after end of following balanced expression (`mark-sexp`).
* `C-M-SPC` - Put the mark at the end of the sexp (`mark-sexp`).
* `C-x C-p` - Move point to the beginning of the current page, and set mark at the end (`mark-page`).

#### Key motion

* `C-M-n` - Move forward over a parenthetical group (`forward-list`).
* `C-M-p` - Move backward over a parenthetical group (`backward-list`).
* `C-M-u` - Move up in parenthesis structure (`backward-up-list`).
* `C-M-d` - Move down in parenthesis structure (`down-list`).
* `C-M-f` - Move forward over a balanced expression (`forward-sexp`).
* `C-M-b` - Move backward over a balanced expression (`backward-sexp`).
* `C-M-k` - Kill balanced expression forward (`kill-sexp`).

* `C-M-a` - Goto the start of a function.
* `C-M-e` - Goto the end of a function.
* `M-a` - Move back to the beginning of the sentence.
* `M-e` - Move forward to the end of the sentence.
* `M-{` - Move back to the previous paragraph beginning.
* `M-}` - Move forward to the next paragraph end.

#### Dired

* `g` - Update the entire contents (`revert-buffer`).
* `l` - Update the specified files (`dired-do-redisplay`).
* `k` - Delete the specified file lines (`dired-do-kill-lines`).
* `s` - Toggle between alphabetical and date/time order (`dired-sort-toggle-or-edit`).
* `C` - Copy the specified files (`dired-do-copy`).
* `D` - Delete the specified files (`dired-do-delete`).
* `R` - Rename the specified files (`dired-do-rename`).

#### Ibuffer

* `S` - Save file.
* `R` - Rename file.
* `m` - Mark file.
* `u` - Unmark file.

#### Search

* `M-s o` - List all lines in current buffer matching regex (`list-matching-lines`), alias of `occur`.

#### Swiper

* `X M-n` - Search for the symbol at point in the current file (`swiper` `thing-at-point`).

#### Ivy

* `C-o` - Shows a Hydra menu in the minibuffer (`hydra-ivy/body`).
* `C-n` - Selects the next candidate (`ivy-next-line`).
* `C-p` - Selects the previous candidate (`ivy-previous-line`).
* `M-<` - Selects the first candidate (`ivy-beginning-of-buffer`).
* `M->` - Selects the last candidate (`ivy-end-of-buffer`).
* `C-v` - Scrolls up by ivy-height lines (`ivy-scroll-up-command`).
* `M-v` - Scrolls down by ivy-height lines (`ivy-scroll-down-command`).
* `//` - Switch to the root directory (`self-insert-command`).
* `~` - Switch to the home directory (`self-insert-command`).
* `C-c C-o` - Save current completion session to a new read-only buffer and exits completion (`ivy-occur`).
* `C-m` - Calls the default action and exits minibuffer (`ivy-done`).
* `C-j` - When completing file names, selects the current directory candidate and starts a new completion session there. Otherwise, it is the same as `ivy-done` (`ivy-alt-done`).
* `C-M-j` - Exits with the current input instead of the current candidate (`ivy-immediate-done`).
* `C-'` - Use `avy` to select candidates (`ivy-avy`).

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

#### LaTeX

* `C-c @ C-n` - Move to next heading (at any level)
* `C-c @ C-p` - Move to previous heading (at any level)
* `C-c @ C-f` - Move Forward to next heading at the same level
* `C-c @ C-b` - Move Backward to previous heading at the same level

#### Reftex

* `C-c (` - Create a label.
* `C-c )` - Look up a reference.
* `C-c [` - Look up a bibliography reference.

#### Etags/Ctags

* `M-.` - Jump to tag underneath cursor.
* `M-*` - Pop back to where you previously invoked `M-.`.

#### Python and Elpy

* `C-c C-d` - Get object or function docs (`elpy-doc`).
* `M-<left>` - (`elpy-nav-indent-shift-left`)
* `M-<right>` -
* `C-c C-o` - Overview of the definitions in the current file.
* `M-.` - Jump to the definition of a function or class (`elpy-goto-definition`).


#### Use Cases

* ###### [Find and replace text in several files in a directory](http://stackoverflow.com/questions/270930/using-emacs-to-recursively-find-and-replace-in-text-files-not-already-open)

* ###### [Search for the symbol at point with swiper] `C-f M-n`

Suggestions are welcome.
