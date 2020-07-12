# Useful keyboard shortcuts

This file lists useful keyboard shortcuts. Most keybindings are from a default installation of GNU Emacs, so the keybindings listed here should also be available in the reference cards. I am maintaining this for **my** ease of reference. Suggestions are welcome.

Use `M-x describe-personal-keybindings` to see personal keybindings defined using `bind-key`. Use `C-h m (describe-mode)` to view documentation for the current major mode, including a list of its key bindings.

## Emacs Window

- `C-x C-+` - Increase font size by one step (`text-scale-adjust`).
- `C-x C--` - Decrease font size by one step (`text-scale-adjust`).

## Text manipulation

- `M-l` - Convert following word to lower case (`downcase-word`).
- `M-c` - Capitalize the following word (`capitalize-word`).
- `M-u` - Convert following word to upper case (`upcase-word`).
- `C-t` - Transpose two characters (`transpose-chars`).
- `C-x r t` - Prepend the selected region with characters (`string-rectangle`).
- `M-k` - Kill forward to the end of the sentence (`kill-sentence`).
- `M-t` - Transpose words (`transpose-words` or `subword-transpose`).
- `M-;` - Invoke comment-dwim (`comment-dwim`).
- `M-/` - Try to expand text before point (`hippie-expand`).
- `C-M-\` - Indent all the lines in the region (`indent-region`).
- `M-q` - Fill paragraph at or after point (`fill-paragraph`).

## Marking

- `C-M-h` - Mark function body (`c-mark-function`).
- `C-x h` - Mark whole buffer (`mark-whole-buffer`).
- `M-h` - Put region around current paragraph (`mark-paragraph`).
- `M-@` - Mark the current word (`mark-word` or `subword-mark`).
- `C-M-h` - Select whole function definition (`mark-defun`).
- `C-M-@` - Set mark after end of following balanced expression (`mark-sexp`).
- `C-M-SPC` - Put the mark at the end of the sexp (`mark-sexp`).
- `C-x C-p` - Move point to the beginning of the current page, and set mark at the end (`mark-page`).
- `C-c C-m` - Jump to a visited position in the current buffer (`counsel-mark-ring`).
- `C-x C-@` - Jump to the buffer and position of the latest entry in the global mark ring (`pop-global-mark`)

## Key motion

- `C-M-n` - Move forward over a parenthetical group (`forward-list`).
- `C-M-p` - Move backward over a parenthetical group (`backward-list`).
- `C-M-u` - Move up in parenthesis structure (`backward-up-list`).
- `C-M-d` - Move down in parenthesis structure (`down-list`).
- `C-M-f` - Move forward over a balanced expression (`forward-sexp`).
- `C-M-b` - Move backward over a balanced expression (`backward-sexp`).
- `C-M-k` - Kill balanced expression forward (`kill-sexp`).

- `C-M-a` - Goto the start of a function (`beginning-of-defun`).
- `C-M-e` - Goto the end of a function (`end-of-defun`).
- `M-a` - Move back to the beginning of the sentence (`backward-sentence`).
- `M-e` - Move forward to the end of the sentence (`forward-sentence`).
- `M-{` - Move back to the previous paragraph beginning (`backward-paragraph`).
- `M-}` - Move forward to the next paragraph end (`markdown-forward-paragraph`).

## Dired

- `C-x d` - Show dired buffer (`dired`).
- `g` - Update the entire contents (`revert-buffer`).
- `l` - Update the specified files (`dired-do-redisplay`).
- `k` - Delete the specified file lines (`dired-do-kill-lines`).
- `s` - Toggle between alphabetical and date/time order (`dired-sort-toggle-or-edit`).
- `C` - Copy the specified files (`dired-do-copy`).
- `D` - Delete the specified files (`dired-do-delete`).
- `R` - Rename the specified files (`dired-do-rename`).
- `r` - Rename the specified file (`dired-efap`).
- `i` - Find file (`counsel-find-file`).
- `/` - Filter files (`dired-narrow`).
- `M-<home>` - Go to HOME directory (`dired-go-home`).
- `M-<up>` - Go to the first file/directory (`dired-jump-to-top`).
- `M-<down>` - Go the last file/directory (`dired-jump-to-bottom`).
- `q` - Kill dired buffer (`quit-window`).
- `C-u q` - Kill dired buffer (``).
- `C-x C-j` - Open dired with cursor right on the file you are editing (`dired-jump`).

## Treemacs

- `C-j` - Initialize or toggle treemacs (`treemacs`).
- `n/p` - Navigate between same-level nodes.
- `u` - Go to parent folder.

## Ibuffer

- `S` - Save file (`ibuffer-do-save`).
- `R` - Rename file (`ibuffer-do-rename-uniquely`).
- `m` - Mark file (`ibuffer-mark-forward`).
- `u` - Unmark file (`ibuffer-unmark-forward`).

## Search

- `M-s o` - List all lines in current buffer matching regex (`list-matching-lines`), alias of `occur`.

### Ag

- `<f8>` - Run an ag search in the project (`projectile-ag`).
- `<return>` - Visit the file (``).
- `n` and `p` - Move between results (``).
- `g` - Run the search again (``).
- `q` - Close the buffer (``).
- `k` - Kill the buffer (``).
- `C-c C-p` -
- `C-x C-s` -
- `C-h m` - Show all the keybindings available inside a results buffer (``).

### Ripgrep

### Swiper

- `X M-n` - Search for the symbol at point in the current file, where `X` is the shortcut to invoke `swiper` (`swiper` `thing-at-point`).

## Ivy

- `M-o` - Execute an available action (`ivy-dispatching-done`).
- `C-o` - Shows a Hydra menu in the minibuffer (`hydra-ivy/body`).
- `C-n` - Selects the next candidate (`ivy-next-line`).
- `C-p` - Selects the previous candidate (`ivy-previous-line`).
- `M-<` - Selects the first candidate (`ivy-beginning-of-buffer`).
- `M->` - Selects the last candidate (`ivy-end-of-buffer`).
- `C-v` - Scrolls up by ivy-height lines (`ivy-scroll-up-command`).
- `M-v` - Scrolls down by ivy-height lines (`ivy-scroll-down-command`).
- `//` - Switch to the root directory (`self-insert-command`).
- `~` - Switch to the home directory (`self-insert-command`).
- `C-c C-o` - Save current completion session to a new read-only buffer and exits completion (`ivy-occur`).
- `C-m` - Calls the default action and exits minibuffer (`ivy-done`).
- `C-j` - When completing file names, selects the current directory candidate and starts a new completion session there. Otherwise, it is the same as `ivy-done` (`ivy-alt-done`).
- `C-j` - Exits with the current input instead of the current candidate (`ivy-immediate-done`). This is useful when copying/renaming files with names that match existing files.
- `C-'` - Use `avy` to select candidates (`ivy-avy`).
- `C-c r` - Resume the last ivy completion session (`ivy-resume`).
- `C-M-a` - Invoke one of the available actions (`ivy-read-action`).

Specify extra flags to the search commands with `C-u` (`universal-argument`) before the command.

## Org mode

- `C-u C-c .` - Insert date and time (`org-time-stamp`).
- `C-c C-d` - Insert "DEADLINE" keyword along with a timestamp (`org-deadline`).
- `C-c C-s` - Insert "SCHEDULED" keyword along with a timestamp (`org-schedule`).

## Projectile

- `C-c p f` - Display a list of all files in the project (`projectile-find-file`).
- `C-c p g` - Jump to a project's files using completion based on context (`projectile-find-file-dwim`).
- `C-c p d` - Display a list of all directories in the project (`projectile-find-dir`).
- `C-c p b` - List buffers local to current project (`projectile-switch-to-buffer`).
- `C-c p e` - Jump to recently visited files in project (`projectile-recentf`).
- `C-c p r` - Simple refactoring with text replace in current project (`projectile-replace`).
- `C-c p S` - Save all project buffers (`projectile-save-project-buffers`).
- `C-c p a` - Switch between .h and .c or .cpp files, useful for C/C++ projects (`projectile-find-other-file`).
- `C-c p i` - Invalidate the project cache (if existing) (`projectile-invalidate-cache`).
- `<f5>` - Switch project (`counsel-projectile-switch-project`).
- `<f6>` - Find file (`counsel-projectile-find-file`).
<!-- * `<f7>` - Switch to project buffer (`counsel-projectile-switch-to-buffer`). -->
- `<f7>` - Run an ag search in the project (`counsel-projectile-rg`).
- `C-c p s g` - Grep in project.
- `C-c p v` - Run `vc-dir` on the project root.
- `C-c p k` - Kill all project buffers.
- `C-c p E` - Opens the root `dir-locals-file` of the project.
- `C-c p C-h` - Show all projectile keybindings.

## LaTeX/AUCTeX

- `C-c @ C-n` - Move to next heading (at any level) (`outline-next-visible-heading`).
- `C-c @ C-p` - Move to previous heading (at any level) (`outline-previous-visible-heading`).
- `C-c @ C-f` - Move Forward to next heading at the same level (`outline-forward-same-level`).
- `C-c @ C-b` - Move Backward to previous heading at the same level (`outline-backward-same-level`).
- `C-c C-e` - Make LaTeX environment (\begin{...}-\end{...} pair) (`LaTeX-environment`).
- `C-c ]` - Close LaTeX environment (`LaTeX-close-environment`).
- `C-c C-o C-f` - Toggle folding mode (`TeX-fold-mode`).
- `C-c C-f C-e` - Insert formatted text (`TeX-font`).
  - `C-e` - Insert emphasized text.
  - `C-b` - Insert bold text.
  - `C-i` - Insert italicized text.
  - `C-r` - Insert roman text.
  - `C-t` - Insert typewriter text.
  - `C-s` - Insert slanted text.
- `C-c _` - Set master file (``).
- `C-c ^` - Switch to master file (`TeX-home-buffer`).

### Reftex

- `C-c (` - Create a label (`reftex-label`).
- `C-c )` - Look up a reference (`reftex-reference`).
- `C-c [` - Look up a bibliography reference (`reftex-citation`).
- `C-c =` - Look up the TOC (`reftex-toc`).

To enforce reparsing, call any of the commands described above with a raw `C-u` prefix, or press the `r` key in the label selection buffer, the table of contents buffer, or the index buffer.

## Markdown

- `C-c C-s e` - Make region or word italic (emphasis) (``).
- `C-c C-s s` - Make region or word bold or strong (``).
- `C-c C-s s` - Insert or start blockquote (``).
- `C-c C-s p` - Insert pre-formatted code blocks (``).
- `C-c -` - Insert a horizontal rule (``).
- `C-c C-c v` - Export the file and view in a browser (``).
- `C-c C-c m` - Compile the file and show in another buffer (``).
- `C-c C-j` - Insert a list (`markdown-insert-list-item`).

## JSON

- `C-c C-f` - Format the region/buffer (`json-reformat-region`).
- `C-c C-p` - Display a path to the object at point (`json-mode-show-path`).

## Web mode

- `C-c C-n` - Jump to opening/closing blocks/tags (``).
- `C-c C-f` - Fold code for code blocks (``).
- `C-c C-i` - Indent entire buffer (``).

## Tags

### XRef

- `M-.` - Jump to tag underneath cursor (`xref-find-definitions`).
- `M-*` - Pop back to where you previously invoked `M-.` (`xref-pop-marker-stacker`).
- `M-?` - Find references to the identifier at point (`xref-find-references`).
- `C-M-.` - Find all meaningful symbols that match PATTERN (`xref-find-apropos`).
- `C-o` - Display the source of xref at point in the appropriate window (`xref-show-location-at-point`).
- `<tab>` - Quit _xref_ buffer, then jump to xref on current line (`xref-quit-and-goto-xref`).
- `r` - Perform interactive replacement of FROM with TO in all displayed xrefs (`xref-query-replace-in-results`).

### Counsel Gtags

- `M-.` - Jump to tag underneath cursor (`find-tags`).
- `M-*` - Pop back to where you previously invoked `M-.`.
- `C-c g c` - Create tags (``).
- `C-c g u` - Update tags (``).
- `C-c g s` - Find other symbol (``).
- `C-c g r` - Find reference (``).

### Counsel Etags

- ``
- ``

## Programming

- `C-M-a` - Jump backward to the beginning of the current function (`c-beginning-of-defun`).
- `C-M-e` - Jump forward to the end of the current function (`c-end-of-defun`).
- `C-M-h` - Mark the current function (`c-mark-function`).
- `C-M-k` - Jump to a tag in the current file (`moo-jump-local`).
- `C-M-j` - Select a tag to jump to from tags defined in current directory (`moo-jump-directory`).
- `C-M-i` - Complete symbol at point (`complete-symbol`).

### Python and Elpy

- `C-c C-d` - Get object or function docs (`elpy-doc`).
- `M-<left>` - (`elpy-nav-indent-shift-left`)
- `M-<right>` -
- `M-e` - Jump to the next block (`python-nav-forward-block`)
- `M-a` - Jump to the previous block (`python-nav-backward-block`)
- `C-c C-o` - Overview of the definitions in the current file.
- `M-.` - Jump to the definition of a function or class (`elpy-goto-definition`).
- `C-<up>` - Move up along lines with same indentation as the current line (`elpy-nav-backward-block`).
- `C-<down>` - Go down along lines with same indentation as the current line (`elpy-nav-forward-block`).

### C/C++

#### RTags

- `M-.` - Jump to tag underneath cursor (``).
- `M-,` - Pop stack location (``).

## Flycheck

Within the error list the following key bindings are available:

- `RET` - Go to the current error in the source buffer (``).
- `n` - Jump to the next error (``).
- `p` - Jump to the previous error (``).
- `e` - Explain the error (``).
- `f` - Filter the error list by level (``).
- `F` - Remove the filter (``).
- `S` - Sort the error list by the column at point (``).
- `g` - Check the source buffer and update the error list (``).
- `q` - Quit the error list and hide its window (``).

## LSP

- ``
- ``

## Version control systems

### Git with Magit

Use `magit-status` to display information about the current Git repository, and `magit-dispatch-popup` to see help with keybindings.

<https://magit.vc/manual/magit/Automatic-Refreshing-of-Magit-Buffers.html#Automatic-Refreshing-of-Magit-Buffers>

- `TAB` - Expand and collapse files.
- `n` - Move to next section.
- `p` - Move to previous section.
- `M-n` - Move to next sibling section.
- `M-p` - Move to previous sibling section.
- `s` - Stage item (`magit-stage`).
- `S` - Stage all changed files (`magit-stage-modified`).
- `u` - Unstage item (`magit-unstage`).
- `U` - Unstage all items (`magit-unstage-all`).
- `c` - Commit menu (`magit-commit`).
  - `c` - Create a new commit on HEAD (`magit-commit-create`).
- `C-c C-c` - Finish current editing session (`with-editor-finish`).
- `C-c C-k` - Cancel current editing session (`with-editor-cancel`).
- `l` - Log menu.
- `M-S` - Show all sections.
- `M-H` - Hide all sections.
- `k` - Delete file(s).
- `C-u S` - Stage all untracked and tracked files.
- `g` - Refresh the current buffer (`magit-refresh`).
- `G` - Refreshes all magit buffers (`magit-refresh-all`).
- `k` - Discard changes in an item (`magit-discard-item`).
- `v` - Revert item (`magit-revert-item`).
- `F` - Pull (`magit-pull`).
- `f` - Fetch (`magit-fetch`).

### Svn

- `g` - Refresh status of files (``).
- `m` - Mark file at point (``).
- `u` - Unmark file at point (``).
- `c` - Commit marked files or file at point (``).
- `l` - Display log for marked files or file at point (``).
- `=` - Run diff on the marked files or file at point (``).
- `U` - Update files for currently examined repository (``).

## Use Cases

### Find and replace text in several files in a directory

- Run `M-x rgrep` to find the string.
- Run `M-x wgrep` or use `C-s C-p`.
- Edit the rgrep results, you can use iedit-mode.
- Use `C-x C-s` to commit wgrep.
- Use `C-x s !` to save the changed files.

<http://stackoverflow.com/questions/270930/using-emacs-to-recursively-find-and-replace-in-text-files-not-already-open>

### Search for the symbol at point

- `isearch` - Traditional incremental forward search for regular expression with `C-f`.
- `counsel-grep-or-swiper` - Use `swiper` (with overview of lines) for small buffers and `counsel-grep` for large files with `<f4>`.
- `rgrep` - Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
- `deadgrep` - Start a ripgrep search for SEARCH-TERM (`<f8>`).
- `counsel-rg` - Grep for a string in the current directory using rg (`C-c s r`).
- `counsel-projectile-rg` - Perform incremental search in the current project with rg (`<f7>`).

#### Isearch

- `C-f C-w` - Search for the word from the current cursor position, keep hitting `C-w` to add subsequent words to the search (`isearch-forward-regexp`).

#### Swiper

- `C-f M-j` - Search for the word from the current cursor position (`swiper` `ivy-yank-word`).
- `C-f M-n` - Search for the complete word from under the current cursor (`swiper` `ivy-next-history-element`).
- [An example of excluding \*.el from the files searched by ag](https://github.com/abo-abo/swiper/pull/774)

### List all files

- `C-x j` - List all files in given directory (`sb/counsel-all-files-recursively)
- `C-x f` - Jump to a file below the current directory (`counsel-file-jump`)
