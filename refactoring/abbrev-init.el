;; automatically load abbreviations table
(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs"
              abbrev-mode t)
(setq save-abbrevs nil ; do not ask to save new abbrevs when quitting
      dabbrev-case-replace nil ; preserve case when expanding
      )
;;(quietly-read-abbrev-file)
