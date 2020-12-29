(require 'font-lock)

(defcustom number-separator "," "Separator between integer value.") 
(defcustom number-separator-interval 3 "How to group numbers.")
(defcustom number-separator-ignore-threshold 4 "How many digits should there be before adding separators?")
(defcustom number-separator-decimal-char "." "Character separating integer from decimal value.")

(defvar number-separator--font-lock-keyword
  `((,"\\.?[[:digit:]]+"
     (0 (list
	 'face nil
	 'display (number-separator)))))
  "Font lock keyword to find decimal numbers.")

(defun number-separator ()  
  "Fix the number and return a new string."
  (let ((number (match-string 0)))
    (if (or (string-prefix-p number-separator-decimal-char number)
	    (<= (length number) number-separator-ignore-threshold))
	nil
      (cl-loop for x from (- (length number) number-separator-interval)
	       downto 1
	       by number-separator-interval
	       do (setq number
			(concat (substring number 0 x)
				number-separator
				(substring number x (length number))))
	       finally return number))))

(define-minor-mode number-separator-mode
  "Separate long numbers."
  nil
  " numsep"
  nil  
  (if number-separator-mode
      (progn 
	(push 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil number-separator--font-lock-keyword))
    (font-lock-remove-keywords nil number-separator--font-lock-keyword)))

(provide 'number-separator)




