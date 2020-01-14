;;; bw-theme.el --- Black/white theme.

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; URL: http://github.com/diasbruno/bw-theme
;; Keywords: themes

;;; Commentary:

;; Black/white theme.
;;
;; Choose colours by settings:
;;
;; (custom-set-variables
;;   '(bw-foreground "yellow")
;;   '(bw-background "blue"))

;; License:

;; See license.md.

;; This file is NOT part of GNU Emacs.

;;; Code:

(deftheme bw
  "Black/white theme.")

(defcustom bw-foreground "black"
  "Foregorund color.")

(defcustom bw-background "white"
  "Backgorund color.")

(defcustom bw-error "brightred"
  "Error color.")
(defcustom bw-warning "DarkOrange1"
  "Warning color.")
(defcustom bw-comment "grey30"
  "Comment color.")

(custom-theme-set-faces
 'bw
 `(default ((t (:family "Menlo" :foundry "nil"
                        :width normal :height 96 :weight normal
                        :slant normal :underline nil
                        :overline nil :strike-through nil
                        :box nil :inverse-video nil
                        :foreground ,bw-foreground :background ,bw-background
                        :stipple nil :inherit nil))))
 '(cursor ((t (:inverse-video t))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(escape-glyph ((((background dark)) (:foreground "grey20"))
                 (((type pc)) (:foreground "magenta"))
                 (t (:foreground "brown"))))
 '(minibuffer-prompt ((t nil)))
 '(highlight ((t (:inverse-video t))))
 `(region ((t (:foreground ,bw-background :background ,bw-foreground))))
 '(shadow ((t (:foreground "grey30"))))
 '(secondary-selection ((t (:inverse-video t))))
 '(trailing-whitespace ((t (:background "red"))))
 `(font-lock-builtin-face
   ((t (:foreground ,bw-foreground))))
 '(font-lock-constant-face
   ((t nil)))
 '(font-lock-doc-face
   ((t (:inherit nil))))
 `(font-lock-function-name-face
   ((t (:foreground ,bw-foreground :bold t))))
 `(font-lock-type-face
   ((t (:foreground ,bw-foreground :bold t))))
 `(font-lock-preprocessor-face
   ((t (:foreground ,bw-foreground :bold t))))
 `(font-lock-warning-face
   ((t (:foreground ,bw-warning))))
 '(font-lock-comment-delimiter-face
   ((default (:inherit (font-lock-comment-face)))))
 `(font-lock-comment-face
   ((t (:foreground ,bw-comment))))
 '(font-lock-variable-name-face
   ((t nil)))
 '(font-lock-keyword-face
   ((t nil)))
 '(font-lock-negation-char-face
   ((t nil)))
 '(font-lock-preprocessor-face
   ((t nil)))
 '(font-lock-regexp-grouping-backslash
   ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct
   ((t nil)))
 '(font-lock-string-face
   ((t nil)))
 '(button ((t (:inherit (link)))))
 `(link ((t (:foreground ,bw-foreground :background ,bw-background) (t (:inherit (underline))))))
 '(link-visited ((default (:inherit (link))) (t (:foreground "cyan"))))
 `(fringe ((t (:background ,bw-background))))
 '(header-line ((default (:inherit (mode-line)))))
 '(tooltip ((t (:inverse-video t))))
 `(mode-line ((t (:foreground ,bw-background :background ,bw-foreground))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:inherit (mode-line)) (t (:inverse-video t)))))
 `(isearch ((t (:foreground ,bw-foreground))))
 `(isearch-fail ((t (:foreground ,bw-foreground :background "red"))))
 '(match ((t (:inverse-video t))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch))))))

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bw)
;;; bw-theme.el ends here
