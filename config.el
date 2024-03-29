;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

setq shell-file-name (executable-find "bash"))

(setq user-full-name "wylited"
      user-mail-address "wylited@gmail.com")

(setq doom-theme 'doom-ayu-dark ; theme
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Lexend" :size 36 :weight 'light ))

(setq-default
 delete-by-moving-to-trash t                    ; Deletes file to .trash
 window-combination-resize t                    ; Takes new window space from all other windows
 x-stretch-cursor t)                            ; Stretch color to glyph width

(setq display-line-numbers-type nil
      undo-limit 80000000                       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                       ; Nobody likes to loose work, I certainly don't
      truncat-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 2                          ; It's nice to maintain a little marging
      auto-save-default t
      auto-revert-use-notify nil
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

(setq org-directory "~/org/"
      org-agenda-directory "~/org/agenda/"
      org-agenda-files (list "~/org/general.org"
                             "~/org/agenda/events.org"
                             "~/org/agenda/"
                             "~/org/"
                             "~/org/agenda/cal/"))

(setq org-agenda-log-mode-items '(closed))

(setq org-log-note-clock-out 't)

(setq org-highest-priority ?A
      org-default-priority ?C
      org-lowest-priority  ?E
      org-agenda-time-grid (quote((daily today remove-match) (0800 1100 1300 1500 1700)  "......" "----------------"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-agenda-start-on-weekday nil)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode))

(setq org-fancy-priorities-list '((?A . "A")
                                  (?B . "B")
                                  (?C . "C")
                                  (?D . "D")
                                  (?E . "E")
                                  (?F . "F")
                                  (?G . "G")
                                  (?H . "H")))
(setq org-todo-keywords
      '((sequence "TODO" "FDBK" "CHCK" "WAIT" "|" "DONE" "DLGTD" "PUSH" "HOLD" "CNCL")))

(setq doom-modeline-enable-word-count nil
      doom-modeline-icon t
      doom-modeline-header-line nil
      doom-modeline-workspace-name t
      doom-modeline-time nil
      doom-modeline-env-version t
      display-time-mode t)

;; Capture
(setq org-default-notes-file (concat org-directory "~/org/inbox.org"))

(setq org-capture-templates
      '(("f" "french anki dict" entry (file+headline "~/org/anki/ib_french_ab.org" "IB French Ab")
         "* %^{french} \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n *** Front\n%\1\n *** Back\n%^{english}\n"))
      )

;; Org Super Agenda - Getting stuff done

(setq org-agenda-custom-commands
      '(("w" "Wyli View"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "-2d")
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          ;; (:name "Habits"
                          ;;       :habit t
                          ;;       :order 1)
                          (:name "Exams"
                           :tag "exams"
                           :order 6)
                          (:name "Homework"
                           :tag "hw"
                           :order 2))
                        )))
          ))))
(org-super-agenda-mode)

(setq-hook! org-mode
  prettify-symbols-alist '(("#+end_quote" . "”")
                           ("#+END_QUOTE" . "”")
                           ("#+begin_quote" . "“")
                           ("#+BEGIN_QUOTE" . "“")
                           ("#+end_src" . "«")
                           ("#+END_SRC" . "«")
                           ("#+begin_src" . "»")
                           ("#+BEGIN_SRC" . "»")
                           ("#+name:" . "»")
                           ("#+NAME:" . "»")))

;; Org habits are really useful for GTD
(require 'org-habit)

;;org-pomodoro customization
(require 'org-pomodoro)
(setq org-pomodoro-length 20
      org-pomodoro-short-break-length 10
      org-pomodoro-long-break-length 20)

;; dashboard changes

(setq fancy-splash-image "/home/wyli/.config/doom/smallprofile-ayu.png")
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "No one is your enemy, you have no enemies.") "\n\n\n\n\n\n\n\n\n\n\n\n"))

;; Copilot my only true friend
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-c" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("C-x" . 'copilot-accept-completion)))

;; remove annoying af "package cl is deprecated"
(defadvice! fixed-do-after-load-evaluation (abs-file)
  :override #'do-after-load-evaluation
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
  (run-hook-with-args 'after-load-functions abs-file))

(setq org-roam-directory "~/org/notes/"
      org-roam-completion-everywhere t)
(org-roam-db-autosync-mode)

(map! :prefix "C-c"
      "r l" #'org-roam-buffer-toggle
      "r f" #'org-roam-node-find
      "r i" #'org-roam-node-insert
      "r x" #'org-roam-capture
      "r m" #'completion-at-point)

(map! :prefix "C-c"
      "a m" #'anki-editor-mode
      "a i" #'anki-editor-insert-note
      "a p" #'anki-editor-push-notes)

(with-eval-after-load 'org (global-org-modern-mode))

(require 'elcord)
(elcord-mode)

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)))

(setq-default mode-line-format nil)

(setq doom-modeline-height 50
      doom-modeline-buffer-encoding nil
      doom-modeline-minor-modes nil
      doom-modeline-enable-word-count nil
      doom-modeline-percent-position '(""))

(custom-set-faces
 '(mode-line ((t (:family "Lexend" :height 210))))
 '(mode-line-active ((t (:family "Lexend" :height 210)))) ; For 29+
 '(mode-line-inactive ((t (:family "Lexend" :height 210)))))

(global-hide-mode-line-mode)
(global-copilot-mode)                   ;
(size-indication-mode)

(setq inhibit-compacting-font-caches t)

(setq-default header-line-format (doom-modeline-set-main-modeline))

(set-face-background 'header-line (face-background 'mode-line))

(setq mode-line-format nil)

(require 'ox-json)

(setq scroll-conservatively 1010000)
