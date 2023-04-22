;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "wylited"
      user-mail-address "wylited@gmail.com")

(setq doom-theme 'doom-ayu-mirage ; theme
      doom-font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'medium))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/"
      org-agenda-directory "~/org/agenda/"
      org-agenda-files (list "~/org/general.org"
                             "~/org/agenda/"
                             "~/org/"))

(setq org-highest-priority ?A
      org-default-priority ?E
      org-lowest-priority  ?H
      org-agenda-time-grid (quote((daily today remove-match) (0900 1100 1300 1500 1700)  "......" "----------------"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t)

(setq-default
      delete-by-moving-to-trash t                    ; Deletes file to .trash
      window-combination-resize t                    ; Takes new window space from all other windows
      x-stretch-cursor t)                            ; Stretch color to glyph width

(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                       ; Nobody likes to loose work, I certainly don't
      truncat-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 2                          ; It's nice to maintain a little marging
      auto-save-default t
      auto-revert-use-notify nil
      auto-revert-verbose nil)

(global-auto-revert-mode 1)

; Modeline changes
(setq doom-modeline-enable-word-count t
        doom-modeline-icon t
        doom-modeline-header-line nil
        doom-modeline-workspace-name t
        doom-modeline-time t
        doom-modeline-env-version t)

(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
  (display-battery-mode 1))                     ; it's nice to know how much power you have

;; custom keybinds?

;; (doom/set-frame-opacity 0.93) ; slight opacity, 0.93 is the best

;; Capture
(setq org-default-notes-file (concat org-directory "~/org/inbox.org"))

(setq org-capture-templates
      '(("b" "Birthday" entry (file+headline "~/org/agenda/birthdays.org" "Birthdays")
         "* %^ \n%^{Birthday}t")
        ("e" "Event" entry (file+headline "~/org/agenda/events.org" "Events")
         "* %^ \n%^{Timestamp}T")
        ("g" "General" entry (file "~/org/inbox.org")
         "* %^")
        ("h" "Homework" entry (file+headline "~/org/agenda/homework.org" "Homework")
         "* %^ \n%^{Timestamp}T")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %^{Title} \n%i%?")
        ("l" "Link" entry (file "~/org/inbox.org")
         "* %? %^L %^g \n%T")
        ("n" "Note" entry (file "~/org/inbox.org")
         "* %? \n%T")
        ("t" "Todo" entry (file "~/org/inbox.org")
         "* TODO %? \n%T")
        ("w" "Work" entry (file+headline "~/org/agenda/work.org" "Work")
         "* %^ \n%^{Timestamp}T"))
)

;;show different priority levels
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Org Super Agenda - Getting stuff done

(setq org-agenda-custom-commands
      '(("w" "Wyli View"
         ((agenda "" ((org-agenda-span 5)
                      (org-agenda-overriding-header "cal")
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (org-agenda-list "" ((org-agenda-overriding-header "items")
                      (org-super-agenda-groups
                       '((:name "Important"
                                :priority "A"
                                :order 1)
                         (:name "Homework"
                                :tag "hw"
                                :order 2)
                         (:name :"Exams"
                                :tag "exam"
                                :order 3))
                       )))
          ))))

;; dashboard changes

(setq fancy-splash-image "/home/wyli/.config/doom/smallprofile.png")
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "No one is your enemy, you have no enemies.")))

;; Copilot my only true friend
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-c" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("C-x" . 'copilot-accept-completion)))

;; mu4e setup
(require 'mu4e)

(setq mu4e-root-maildir (expand-file-name "~/mail"))

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, GMail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/INBOX"             . ?i)
        ("/[Gmail].Sent Mail" . ?s)
        ("/[Gmail].Trash"     . ?t)))

(setq mu4e-get-mail-command "offlineimap") ;; updating using U in main view
;; consult https://gist.github.com/areina/3879626 if anything goes wrong...

; I like org-modern-mode
; (global-org-modern-mode)

;; push typst-mode directory to the load-path like this

(push (expand-file-name "modules/languages/typst-mode" user-emacs-directory) load-path)
;; load tyspt-mode
(require 'typst-mode)

;; remove annoying af "package cl is deprecated"
;;
(defadvice! fixed-do-after-load-evaluation (abs-file)
  :override #'do-after-load-evaluation
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
  (run-hook-with-args 'after-load-functions abs-file))
