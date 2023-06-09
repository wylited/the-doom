;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "wylited"
      user-mail-address "wylited@gmail.com")

(setq doom-theme 'doom-ayu-dark ; theme
      doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 20 :weight 'medium))

(setq-default
      delete-by-moving-to-trash t                    ; Deletes file to .trash
      window-combination-resize t                    ; Takes new window space from all other windows
      x-stretch-cursor t)                            ; Stretch color to glyph width

(setq display-line-numbers-type 'relative
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
                             "~/org/agenda/"
                             "~/org/"
                             "~/org/notes/notes/"))

(setq org-highest-priority ?A
      org-default-priority ?E
      org-lowest-priority  ?H
      org-agenda-time-grid (quote((daily today remove-match) (0900 1100 1300 1500 1700)  "......" "----------------"))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-agenda-start-on-weekday nil)

(setq doom-modeline-enable-word-count t
        doom-modeline-icon t
        doom-modeline-header-line nil
        doom-modeline-workspace-name t
        doom-modeline-time t
        doom-modeline-env-version t
        display-time-mode t)

(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
  (display-battery-mode 1))                     ; it's nice to know how much power you have

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
         ((agenda "" ((org-agenda-span 3)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "-1d")
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
; (require 'mu4e)

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
(defadvice! fixed-do-after-load-evaluation (abs-file)
  :override #'do-after-load-evaluation
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      (mapc #'funcall (cdr a-l-element))))
  (run-hook-with-args 'after-load-functions abs-file))

(require 'ivy-posframe)

(setq ivy-posframe-display-functions-alist
      '((complete-symbol . ivy-posframe-display-at-point)
        (counsel-M-x     . ivy-posframe-display-at-frame-top-center)))

(ivy-posframe-mode 1)

(setq org-roam-directory "~/org/notes/"
      org-roam-completion-everywhere t)
(org-roam-db-autosync-mode)

(setq org-id-link-to-org-use-id t)

(map! :prefix "C-c"
      "r l" #'org-roam-buffer-toggle
      "r f" #'org-roam-node-find
      "r i" #'org-roam-node-insert
      "r m" #'completion-at-point)

(setq org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "${title}\n* TODO " )
      :unnarrowed t)))
