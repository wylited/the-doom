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
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

(setq-default
      delete-by-moving-to-trash t                    ; Deletes file to .trash
      window-combination-resize t                    ; Takes new window space from all other windows
      x-stretch-cursor t)                            ; Stretch color to glyph width

(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                       ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      scroll-margin 2)                          ; It's nice to maintain a little margin

; Modeline changes
(setq doom-modeline-enable-word-count t
        doom-modeline-icon t
        doom-modeline-header-line nili
        doom-modeline-workspace-name t
        doom-modeline-time t
        doom-modeline-env-version t)

(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
  (display-battery-mode 1))                     ; it's nice to know how much power you have

;; (doom/set-frame-opacity 0.93) ; slight opacity, 0.93 is the best

(setq org-agenda-custom-commands
      '(("w" "Super wyli view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "important"
                                 :priority>= "C"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Exams"
                                :tag "exams"
                                :order 10)))))))))

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

;; treemacs config

;; (add-hook! 'after-init-hook #'treemacs
;; (after! treemacs
;; (add-hook! 'treemacs-mode-hook (setq window-divider-mode -1
;;                                     variable-pitch-mode 1
;;                                     treemacs-follow-mode 1
;;                                     treemacs-position 'right))) ;; right align, is just better

; I like org-modern-mode
; (global-org-modern-mode)
