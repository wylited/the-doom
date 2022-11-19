;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "wylited"
      user-mail-address "wylited@gmail.com")

(setq doom-theme 'doom-ayu-mirage ; theme
      doom-font (font-spec :family "Lilex Nerd Font" :size 16 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 20 :weight 'medium))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/"
      org-agenda-directory "~/org/agenda/"
      org-agenda-files (list "~/org/general.org"
                             "~/org/agenda/"
                             "~/org/"
                             "~/org/hkywa"))

(setq org-highest-priority ?A
      org-default-priority ?E
      org-lowest-priority  ?H)

(setq-default
 delete-by-moving-to-trash t                    ; Deletes file to .trash
 window-combination-resize t                    ; Takes new window space from all other windows
 x-stretch-cursor t)                            ; Stretch color to glyph width

(setq undo-limit 80000000                       ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                     ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                       ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"              ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                 ; I can trust my computers ... can't I?
      scroll-margin 2)                          ; It's nice to maintain a little margin

; Modeline changes
(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-icon t
        doom-modeline-header-line nil
        doom-modeline-workspace-name t
        doom-modeline-time t
        doom-modeline-env-version t))

(add-hook! 'doom-modeline-mode-hook
           (progn
  (set-face-attribute 'header-line nil
                      :background (face-background 'mode-line)
                      :foreground (face-foreground 'mode-line))
  ))                       ; Enable time in the mode-

(unless (string-match-p "^Power N/A" (battery)) ; On laptops...
  (display-battery-mode 1))                     ; it's nice to know how much power you have

(if (eq initial-window-system 'x)               ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq default-frame-alist
      (append (list
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 0)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(doom/set-frame-opacity 0.95)
(setq centaur-tabs-style "wave"
      centaur-tabs-set-bar 'under)

(after! doom-modeline
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info vcs word-count)
    '(buffer-position misc-info major-mode)))

;; Auto completion provided by copilot

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

;; treemacs config

(add-hook! 'after-init-hook #'treemacs)
(after! treemacs
(add-hook! 'treemacs-mode-hook (setq window-divider-mode -1
                                     variable-pitch-mode 1
                                     treemacs-follow-mode 1
                                     treemacs-position 'right)))

;; Org Modern Mode setup

;; Minimal UI
(package-initialize)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…")

(global-org-modern-mode)

(use-package! org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t
      org-agenda-start-day nil)

(setq org-agenda-custom-commands
      '(("w" "wyliworld"
         ((agenda "" ((org-agenda-span 1)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date nil
                                :todo "TODAY"
                                :scheduled nil
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:discard (:todo "TODO"))
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 1)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Emacs"
                                 :tag "emacs"
                                 :order 14)
                          (:name "Home"
                                 :tag "home"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "Notes"
                                 :tag "notes"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
