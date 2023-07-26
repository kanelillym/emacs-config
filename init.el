;;; -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq default-directory "~/../../Documents/git")
(setq my/org-file-path "~/../../Documents/org/")

;; Don't display the start page
(setq inhibit-startup-message t)
;; Disable visible scrollbar
(scroll-bar-mode -1)
;; Disable top toolbar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Give space on the edges
(set-fringe-mode 10)
;; Disable menu bar
(menu-bar-mode -1)
;; When there is an alarm ping, flash the screen
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for org, terminal, and shell modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar runemacs/default-font-size 140)

;; By default, use Fira Code Retina font.
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; For monospace, use Fira Code Retina font.
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 125)

;; For non-monospace (used in org mode), use Cantarell font.
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 125 :weight 'regular)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ivy
  ;; :diminish ; I'm not sure what this does, or why it's taken out.
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . counsel-up-directory)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-l" . counsel-up-directory)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-h-delete nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
    :ensure t
    :after evil
    :config
    (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-y") 'yank)
(global-set-key (kbd "C-c k") 'kill-region)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(global-set-key (kbd "C-x x b") 'magit-blame)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/../../Documents/git")
    (setq projectile-project-search-path '("~/../../Documents/git"))
    (setq projectile-switch-project-action #'projectile-dired)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil))

(defun my/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
:hook (org-mode . my/org-mode-setup)
:bind (("C-c a" . org-agenda))

:config
(setq org-ellipsis " ▾")

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-deadline-warning-days 8)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(k@)")))
;; Configure custom agenda view
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (todo "ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("W" "Work Tasks" tags-todo "+job")

        ;;Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<=15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20) ;; TODO tweak
          (org-agenda-files org-agenda-files)))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANCELLED"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))

;; Save org buffers after refiling
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Scheduled TODOs with STYLE: HABIT will show a history chart in the agenda
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
(setq org-habit-preceding-days 15)
(setq org-habit-following-days 5)

(my/org-font-setup))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-bullets
:after org
:hook (org-mode . org-bullets-mode)
:custom
(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (python . t)))
(setq org-confirm-babel-evaluate nil)

(defun my/org-babel-tangle-config ()
(when (string-equal (buffer-file-name)
                    (expand-file-name "~/.emacs.d/config.org")) ; This file
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package org-roam
  :ensure t
  :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat my/org-file-path "roam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n p" . my/org-roam-find-active-project)
         ("C-c n a" . my/org-roam-find-area)
         ("C-c n r" . my/org-roam-find-resource)
         ("C-c n t" . my/org-roam-capture-task)
         ("C-c n e" . my/org-roam-capture-event)
         ("C-c n b" . my/org-roam-capture-inbox)
         ("C-c n R" . org-roam-node-random)
         ("C-c n T a" . org-roam-tag-add)
         ("C-c n T r" . org-roam-tag-remove)
         ("C-c n A a" . org-roam-alias-add)
         ("C-c n A r" . org-roam-alias-remove)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
(org-roam-db-autosync-mode))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)

        ("p" "project" plain
         my/org-roam-para-template
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
         :unnarrowed t)

        ("a" "area" plain
         my/org-roam-para-template
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Area\n")
         :unnarrowed t)

        ("b" "bibliography" plain
         my/org-roam-biblio-template
         :if-new (file+head
                  "%<%Y%m%d%H%M%S>-biblio-${slug}.org" "#+title: ${title}\n#+filetags: biblio\n")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %<%H:%M>: %?"
         :if-new (file+head
                  "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(setq my/org-roam-para-template "* Goals\n\n%?\n\n* Tasks\n\n** NEXT Add initial tasks\n\n* Dates\n\n")
(setq my/org-roam-biblio-template "* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?\n\n* Notes\n\n")

(defun my/org-roam-filter-by-tags (taglist)
  "Create a lambda which returns t iff any string in TAGLIST is a tag on a provided org-roam node."
  (lambda (node)
    (setq check nil)
    (dolist (tag taglist)
      (if (member tag (org-roam-node-tags node))
          (setq check t)))
    check))

(defun my/org-roam-filter-by-tag (tag-name)
  "Takes TAG-NAME, a string, and creates a lambda which return t iff a provided org-roam node is tagged with TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  "Create and return a list of all org-roam nodes which have TAG-NAME as one of their tags."
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-filter-by-tags-exclude-archive (taglist)
  "Does the same thing as my/org-roam-filter-by-tags, but will always return nil if \"Archive\" is a member of the node's tags."
  (lambda (node)
    (setq check nil)
    (dolist (tag taglist)
      (if (and (member tag (org-roam-node-tags node)) (not (member "Archive" (org-roam-node-tags node))))
          (setq check t)))
    check))

(defun my/org-roam-filter-by-tags-exclusive (taglist)
  "Create a filtering lambda which returns nil iff the provided roam node is tagged with any member of taglist, and returns t otherwise"
  (lambda (node)
    (setq check t)
    (dolist (tag taglist)
      (if (member tag (org-roam-node-tags node)) (setq check nil)))
    check))

(defun org-roam-node-insert-immediate (arg &rest args)
  "Insert a link to an org-roam node. If the node does not exist, create it but do not prompt for a template or contents."
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-refresh-agenda-list ()
  "Build the `org-agenda-files' list to be all org-roam nodes which are tagged with any of '(\"Area\" \"Inbox\" \"Project\")."
  (interactive)
  (setq org-agenda-files
        (append (my/org-roam-list-notes-by-tag "Area")
                (my/org-roam-list-notes-by-tag "Inbox")
                (my/org-roam-list-notes-by-tag "Project"))))

;; Always build the org-agenda-files list on startup.
(my/org-roam-refresh-agenda-list)

(defun my/org-roam-project-finalize-hook ()
  "Refreshes `org-agenda-files' to ensure the captured node is added if the capture was not aborted."
  ;; When this hook is invoked, remove it from the hookpoint
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  (unless org-note-abort
    (my/org-roam-refresh-agenda-list)))

(defun my/org-roam-find-active-project ()
  "Find or create a node by title which has the tag \"Project\" and does not have the tag \"Archive\". If the target node does not exist, the creation process is identical to `my/org-roam-find-all-projects'."
  (interactive)
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tags-exclude-archive '("Project"))
   nil
   ;; If the selected node does not exist, override capture templates so that only the Project template is available
   :templates '(("p" "project" plain
                 my/org-roam-para-template
                 :if-new (file+head
                          "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
                 :unnarrowed t))))

(defun my/org-roam-find-all-projects ()
  "Find or create an org node by title which has the tag \"Project\"."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   nil
   :templates '(("p" "project" plain
                 my/org-roam-para-template
                 :if-new (file+head
                          "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
                 :unnarrowed t))))

(defun my/org-roam-find-archive ()
  "Find or create an org node by title which has the tag \"Archive\"."
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary. Capture templates are as default.
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Archive")
   nil))

(defun my/org-roam-find-area ()
  "Find or create an org node by title which has the tag \"Area\"."
  (interactive)
  ;; Add the Area file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select an Area file to open, creating it if necessary
  (org-roam-node-find
   nil nil
   (my/org-roam-filter-by-tag "Area")
   nil
   :templates '(("a" "area" plain
                 my/org-roam-para-template
                 :if-new (file+head
                          "%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Area")
                 :unnarrowed t))))

(defun my/org-roam-find-resource ()
  "Find an org node by title which is not tagged with \"Project\", \"Area\", or \"Inbox\"."
  (interactive)
  (org-roam-node-find
   nil nil
   (my/org-roam-filter-by-tags-exclusive '("Project" "Area" "Inbox"))
   nil)) ;; Don't override default templates if creating a new file

(defun my/org-roam-capture-inbox ()
  "Capture a bullet into the Inbox.org file."
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "default" plain
                                   "* %?"
                                   :if-new (file+head
                                            "Inbox.org" "#+title: Inbox\n")))))

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Ensure that the project or area node is included in the org agenda after the capture is saved
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tags-exclude-archive '("Area" "Project")))
                     :templates '(("p" "project" plain
                                   "** TODO %?"
                                   :if-new (file+head+olp
                                            "%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                            ("Tasks")))

                                  ("a" "area" plain
                                   "** TODO %?"
                                   :if-new (file+head+olp
                                            "%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Area"
                                            ("Tasks"))))))

(defun my/org-roam-capture-event ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tags-exclude-archive '("Area" "Project")))
                     :templates '(("e" "event" plain
                                   "** %?\n%U\n%^T"
                                   :if-new (file+head+olp
                                            "%<%Y%m%d%H%M%S>-${slug}.org"
                                            "#+title: ${title}\n#+category: ${title}"
                                            ("Dates"))))))

(use-package ement)
;; (use-package quelpa-use-package)
;; (quelpa '(ement :repo "alphapapa/ement.el" :fetcher github))

(use-package elpher)

(use-package ox-gemini)

;; (use-package gemini-mode)

(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode "\\.ledger\\'") ; file format which should be treated as a ledger file

(use-package company-ledger) ; Use `company' autocomplete for ledger
(add-hook 'ledger-mode-hook 'company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pomodoro company-ledger ledger-mode org-roam-bibtex org-roam-ui python-black sqlite sqlite3 emacsql-sqlite org-roam emacsql org-pomodoro pytest python-pytest eslint-fix typescript-mode quelpa-use-package visual-fill-column org-bullets forge magit counsel-projectile projectile hydra evil-collection evil general helpful counsel ivy-rich which-key rainbow-delimiters doom-themes doom-modeline all-the-icons ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(org-agenda-list)
(org-agenda-day-view)
(delete-other-windows)
