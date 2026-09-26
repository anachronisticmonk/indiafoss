;;; deck-present.el --- epresent setup for deck.org  -*- lexical-binding: t; -*-

;; Loaded by `emacs -Q -l deck-present.el' (see epresent.sh).  Independent of
;; Doom: epresent itself is vendored in vendor/epresent.el.
;;
;; epresent gives one slide per top-level heading.  This file adds what
;; deck.html has and epresent does not:
;;   - builds: a `#+pause:' line hides the rest of the slide until the next key
;;   - a Nord dark / light palette, toggled with `t'
;;   - screenshots that swap to their img/NAME-light.png twin in light mode
;;   - colour markup: {{{bad(...)}}} and friends, and a trailing «bad» tag
;;     that colours a whole line (see the header of deck.org)
;;   - one move per key press: a held key or a bouncing clicker cannot race
;;     through the builds
;;
;;   DECK_THEME=light ./epresent.sh     start in light mode
;;   DECK_ALL=1 ./epresent.sh           every build revealed, for review

;; --- native compilation OFF (same reasoning as present.el) ------------------
(setq native-comp-jit-compilation nil
      native-comp-enable-subr-trampolines nil
      native-comp-async-report-warnings-errors nil
      inhibit-automatic-native-compilation t
      warning-suppress-types '((native-compiler) (comp))
      warning-suppress-log-types '((native-compiler) (comp))
      warning-minimum-level :error)

(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      make-backup-files nil
      auto-save-default nil
      create-lockfiles nil
      use-dialog-box nil
      confirm-kill-emacs nil)

(defconst deck-dir (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path (expand-file-name "vendor" deck-dir))

(require 'cl-lib)
(require 'org)
(require 'flyspell)                     ; epresent calls `flyspell-mode-off'
(require 'epresent)

;; --- LaTeX: $...$ and \[...\] render as images --------------------------------
;; dvisvgm gives SVGs, which stay sharp at stage size; dvipng is the fallback.
;; Images take the default face's colour when made, so `deck-toggle-theme'
;; re-renders them.  Output is cached in ltximg/, so each theme pays once.
;; If TeX breaks, the talk still runs with the raw source showing.
(let ((tex "/Library/TeX/texbin"))     ; MacTeX, in case PATH lacks it
  (when (and (file-directory-p tex) (not (member tex exec-path)))
    (add-to-list 'exec-path tex t)
    (setenv "PATH" (concat (getenv "PATH") ":" tex))))
(setq org-preview-latex-default-process
      (if (and (image-type-available-p 'svg) (executable-find "dvisvgm"))
          'dvisvgm 'dvipng)
      org-preview-latex-image-directory
      (expand-file-name "ltximg/" (file-name-directory
                                   (or load-file-name buffer-file-name))))
;; `auto' takes the colour of the face the fragment sits in: ink in a
;; paragraph, accent in a #+begin_quote formula box.
(setq org-format-latex-options
      (plist-put (plist-put org-format-latex-options :foreground 'auto)
                 :background "Transparent"))

;; The palette, inside LaTeX: \cgood{...} \cbad{...} \cfaint{...} \cdim{...}
;; \caccent{...} \cwarn{...} \cviolet{...} \cink{...}.  The header is part of
;; org's image cache key, so a theme switch re-renders in the new colours.
(defvar deck--latex-base-header org-format-latex-header)

(defun deck--latex-rgb (hex)
  (mapconcat (lambda (i) (format "%.3f" (/ (string-to-number
                                             (substring hex i (+ i 2)) 16)
                                            255.0)))
             '(1 3 5) ","))

(defun deck--latex-header (palette)
  (concat deck--latex-base-header "\n"
          (mapconcat
           (lambda (k)
             (format "\\definecolor{deck%s}{rgb}{%s}\n\\newcommand{\\c%s}[1]{{\\color{deck%s}#1}}"
                     k (deck--latex-rgb (plist-get palette (intern (concat ":" k))))
                     k k))
           '("ink" "dim" "faint" "accent" "good" "bad" "warn" "violet")
           "\n")))

(defun deck--latex-safely (fn &rest args)
  ;; `auto' colours come from `face-at-point', so fontify first (a quote
  ;; block is not accent-coloured until font-lock has run), and fall back to
  ;; `default' where a fragment has no face: org errors on a nil face.
  (save-restriction (widen) (font-lock-ensure))
  (let ((orig (symbol-function 'face-at-point)))
    (cl-letf (((symbol-function 'face-at-point)
               (lambda (&rest a)
                 (let ((f (apply orig a)))
                   (if (and f (symbolp f) (facep f)) f 'default)))))
      (condition-case err (apply fn args)
        (error (message "LaTeX preview failed: %s" (error-message-string err)))))))
(advice-add 'org-latex-preview :around #'deck--latex-safely)

(defun deck--render-latex ()
  "Re-render every LaTeX fragment in the colours of the current theme."
  (save-restriction
    (widen)
    (org-latex-preview '(64))
    (let ((org-format-latex-options
           (plist-put (copy-tree org-format-latex-options)
                      :scale epresent-format-latex-scale)))
      (org-latex-preview '(16)))))

(setq org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t
      org-adapt-indentation nil
      org-startup-with-inline-images nil)

;; --- a small Lean 4 mode, for colour only (as in present.el) ----------------
(defvar deck-lean-font-lock
  `((,(regexp-opt '("theorem" "lemma" "def" "abbrev" "example" "instance"
                    "structure" "inductive" "namespace" "end" "open" "import"
                    "variable" "where")
                  'symbols)
     . font-lock-keyword-face)
    (,(regexp-opt '("by" "fun" "let" "have" "show" "from" "match" "with"
                    "if" "then" "else" "at" "using" "in")
                  'symbols)
     . font-lock-builtin-face)
    (,(regexp-opt '("Prop" "Type" "Nat" "Int" "Bool" "List" "Option"
                    "true" "false")
                  'symbols)
     . font-lock-type-face)
    ("\\_<\\(sorry\\)\\_>" 1 font-lock-warning-face)))

(define-derived-mode deck-lean-mode prog-mode "Lean4"
  "Minimal Lean 4 mode: enough for legible slides."
  (setq-local comment-start "-- ")
  (setq-local font-lock-defaults '(deck-lean-font-lock)))
(add-to-list 'org-src-lang-modes '("lean" . deck-lean))

;; --- palette: the same tokens as deck.html ----------------------------------
(defconst deck-palettes
  '((dark
     :ground "#242933" :panel "#2E3440" :rule "#434C5E"
     :ink "#E5E9F0" :dim "#96A0B5" :faint "#6B7689" :accent "#88C0D0"
     :good "#A3BE8C" :bad "#BF616A" :warn "#EBCB8B" :violet "#B48EAD"
     :pill-bg "#3B4252" :good-bg "#424D48" :bad-bg "#493640" :accent-bg "#3C4D59"
     :good-line "#4A7150" :bad-line "#7D454C")
    (light
     :ground "#ECEFF4" :panel "#E3E7EE" :rule "#C3CBD9"
     :ink "#2E3440" :dim "#4C566A" :faint "#737D90" :accent "#3E7896"
     :good "#557A3E" :bad "#A63D48" :warn "#8F6400" :violet "#83577E"
     :pill-bg "#D8DEE9" :good-bg "#CBD5CC" :bad-bg "#DDC8CE" :accent-bg "#C6D5DF"
     :good-line "#9DBA88" :bad-line "#D59AA0")))

(defvar deck-theme 'dark)

(defun deck--first-font (&rest names)
  (or (cl-find-if (lambda (f) (member f (font-family-list))) names)
      (car (last names))))
(defvar deck-mono (deck--first-font "Fira Code" "JetBrains Mono" "Menlo"))
(defvar deck-serif (deck--first-font "Iowan Old Style" "Palatino" "Georgia"))

(dolist (f '(deck-dim deck-faint deck-accent deck-good deck-bad deck-warn
             deck-violet deck-punch deck-punch-bad deck-punch-em deck-sub
             deck-subtitle deck-pill deck-pill-good deck-pill-bad deck-pill-on
             deck-rule deck-bar))
  (custom-declare-face f '((t)) "deck.org presentation face." :group 'epresent))

(defun deck--face (face &rest attrs)
  (when (facep face) (apply #'set-face-attribute face nil attrs)))

(defun deck-apply-theme (theme)
  "Colour everything from the THEME palette, `dark' or `light'."
  (setq deck-theme theme
        org-format-latex-header (deck--latex-header (alist-get theme deck-palettes)))
  (cl-destructuring-bind
      (&key ground panel rule ink dim faint accent good bad warn violet
            pill-bg good-bg bad-bg accent-bg good-line bad-line)
      (alist-get theme deck-palettes)
    (deck--face 'default :foreground ink :background ground)
    (deck--face 'fringe :background ground)
    (deck--face 'internal-border :background ground)
    (dolist (f '(mode-line mode-line-inactive header-line))
      (deck--face f :foreground faint :background ground :box nil
                  :overline nil :underline nil :height 0.55))
    (deck--face 'minibuffer-prompt :foreground accent)
    ;; slide furniture
    (deck--face 'epresent-heading-face :inherit nil :foreground ink
                :family deck-serif :weight 'bold :height 1.55)
    (deck--face 'epresent-subheading-face :inherit nil :foreground accent)
    (deck--face 'epresent-title-face :inherit nil :foreground ink
                :family deck-serif :weight 'bold :height 1.8)
    (deck--face 'epresent-author-face :inherit nil :foreground faint)
    (deck--face 'epresent-date-face :inherit nil :foreground faint)
    (deck--face 'epresent-bullet-face :foreground accent)
    (deck--face 'org-level-1 :inherit nil :foreground ink)
    (deck--face 'bold :foreground ink :weight 'bold)
    (deck--face 'italic :foreground accent :slant 'normal)
    (deck--face 'org-block :background panel :foreground ink :extend t)
    (deck--face 'org-code :background panel :foreground warn)
    (deck--face 'org-verbatim :background panel :foreground warn)
    (deck--face 'org-quote :background panel :foreground accent
                :slant 'normal :extend t :height 1.25)
    (deck--face 'org-table :foreground dim)
    (deck--face 'org-macro :inherit nil :foreground ink)
    ;; code, as the deck's highlighter colours it
    (deck--face 'font-lock-keyword-face :foreground accent :weight 'normal)
    (deck--face 'font-lock-string-face :foreground good)
    (deck--face 'font-lock-doc-face :foreground good :slant 'normal)
    (deck--face 'font-lock-comment-face :foreground faint :slant 'italic)
    (deck--face 'font-lock-comment-delimiter-face :foreground faint :slant 'italic)
    (dolist (f '(font-lock-builtin-face font-lock-constant-face
                 font-lock-number-face font-lock-type-face))
      (deck--face f :foreground violet :weight 'normal))
    (deck--face 'font-lock-function-name-face :foreground ink)
    (deck--face 'font-lock-variable-name-face :foreground ink)
    (deck--face 'font-lock-preprocessor-face :foreground warn)
    (deck--face 'font-lock-warning-face :foreground bad :weight 'bold)
    ;; markup
    (deck--face 'deck-dim :foreground dim)
    (deck--face 'deck-faint :foreground faint)
    (deck--face 'deck-accent :foreground accent)
    (deck--face 'deck-good :foreground good)
    (deck--face 'deck-bad :foreground bad)
    (deck--face 'deck-warn :foreground warn)
    (deck--face 'deck-violet :foreground violet)
    (deck--face 'deck-punch :foreground ink :family deck-serif :height 1.3)
    (deck--face 'deck-punch-bad :foreground bad :family deck-serif :height 1.3)
    (deck--face 'deck-punch-em :foreground accent :family deck-serif :height 1.3)
    (deck--face 'deck-sub :foreground dim :height 0.7)
    (deck--face 'deck-subtitle :foreground accent :family deck-serif
                :slant 'italic :height 1.25)
    ;; a box in the fill colour is padding: 10px either side, 4px above/below
    (deck--face 'deck-pill :foreground ink :background pill-bg
                :box `(:line-width (10 . 4) :color ,pill-bg))
    (deck--face 'deck-pill-good :foreground good :background good-bg :weight 'semi-bold
                :box `(:line-width (10 . 4) :color ,good-bg))
    (deck--face 'deck-pill-bad :foreground bad :background bad-bg :weight 'semi-bold
                :box `(:line-width (10 . 4) :color ,bad-bg))
    (deck--face 'deck-pill-on :foreground accent :background accent-bg :weight 'semi-bold
                :box `(:line-width (10 . 4) :color ,accent-bg))
    (deck--face 'deck-rule :foreground accent)
    (deck--face 'deck-bar :foreground accent :background panel)))

;; --- markup, drawn on top of `epresent-fontify' ------------------------------
(defconst deck-markup-faces
  '(("dim" . deck-dim) ("faint" . deck-faint) ("accent" . deck-accent)
    ("good" . deck-good) ("bad" . deck-bad) ("warn" . deck-warn)
    ("violet" . deck-violet) ("punch" . deck-punch)
    ("punchbad" . deck-punch-bad) ("punchem" . deck-punch-em)
    ("sub" . deck-sub) ("subtitle" . deck-subtitle) ("pill" . deck-pill)
    ("pillgood" . deck-pill-good) ("pillbad" . deck-pill-bad)
    ("pillon" . deck-pill-on)))

(defun deck--ov (beg end &rest props)
  "An overlay from BEG to END that epresent cleans up with its own."
  (let ((ov (make-overlay beg end)))
    (while props (overlay-put ov (pop props) (pop props)))
    (push ov epresent-overlays)
    ov))

(defun deck--blocks (fn)
  "Call FN with TYPE, BEG and END for the body of every #+begin_ block."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+begin_\\([a-z]+\\)" nil t)
        (let ((type (downcase (match-string 1)))
              (beg (line-beginning-position 2)))
          (when (re-search-forward (format "^[ \t]*#\\+end_%s" type) nil t)
            (funcall fn type beg (line-beginning-position))))))))

(defun deck--fontify (&rest _)
  (save-excursion
    ;; epresent hides #+begin_/#+end_ lines outright, which glues blocks to
    ;; the paragraphs around them.  Keep each one as a half-height gap
    ;; instead; it also lets the first line of a block take its padding.
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+\\(begin\\|end\\)_.*$" nil t)
        (let ((bol (line-beginning-position)) (eol (line-end-position)))
          (dolist (ov (overlays-at bol))
            (when (eq (overlay-get ov 'invisible) 'epresent-hide)
              (delete-overlay ov)
              (setq epresent-overlays (delq ov epresent-overlays))))
          (deck--ov bol eol 'invisible 'epresent-hide)
          (when (< eol (point-max))
            (deck--ov eol (1+ eol) 'face '(:height 0.5))))))
    (deck--blocks
     (lambda (type beg end)
       (when (member type '("src" "example"))
         ;; epresent hides every line that starts with `#', which eats
         ;; Python comments.  Give lines inside code back.
         (dolist (ov (overlays-in beg end))
           (when (and (eq (overlay-get ov 'invisible) 'epresent-hide)
                      (>= (overlay-start ov) beg) (<= (overlay-end ov) end))
             (delete-overlay ov)
             (setq epresent-overlays (delq ov epresent-overlays))))
         (let ((pad (propertize "  " 'face 'org-block)))
           (deck--ov beg end 'line-prefix pad 'wrap-prefix pad)))
       (when (equal type "quote")
         (let ((bar (propertize "▌ " 'face 'deck-bar)))
           (deck--ov beg end 'line-prefix bar 'wrap-prefix bar)))))
    ;; {{{face(text)}}}
    (goto-char (point-min))
    (while (re-search-forward "{{{\\([a-z]+\\)(\\(.*?\\))}}}" nil t)
      (let* ((name (match-string 1))
             (face (cdr (assoc name deck-markup-faces))))
        ;; {{{at(N)}}}: continue at column N, so two lines can share stops
        ;; however wide the pills on them are
        (when (equal name "at")
          (deck--ov (match-beginning 0) (match-end 0) 'display
                    `(space :align-to ,(string-to-number (match-string 2)))))
        ;; pills stand taller than text; give their line some air
        (when (string-prefix-p "pill" name)
          (let ((eol (line-end-position)))
            (when (< eol (point-max))
              (deck--ov eol (1+ eol) 'line-spacing 0.45))))
        (when face
          (deck--ov (match-beginning 0) (match-beginning 2) 'invisible 'epresent-hide)
          (deck--ov (match-end 2) (match-end 0) 'invisible 'epresent-hide)
          (let ((ov (deck--ov (match-beginning 2) (match-end 2)
                              'face face 'priority 20)))
            ;; drawn as a rounded SVG per page, see `deck--render-pills'
            (when (string-prefix-p "pill" name)
              (overlay-put ov 'deck-pill face))))))
    ;; a trailing «face» colours the whole line
    (goto-char (point-min))
    (while (re-search-forward "[ \t]*«\\([a-z]+\\)»[ \t]*$" nil t)
      (let ((face (cdr (assoc (match-string 1) deck-markup-faces))))
        (when face
          (deck--ov (match-beginning 0) (match-end 0) 'invisible 'epresent-hide)
          (deck--ov (line-beginning-position) (match-beginning 0)
                    'face face 'priority 15))))))
(advice-add 'epresent-fontify :after #'deck--fontify)

;; --- pills: rounded, outlined, as in deck.html -------------------------------
;; A face cannot round its corners, so each pill is a small SVG sized from the
;; current font (text scale included) and coloured from the palette.  The
;; face on the overlay stays as a fallback where SVG is unavailable.
(require 'svg)

(defun deck--pill-colours (face)
  "Text, fill and border colours for pill FACE in the current palette."
  (let ((pal (alist-get deck-theme deck-palettes)))
    (cl-flet ((c (k) (plist-get pal k)))
      (pcase face
        ('deck-pill-good (list (c :good) (c :good-bg) (c :good)))
        ('deck-pill-bad  (list (c :bad) (c :bad-bg) (c :bad)))
        ('deck-pill-on   (list (c :accent) (c :accent-bg) (c :accent)))
        (_               (list (c :ink) (c :pill-bg) (c :faint)))))))

(defun deck--pill-image (text face)
  (let* ((k (if (bound-and-true-p text-scale-mode)
                (expt text-scale-mode-step text-scale-mode-amount)
              1.0))
         (cw (* k (frame-char-width)))
         (lh (* k (frame-char-height)))
         (fs (* k (/ (face-attribute 'default :height (selected-frame)) 10.0)))
         (padx (* 0.9 cw))
         (w (ceiling (+ (* (string-width text) cw) (* 2 padx))))
         (h (ceiling (* 1.2 lh)))
         (sw 2)
         (svg (svg-create w h)))
    (pcase-let ((`(,fg ,bg ,line) (deck--pill-colours face)))
      (svg-rectangle svg (/ sw 2.0) (/ sw 2.0) (- w sw) (- h sw)
                     :rx (/ (- h sw) 2.0) :fill bg :stroke line :stroke-width sw)
      (svg-text svg text :x (/ w 2.0) :y (/ h 2.0)
                :text-anchor "middle" :dominant-baseline "central"
                :font-family deck-mono :font-size fs
                :font-weight (if (eq face 'deck-pill) "normal" "600")
                :fill fg))
    (svg-image svg :ascent 'center :scale 1.0)))

(defun deck--render-pills ()
  "Draw this page's pills in the current theme and text size."
  (when (and (display-graphic-p) (image-type-available-p 'svg))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((face (overlay-get ov 'deck-pill)))
        (when face
          (overlay-put ov 'display
                       (deck--pill-image
                        (buffer-substring-no-properties (overlay-start ov)
                                                        (overlay-end ov))
                        face)))))))

(defun deck--latex-at-text-size ()
  "Show LaTeX previews at their rendered size.
They are rendered to match the slide font (`epresent-format-latex-scale');
Emacs's automatic image scaling would enlarge them again by the font width."
  (save-restriction
    (widen)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((spec (overlay-get ov 'display)))
        (when (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
                   (eq (car-safe spec) 'image)
                   (not (equal (plist-get (cdr spec) :scale) 1.0)))
          (overlay-put ov 'display
                       (cons 'image (plist-put (copy-sequence (cdr spec))
                                               :scale 1.0))))))))

;; --- per-page work: builds, rule, scale, images ------------------------------
(defvar deck-show-all (getenv "DECK_ALL"))
(defvar-local deck--steps nil "Hidden builds on this page, in order.")
(defvar-local deck--shown 0 "Builds revealed so far on this page.")
(defvar-local deck--page-ovs nil)
(defvar-local deck--heading-cookie nil)
(defvar-local deck--total 0)

(defun deck--prop (name)
  (org-entry-get (point-min) name))

(defun deck--build-steps ()
  (mapc #'delete-overlay deck--steps)
  (setq deck--steps nil deck--shown 0)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*#\\+pause:.*$" nil t)
        ;; each overlay runs to the end of the page; revealing the first
        ;; one uncovers exactly up to the next pause
        (let ((ov (make-overlay (line-beginning-position) (point-max))))
          (overlay-put ov 'invisible 'deck-step)
          (overlay-put ov 'priority 100)  ; beats an image's `deck-shown'
          (push ov deck--steps)))))
  (setq deck--steps (nreverse deck--steps)))

(defun deck--reveal (n)
  (while (and deck--steps (< deck--shown n))
    (delete-overlay (pop deck--steps))
    (cl-incf deck--shown))
  (force-mode-line-update))

(defun deck--light-twin (file)
  (let ((twin (concat (file-name-sans-extension file) "-light."
                      (file-name-extension file))))
    (and (file-exists-p twin) twin)))

(defun deck--fit-images ()
  "Size this page's images to the window; use light twins in light mode."
  (let ((win (get-buffer-window (current-buffer) t))
        (frac (string-to-number (or (deck--prop "IMG_HEIGHT") "0.6"))))
    (when win
      (let ((mw (round (* 0.96 (window-body-width win t))))
            (mh (round (* frac (window-body-height win t)))))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((spec (overlay-get ov 'display)))
            (when (and (eq (car-safe spec) 'image)
                       (not (overlay-get ov 'deck-pill))
                       (not (eq (overlay-get ov 'org-overlay-type)
                                'org-latex-overlay)))
              (let* ((orig (or (overlay-get ov 'deck-file)
                               (plist-get (cdr spec) :file)))
                     (file (or (and (eq deck-theme 'light)
                                    (deck--light-twin orig))
                               orig)))
                (overlay-put ov 'deck-file orig)
                ;; org hides the link's brackets with `invisible', which
                ;; also hides the image drawn over them; an invisibility
                ;; value outside the spec makes the image visible again
                (overlay-put ov 'invisible 'deck-shown)
                (overlay-put ov 'display
                             (create-image file nil nil
                                           :max-width mw :max-height mh))))))))))

(defun deck--on-page (&rest _)
  (when (derived-mode-p 'epresent-mode)
    (mapc #'delete-overlay deck--page-ovs)
    (setq deck--page-ovs nil)
    (text-scale-set (string-to-number (or (deck--prop "SCALE") "0")))
    (when deck--heading-cookie
      (face-remap-remove-relative deck--heading-cookie)
      (setq deck--heading-cookie nil))
    (let ((h (deck--prop "HEADING")))
      (when h
        (setq deck--heading-cookie
              (face-remap-add-relative 'epresent-heading-face
                                       :height (* 1.55 (string-to-number h))))))
    (unless (deck--prop "NORULE")
      (save-excursion
        (goto-char (point-min))
        (let ((ov (make-overlay (line-end-position) (line-end-position))))
          (overlay-put ov 'after-string
                       (concat "\n" (propertize "━━━" 'face 'deck-rule)))
          (push ov deck--page-ovs))))
    (deck--render-pills)
    (deck--latex-at-text-size)
    (deck--fit-images)
    (deck--build-steps)
    (when deck-show-all (deck--reveal most-positive-fixnum))
    (goto-char (point-min))
    (let ((win (get-buffer-window (current-buffer) t)))
      (when win (set-window-start win (point-min))))))
(advice-add 'epresent-current-page :after #'deck--on-page)

(defun deck--on-resize (frame)
  (dolist (w (window-list frame 'nomini))
    (with-current-buffer (window-buffer w)
      (when (derived-mode-p 'epresent-mode) (deck--fit-images)))))
(add-hook 'window-size-change-functions #'deck--on-resize)

;; --- navigation --------------------------------------------------------------
(defvar deck--last-press 0.0)
(defun deck--too-soon-p ()
  "Non-nil when this press follows the last within 150ms.
Auto-repeat from a held key and a double-firing clicker both look like
this; each suppressed press restarts the window, so holding a key does
not race through the deck."
  (let ((now (float-time)))
    (prog1 (< (- now deck--last-press) 0.15)
      (setq deck--last-press now))))

(defun deck--page-count ()
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((n 0))
        (while (re-search-forward "^\\* " nil t) (cl-incf n))
        n))))

(defun deck-next ()
  "Next build, or the next slide once every build is showing."
  (interactive)
  (unless (deck--too-soon-p)
    (cond (deck--steps (deck--reveal (1+ deck--shown)))
          ((< epresent-page-number deck--total) (epresent-next-page)))))

(defun deck-prev ()
  "Hide the last build, or go back to the previous slide fully built."
  (interactive)
  (unless (deck--too-soon-p)
    (cond ((> deck--shown 0)
           (let ((n (1- deck--shown)))
             (deck--build-steps)
             (deck--reveal n)))
          ((> epresent-page-number 1)
           (epresent-previous-page)
           (deck--reveal most-positive-fixnum)))))

(defun deck-first ()
  "The title slide (epresent's own `t'/`1' go to a table of contents)."
  (interactive)
  (widen)
  (goto-char (point-min))
  (when (re-search-forward "^\\* " nil t) (beginning-of-line))
  (setq epresent-page-number 1)
  (epresent-current-page))

(defun deck-goto (n)
  "Jump to slide N."
  (interactive "nGo to slide: ")
  (deck-first)
  (dotimes (_ (1- (max 1 (min n deck--total))))
    (epresent-next-page)))

(defun deck--titles ()
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let (acc)
        (while (re-search-forward "^\\* \\(.*\\)$" nil t)
          (push (replace-regexp-in-string
                 "{{{[a-z]+(\\(.*?\\))}}}" "\\1" (match-string-no-properties 1))
                acc))
        (nreverse acc)))))

(defun deck-overview ()
  "Pick a slide by title."
  (interactive)
  (let* ((cands (cl-loop for ttl in (deck--titles) for i from 1
                         collect (format "%2d  %s" i ttl)))
         (pick (completing-read "Slide: " cands nil t)))
    (deck-goto (string-to-number pick))))

(defun deck-toggle-theme ()
  "Switch between the dark and light palettes."
  (interactive)
  (deck-apply-theme (if (eq deck-theme 'dark) 'light 'dark))
  (deck--render-latex)
  (deck--latex-at-text-size)
  (deck--render-pills)
  (deck--fit-images))

;; --- sizing ------------------------------------------------------------------
(defvar deck-columns 84 "Characters that must fit across the slide.")
(defvar deck-rows 22 "Lines that must fit down the slide.")

(defun deck--fit-height ()
  "Font height (1/10 pt) that fits the grid on this monitor; sets the border."
  (let* ((geo (alist-get 'geometry (frame-monitor-attributes)))
         (w (nth 2 geo)) (h (nth 3 geo))
         (b (round (* 0.045 w)))
         (px (min (/ (- w (* 2 b)) (* 0.6 deck-columns))
                  (/ (- h (* 2 b)) (* 1.4 deck-rows)))))
    (setq epresent-border-width b)
    (round (* 10 px))))

(defun deck--zoom (k)
  (set-face-attribute 'default epresent--frame :height
                      (round (* k (face-attribute 'default :height epresent--frame))))
  (deck--render-pills))
(defun deck-bigger () "Larger text." (interactive) (deck--zoom 1.08))
(defun deck-smaller () "Smaller text." (interactive) (deck--zoom (/ 1 1.08)))
(defun deck-refit () "Size text to this monitor again." (interactive)
       (set-face-attribute 'default epresent--frame :height (deck--fit-height))
       (deck--render-pills))

(defun deck-help ()
  (interactive)
  (message "SPC → n: next  ·  ← p DEL: back  ·  t: light/dark  ·  o: pick slide  ·  g: go to  ·  1: first  ·  + - 0: size  ·  j k: scroll  ·  q: quit"))

;; --- keys --------------------------------------------------------------------
(dolist (k (list " " "n" "f" "l" [right] [next]))
  (define-key epresent-mode-map k #'deck-next))
(dolist (k (list "p" "b" "h" [left] [prior] [backspace] (kbd "DEL")))
  (define-key epresent-mode-map k #'deck-prev))
(define-key epresent-mode-map "t" #'deck-toggle-theme)
(define-key epresent-mode-map "1" #'deck-first)
(define-key epresent-mode-map "g" #'deck-goto)
(define-key epresent-mode-map "v" #'deck-goto)
(define-key epresent-mode-map "o" #'deck-overview)
(define-key epresent-mode-map "+" #'deck-bigger)
(define-key epresent-mode-map "=" #'deck-bigger)
(define-key epresent-mode-map "-" #'deck-smaller)
(define-key epresent-mode-map "0" #'deck-refit)
(define-key epresent-mode-map "?" #'deck-help)

(setq epresent-mode-line
      '(:eval (let ((s (format "%s%d / %d  " (if deck--steps "·  " "")
                               epresent-page-number deck--total)))
                (concat (propertize " " 'display
                                    `(space :align-to (- right ,(length s))))
                        s))))

;; --- go ----------------------------------------------------------------------
(defun deck--start-hook ()
  (setq deck--total (deck--page-count))
  (add-to-invisibility-spec 'deck-step)
  (visual-line-mode 1)
  (setq-local cursor-type nil)
  (deck-first))
(add-hook 'epresent-start-presentation-hook #'deck--start-hook)

(defun deck-start ()
  "Present deck.org fullscreen in this frame."
  (interactive)
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (blink-cursor-mode -1)
  (deck-apply-theme (if (equal (getenv "DECK_THEME") "light") 'light 'dark))
  (switch-to-buffer (find-file-noselect (expand-file-name "deck.org" deck-dir)))
  (delete-other-windows)
  (let ((h (deck--fit-height)))
    (setq epresent-face-attributes `((default :height ,h :family ,deck-mono))
          ;; LaTeX renders at 10pt; match the slide text
          epresent-format-latex-scale (/ h 100.0)))
  ;; Present in this frame rather than epresent's minibuffer-less one, so
  ;; `o' and `g' can prompt on stage.
  (let ((f (selected-frame)))
    (setq epresent--frame f)
    (modify-frame-parameters
     f `((internal-border-width . ,epresent-border-width)
         (left-fringe . 0) (right-fringe . 0)
         (vertical-scroll-bars . nil) (cursor-type . nil)))
    (unless (frame-parameter f 'fullscreen) (toggle-frame-fullscreen f)))
  (epresent-run))

(unless noninteractive (deck-start))
;;; deck-present.el ends here
