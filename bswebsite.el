;;;; THESE DEFINITIONS MAY BE OVERIDDEN BY SETUP FILE OR TEMPLATE ;;;;

(setq bswebsite-project-dir nil)

(setf bswebsite-stylesheet nil)

(setq bswebsite-title "untitled")

(setq bswebsite-image-size-default 1000)

(setq bswebsite-image-size-thumb 300)

;; name of the file currently being processed
(setq bswebsite-current-source-file nil)

(defun bswebsite-insert-page-header () nil)

;;;;;;;;;;;;;;;;;;;;;;;;;; WEBSITE BUILDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bswebsite-src-dir ()
  (file-name-concat bswebsite-project-dir "src"))

(defun bswebsite-src-resrc-dir ()
  (file-name-concat bswebsite-project-dir "src" "resrc"))

(defun bswebsite-build-dir ()
  (file-name-concat bswebsite-project-dir "build"))

(defun bswebsite-build-resrc-dir ()
  (file-name-concat bswebsite-project-dir "build" "resrc"))

(defun bswebsite-resize-image (image-url max-dimension &optional filename-suffix)
  "Uses imagemagick to create a resized copy in the build-resrc dir. Optionally
appends a string on to the the file name of the copy.

Returns the url of the new image, relative to the project dir."
  (let* ((src-url (file-name-concat (bswebsite-src-dir) image-url))
         (relative-url (file-name-with-extension
                        (file-name-concat
                         (file-name-directory image-url)
                         (concat (file-name-base image-url) filename-suffix))
                        (file-name-extension image-url)))
         (build-url (file-name-concat (bswebsite-build-dir) relative-url)))

    (shell-command (format "convert %s -resize %sx%s %s"
                           src-url
                           max-dimension
                           max-dimension
                           build-url))
    ;; return relative url of new image
    relative-url))

(defun bswebsite-resize-image-thumbnail (image-url)
  (bswebsite-resize-image image-url bswebsite-image-size-thumb "_thumb"))

(defun bswebsite-resize-image-default (image-url)
  (bswebsite-resize-image image-url bswebsite-image-size-default))

(defun bswebsite-find-section (section-name)
  "Return list of two numbers - start and end point of section."
  (goto-char (point-min))
  (let ((begin-section (search-forward-regexp (concat "^\s*;+\s*BEGIN\s+" section-name) nil t))
        (end-section (search-forward-regexp (concat "^\s*;+\s*END\s+" section-name) nil t))
        (section '()))
    (if (and begin-section end-section)
        (progn
          ;; beginning of last line
          ;; (move-beginning-of-line 1)
          (move-end-of-line 0) ; end of previous line
          (setq end-section (point))
          ;; beginning of next line after start
          (goto-char begin-section)
          (move-beginning-of-line 2)
          (setq begin-section (point))
          ;; return start and end point in a list
          (list begin-section end-section))
      nil)))

(defun bswebsite-get-line-at-point ()
  (let* ((begin-line (progn (move-beginning-of-line 1) (point)))
         (end-line (progn (move-end-of-line 1) (point)))
         (line (string-trim (buffer-substring-no-properties begin-line end-line))))
    line))

(defun bswebsite-strip-comment (line)
  (let ((begin (string-match "[^ ;]+" line))
        (end (string-match "$" line)))
    (string-trim (substring line begin end))))

(defun bswebsite-make-insert-statement (text)
  (cond ((string-match "^\s*<h1>\s*" text)
         (concat "(insert \"<h1>" text "</h1>\n\")"))
        ((string-match "^\s*<h2>\s*" text)
         (concat "(insert \"<h2>" text "</h2>\n\")"))
        (t
         (concat "(insert \"<p>" text "</p>\n\")"))))

(defun bswebsite-get-body-statements (source-file)
  (with-current-buffer (find-file-noselect source-file)
    (let ((section (bswebsite-find-section "BODY"))
          (body-statements '()))
      (when section
        (let ((begin-section (pop section))
              (end-section (pop section))
              (paragraph "")
              (building-paragraph nil)
              (parsing t))

          (goto-char begin-section)

          (while parsing
          
            ;; get next line
            (let ((line (bswebsite-get-line-at-point)))

              (cond
               
               ;; empty line
               ((string-empty-p line)
                (when (not (string-empty-p paragraph))
                  (push (bswebsite-make-insert-statement paragraph) body-statements))
                (setq building-paragraph nil)
                (setq paragraph ""))

               ;; s-expression
               ((string-match "^(" line)
                (move-beginning-of-line 1)
                (let ((begin-sexp (point))
                      (end-sexp (progn (forward-sexp)
                                       (point))))
                  (push (buffer-substring-no-properties begin-sexp end-sexp) body-statements)))
               
               ;; comment line
               ((string-match "^;" line)
                (setq building-paragraph t)
                (setq paragraph (concat paragraph
                                        " "
                                        (bswebsite-strip-comment line))))))

            ;; go to next line
            (if (< (point) end-section)
                (move-beginning-of-line 2)
              (setf parsing nil)))))

      ;; return
      (reverse body-statements))))

(defun bswebsite-insert-head-contents ()
  (insert (format "<title>%s</title>\n" bswebsite-title))
  (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"></link>\n")
  (insert "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>\n"))

(defun bswebsite-build-page (source-file)
  (message "building page: %s" source-file)
  (setf bswebsite-current-source-file source-file)
  (let ((page-src (file-name-concat (bswebsite-src-dir) source-file))
        (page-build (file-name-concat (bswebsite-build-dir)
                                      (file-name-with-extension source-file "html")))
        (section '())
        (body-statements '()))
  
    ;; operate on file without switching buffer
    (with-current-buffer (find-file-noselect page-src)

      ;; eval function definitions
      (setq section (bswebsite-find-section "FUNCTIONS"))
      (when section
        (let ((begin-section (pop section))
              (end-section (pop section)))
          (eval-region begin-section end-section))))

    (setq body-statements (bswebsite-get-body-statements page-src))

    ;; build the page
    (make-empty-file page-build)
    (with-temp-file page-build
      (insert "<html>\n")
      (insert "<head>\n")
      (bswebsite-insert-head-contents)
      (insert "</head>\n")
      (insert "<body>\n")
      (bswebsite-insert-page-header)
      (while body-statements
        (eval (car (read-from-string (pop body-statements)))))
      (insert "</body>\n")
      (insert "</html>\n"))))

(defun bswebsite-load-template (template-dir)
  (let ((template-file (file-name-concat template-dir "template.el")))
    (message "loading template: %s" template-file)
    (load-file template-file)
    (setf bswebsite-stylesheet (file-name-concat template-dir "style.css"))
    (message "template stylesheet: %s" bswebsite-stylesheet)))

(defun bswebsite-build-site (project-dir)
  "project-dir is the path to the website project directory."
  (interactive "DWebsite project dir: ")
  (setf bswebsite-project-dir project-dir)

  ;; (message "project dir: %s\n" bswebsite-project-dir)
  ;; (message "project dir is directory? %s\n" (file-directory-p bswebsite-project-dir))

  (let* (
         ;; (messages-buffer-name (file-name-concat project-dir "build-log"))
         (src-dir (bswebsite-src-dir))
         (build-dir (bswebsite-build-dir))
         (src-resrc (file-name-concat src-dir "resrc"))
         (build-resrc (file-name-concat build-dir "resrc"))
         (setup-file (file-name-concat project-dir "setup.el"))
         (build-style (file-name-concat build-dir "style.css")))

    (message "\nSTARTING WEBSITE BUILD AT %s" (current-time-string))

    ;; (message "build-log: %s" messages-buffer-name)
    
    (message "source dir: %s" src-dir)
    (message "build dir: %s" build-dir)
    (message "resources dir (source): %s" src-resrc)
    (message "resources dir (build): %s" build-resrc)
    (message "setup file: %s" setup-file)

    (message "source dir exists? %s" (file-exists-p src-dir))
    (message "build dir exists? %s" (file-exists-p build-dir))
    (message "resources dir exists (source)? %s" (file-exists-p src-resrc))
    (message "setup file exists? %s" (file-exists-p setup-file))

    ;; let's get started...

    ;; load setup first - setup script loads template
    (when (file-exists-p setup-file)
      (message "loading setup")
      (load-file setup-file))

    ;; delete build dir along with existing contents
    (when (file-exists-p build-dir)
      (message "deleting existing build-dir")
      (delete-directory build-dir t))
    ;; make new build dir
    (make-directory build-dir)
    (make-directory build-resrc)

    ;; copy stylesheet, if it exists
    (if (file-exists-p bswebsite-stylesheet)
        (copy-file bswebsite-stylesheet build-style)
      (message "stylesheet does not exist: %s" bswebsite-stylesheet))

    ;; build pages (every file in src dir with .el extension)
    (let ((src-files (directory-files src-dir nil "^[0-9a-zA-Z\\-\\_]+\\.el$")))
      (while src-files
        (bswebsite-build-page (pop src-files)))))
  (message "WEBSITE BUILD FINISHED\n"))
