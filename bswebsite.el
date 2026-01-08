;;;; THESE DEFINITIONS MAY BE OVERIDDEN BY SETUP FILE OR TEMPLATE ;;;;

;; path to archive database
(setf bswebsite-archive-database-file nil)

(setf bswebsite-project-dir nil)

(setf bswebsite-stylesheet nil)

(setf bswebsite-title "untitled")

(setf bswebsite-image-size-default 1000)

(setf bswebsite-image-size-thumb 300)

;; name of the file currently being processed
(setf bswebsite-current-source-file nil)

;; name of the function currently executing (for error tracking)
(setf bswebsite-function-call-stack '())

(defun bswebsite-insert-page-header () nil)

;;;;;;;;;;;;;;;;;;;;; LOGGING AND ERROR TRACKING ;;;;;;;;;;;;;;;;;;;;;

(setf bswebsite-build-report-buffer "*bswebsite-build-report*")

(defun bswebsite-report-init ()
  "Switches to the build-report buffer and deletes any existing content."
  (switch-to-buffer-other-window bswebsite-build-report-buffer)
  (delete-region (point-min) (point-max)))

(defun bswebsite-report-append (str &rest format-args)
  (save-excursion
    (switch-to-buffer bswebsite-build-report-buffer)
    (goto-char (point-max))
    (insert (format (concat str "\n") format-args))))

(setf bswebsite-build-errors '()) ; list of errors encountered during build

(defun bswebsite-errors-init ()
  "Clears the errors list"
  (setf bswebsite-build-errors '()))

(defun bswebsite-errors-add (str)
  "Add a string to the errors list"
  (push
   (format "%s: %s" bswebsite-function-call-stack str)
   bswebsite-build-errors))

(defmacro bswebsite-with-error-handling (function-name info-str &rest body-statements)
  "Executes contents wrapped inside an error handling block.
If an error is encountered it is logged, including the info-string
passed in as argument."
  `(progn
     (push ,function-name bswebsite-function-call-stack)
     (condition-case err
         (progn
           ,@body-statements)
       ;; catch any error
       (error
        (bswebsite-errors-add (format "%sERROR=%s" ,info-str err))))
     (pop bswebsite-function-call-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;; WEBSITE BUILDER ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bswebsite-src-dir ()
  (file-name-concat bswebsite-project-dir "src"))

(defun bswebsite-src-resrc-dir ()
  (file-name-concat bswebsite-project-dir "src" "resrc"))

(defun bswebsite-build-dir ()
  (file-name-concat bswebsite-project-dir "build"))

(defun bswebsite-build-resrc-dir ()
  (file-name-concat bswebsite-project-dir "build" "resrc"))

(defun bswebsite-get-image-source-url (image)
  "Checks whether image is an archive UID, otherwise gets it from src/resrc."
  ;; regex to match archive document UID in format "UID-___"
  (if (string-match "UID-\\([0-9]+\\)" image)
      (let ((uid (match-string 1 image)))
        ;; get url from archive
        (bsarchive-get-url-for-uid (string-to-number uid)))
    ;; use the given url
    (file-name-concat (bswebsite-src-dir) "resrc" image)))

(defun bswebsite-resize-image (image-url max-dimension &optional filename-suffix)
  "Uses imagemagick to create a resized copy in the build-resrc dir. Optionally
appends a string on to the the file name of the copy.

Returns the url of the new image, relative to the project dir."
  (let ((relative-url "RELATIVE-URL-NOT-FOUND"))
     (let ((src-url (bswebsite-get-image-source-url image-url)))
       (setf relative-url (file-name-with-extension
                           (file-name-concat "resrc"
                                             (concat (file-name-base src-url) filename-suffix))
                           (file-name-extension src-url)))
       (let ((build-url (file-name-concat (bswebsite-build-dir) relative-url)))

         (shell-command (format "convert %s -resize %sx%s %s"
                                src-url
                                max-dimension
                                max-dimension
                                build-url))))

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
          (move-end-of-line 0) ; end of previous line
          (setf end-section (point))
          ;; beginning of next line after start
          (goto-char begin-section)
          (move-beginning-of-line 2)
          (setf begin-section (point))
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
  "Makes an elisp statement which when executed, will insert an html
element containing the input text."
  (cond ((string-match "^\s*<h1>\s*" text)
         (concat "(insert \"" text "</h1>\n\")"))
        ((string-match "^\s*<h2>\s*" text)
         (concat "(insert \"" text "</h2>\n\")"))
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

               ;; empty line (end of paragraph)
               ((string-empty-p line)
                (when (not (string-empty-p paragraph))
                  (push (bswebsite-make-insert-statement paragraph) body-statements))
                (setf building-paragraph nil)
                (setf paragraph ""))

               ;; s-expression
               ((string-match "^(" line)
                (move-beginning-of-line 1)
                (let ((begin-sexp (point))
                      (end-sexp (progn (forward-sexp)
                                       (point))))
                  (push (buffer-substring-no-properties begin-sexp end-sexp) body-statements)))

               ;; comment line
               ((string-match "^;" line)
                (setf building-paragraph t)
                (setf paragraph (concat paragraph
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
   (bswebsite-report-append "building page: %s" source-file)
   (setf bswebsite-current-source-file source-file)
   (let ((page-src (file-name-concat (bswebsite-src-dir) source-file))
         (page-build (file-name-concat (bswebsite-build-dir)
                                       (file-name-with-extension source-file "html")))
         (section '())
         (body-statements '()))

     ;; operate on file without switching buffer
     (with-current-buffer (find-file-noselect page-src)

       ;; eval function definitions
       (setf section (bswebsite-find-section "FUNCTIONS"))
       (when section
         (bswebsite-report-append "loading page functions")
         (let ((begin-section (pop section))
               (end-section (pop section)))
           (eval-region begin-section end-section))))

     (bswebsite-report-append "getting body statements")
     (setf body-statements (bswebsite-get-body-statements page-src))

     ;; build the page
     (bswebsite-report-append "building page: %s" page-build)
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

(defun bswebsite-verify-dir (label dir)
  "Return t if dir exists and is a directory, nil otherwise. Report results via
message."
  (if (file-exists-p dir)
      (if (file-directory-p dir)
          (progn (message "%s: %s" label dir)
                 t)
        (progn (message "ERROR, \"%s\" is not a directory: %s" label dir)
               nil))
    (progn (message "ERROR, \"%s\" does not exist: %s" label dir)
           nil)))

(defun bswebsite-verify-file (label file)
  "Return t if file exists and is a file, nil otherwise. Report results via
message."
  (if (file-exists-p file)
      (if (not (file-directory-p file))
          (progn (message "%s: %s" label file)
                 t)
        (progn (message "ERROR, \"%s\" is a directory, not a file: %s" label file)
               nil))
    (progn (message "ERROR, \"%s\" does not exist: %s" label file)
           nil)))

(defun bswebsite-build-site (project-dir)
  "project-dir is the path to the website project directory."
  (interactive "DWebsite project dir: ")
  (setf bswebsite-project-dir project-dir)
  (bswebsite-report-init)
  (bswebsite-errors-init)
  (bswebsite-report-append (concat "PROJECT DIR: " bswebsite-project-dir))
  (let* ((src-dir (bswebsite-src-dir))
         (build-dir (bswebsite-build-dir))
         (src-resrc (file-name-concat src-dir "resrc"))
         (build-resrc (file-name-concat build-dir "resrc"))
         (setup-file (file-name-concat project-dir "setup.el"))
         (build-style (file-name-concat build-dir "style.css")))

    (bswebsite-report-append "STARTING WEBSITE BUILD AT %s" (current-time-string))

    (when (and (bswebsite-verify-dir "project dir" bswebsite-project-dir)
               (bswebsite-verify-dir "source dir" src-dir)
               (bswebsite-verify-dir "resources dir" src-resrc)
               (bswebsite-verify-file "setup file" setup-file))

      ;; load setup first - setup script loads template
      (bswebsite-report-append "loading setup")
      (load-file setup-file)

      ;; delete build dir along with existing contents
      (when (file-exists-p build-dir)
        (bswebsite-report-append "deleting existing build-dir")
        (delete-directory build-dir t))
      (bswebsite-report-append "making new build dir at: %s" build-dir)
      (make-directory build-dir)
      (make-directory build-resrc)

      ;; copy stylesheet, if it exists
      (if (file-exists-p bswebsite-stylesheet)
          (copy-file bswebsite-stylesheet build-style)
        (bswebsite-report-append "ERROR, stylesheet does not exist: %s" bswebsite-stylesheet))

      ;; build pages (every file in src dir with .el extension)
      (let ((src-files (directory-files src-dir nil "^[0-9a-zA-Z\\-\\_]+\\.el$")))
        (while src-files
          (bswebsite-build-page (pop src-files)))))
    (bswebsite-report-append "WEBSITE BUILD FINISHED")
    (if (not bswebsite-build-errors)
        (bswebsite-report-append "\nNO ERRORS")
      (progn
        (bswebsite-report-append "\nERRORS:")
        (dolist (err-msg (reverse bswebsite-build-errors))
          (bswebsite-report-append err-msg))))))
