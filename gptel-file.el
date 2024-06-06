;; -*- lexical-binding: t -*-

(require 'gptel)
(eval-when-compile
  (require 'pp))

(defvar gptel-decode-on-insert t)

(defvar-local gptel-is-decoded nil)
(setf (get 'gptel-is-decoded 'permanent-local) t)

;;;###autoload
(defun gptel-file-on-mode-toggle ()
  (setq gptel-is-decoded gptel-mode))

;;;###autoload
(add-hook 'gptel-mode-hook #'gptel-file-on-mode-toggle)

(defun gptel-enable-after-major-mode-change ()
  "Enable `gptel-mode` after you change major mode/ load a new file.
  called in `after-change-major-mode-hook`"
  (when gptel-is-decoded
    (gptel-mode)))
(add-hook 'after-change-major-mode-hook #'gptel-enable-after-major-mode-change)


;;;###autoload
(defun gptel-file-handler (operation &rest args)
  "Entry for `file-name-handler-alist`.
  OPERATION is the primitive and ARGS arguments."
  (cond ((and (eq operation 'insert-file-contents) gptel-decode-on-insert)
         (apply #'gptel-insert-file-contents-decoded args))
        ((and (eq operation 'write-region) gptel-is-decoded)
         (apply #'gptel-write-region-encoded args))
        (t (apply #'gptel-run-real-handler operation args))))

(defsubst gptel-run-real-handler (operation &rest args)
  "Call the next handler in `file-name-handler-alist` for OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (cons #'gptel-file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defvar-local gptel-major-mode-from-file nil)
(setf (get 'gptel-major-mode-from-file 'permanent-local) t)

(defun gptel-decode-file-object-v1 (obj)
  "decode OBJ and insert into current buffer.
  Called from gptel-insert-file-contents.
  OBJ is returned by `read`."
  (setq gptel-major-mode-from-file
        (plist-get obj :major-mode))
  (mapc
   (lambda (x)
     (let ((content (plist-get x :content)))
       (pcase (plist-get x :role)
         ("system" (setq-local gptel--system-message content))
         ("user" (insert content))
         ("assistant" (insert (propertize content 'gptel 'response))))))
   (plist-get obj :conversation)))

(defun gptel-decode-buffer ()
  (let (obj)
    (condition-case err
        (progn
          (atomic-change-group
            (goto-char (point-min))
            (setq obj (read (current-buffer)))
            (delete-region (point-min) (point-max))
            (let ((version (plist-get obj :version)))
              (cond
               ((version<= version "1.0")
                (gptel-decode-file-object-v1 obj))
               (t (error "Unknown version: %s" version)))))
          (setq gptel-is-decoded t))
      (error (message "invalid gpt file: %S" err))))
  nil)

(defun gptel-encode-buffer ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (let (obj gptel--num-messages-to-send)
        (setq obj
              `(
                :version "1.0"
                :major-mode ,major-mode
                :gptel-model ,gptel-model
                :gptel--backend-name ,(gptel-backend-name gptel-backend)
                :gptel-temperature ,gptel-temperature
                :gptel--system-message ,gptel--system-message
                :gptel-max-tokens ,gptel-max-tokens
                :conversation
                ,(gptel--create-prompt nil t)))
        (let (print-integers-as-characters
              print-length
              print-level
              (print-escape-newlines t)
              (print-escape-control-characters t)
              (print-escape-nonascii t)
              print-escape-multibyte
              (print-quoted t)
              print-gensym
              print-circle
              print-charset-text-property
              print-symbols-bare

              pp-use-max-width
              (pp-escape-newlines t))
          (pp-to-string obj))))))


(defun gptel-insert-file-contents-decoded (filename &optional visit beg end replace)
  "Decode FILENAME to a gptel buffer.
  See `insert-file-contents` for details on VISIT BEG END REPLACE."
  (let (ans)
    ;; FIXME: honor replace == nil
    (atomic-change-group
      (delete-region (point-min) (point-max))
      ;; other file name handlers might call back into here.
      ;; if that happens, we pretend to them we dont exist by
      ;; not decoding
      (let ((gptel-decode-on-insert nil)
            (coding-system-for-read 'utf-8-emacs-unix))
        (setq ans (gptel-run-real-handler #'insert-file-contents filename visit beg end replace)))
      (gptel-decode-buffer))
    ans))

(defun gptel-write-region-encoded (_start _end filename &optional append visit lockname mustbenew)
  "Encode current buffer and then write to FILENAME.
  See `write-region` for details on APPEND VISIT LOCKNAME MUSTBENEW. "
  (when append (error "append not supported"))
  (let ((val (gptel-encode-buffer))
        (gptel-decode-on-insert nil)
        (coding-system-for-write 'utf-8-emacs-unix))
    (gptel-run-real-handler
     #'write-region
     val nil filename
     nil visit lockname mustbenew)))

(add-hook 'after-change-major-mode-hook #'gptel-enable-after-major-mode-change)
;;;###autoload
(defun gptel-load-major-mode ()
  "Load major mode decoded from `gptel-insert-file-contents'."
  (when gptel-major-mode-from-file
    (unwind-protect
        (funcall gptel-major-mode-from-file)
      (setq gptel-major-mode-from-file nil))))

;;;###autoload
(add-to-list
 'file-name-handler-alist
 (cons (rx ".gpt" eos)
       #'gptel-file-handler))

;;;###autoload
(add-to-list
 'auto-mode-alist
 ;; decide major-mode by what is before the .gpt suffix
 (cons (rx ".gpt" eos)
       #'gptel-load-major-mode))


(provide 'gptel-file)
