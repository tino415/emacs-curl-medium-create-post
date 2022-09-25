(defvar medium-user-id nil)

(defconst medium-create-post-curl-template
  (string-join
   (list
    "curl -s"
    "--request POST \"https://api.medium.com/v1/users/%s/posts\""
    "--header \"Authorization: Bearer %s\""
    "--header \"Content-Type: application/json\""
    "--header \"Accept: application/json\""
    "--header \"Accept-Charset: utf-8\""
    "--data %s") " "))

(defun medium-create-post-json (title content tags)
  "Format create post request json body to string"
  (json-encode
   (list
    :title title
    :content content
    :tags tags
    :contentFormat "markdown"
    :publishStatus "draft")))

(defun medium-retrieve-secret ()
  "Retrieve first access token for api.medium.com host from auth-sources"
  (let* ((credentials (auth-source-search :host "api.medium.com"))
         (secret (plist-get (car credentials) :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun medium-retrieve-user-id ()
  "Call medium api with access token to retrieve details about user"
  (interactive)
  (let ((access-token (medium-retrieve-secret))
        (command-template
         "curl -s 'https://api.medium.com/v1/me' --header 'Authorization: Bearer %s'"))
    (shell-command (format command-template access-token))))

(defun medium-create-post-curl-command (user-id token title content tags)
  "Format medium create post cURL command and return as string"
  (format
   medium-create-post-curl-template
   user-id
   token
   (shell-quote-argument (medium-create-post-json title content tags))))
                                            
(defun medium-create-post (title tags)
  "Ask for title and tags and create post on medium from current buffer"
  (interactive "sTitle: \nsTags: \n")
  (setq tags (split-string tags " "))
  (let* ((access-token (medium-retrieve-secret))
         (command
          (medium-create-post-curl-command
           medium-user-id
           access-token
           title
           (buffer-string)
           tags)))
    (shell-command command)))

(provide 'medium)
