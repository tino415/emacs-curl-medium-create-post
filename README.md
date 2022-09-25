# Integrate Emacs and medium using cURL

From time to time I write articles and I chose medium as platform where to write. 
But I'm also recent Emacs convert and I would prefer to write articles in text editor of my
choice. Luckily I recently discovered that medium have HTTP API to serve my needs.

## What we are building?

I looked through API and it looks like you can basically do everything
on medium using that API. But for start I would like to be able to write
article using markdown locally and then upload it to medium.
You can find specification of this API [here](https://github.com/Medium/medium-api-docs).

## Integration token

In documentation there are two ways to authenticate API against medium,
1. Self issued access tokens,
2. Web based authentication.

Since they are mentioning that they are not allowing new integration's
to use Web based authentication, I'm not even trying, so lets go with
self issued access tokens.

Regarding what is said in documentation, I found out that I don't
need to write any email and section `Integration tokens` was already
allowed in mi settings page. So go to `Profile > Settings > Integration Tokens`
and generate new integration token.

## Token storage

Emacs has mechanism to save secrets in GPG encrypted format.
Package providing this functionality is named
[auth-sources](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html).

To save integration token, simply create or add this line to `~/.authinfo.gpg`:

```
machine api.medium.com password <your-integration-token>
```

After you save file, it should by automatically encrypted or it will ask which
GPG key to use. I assume GPG is already set up but if not, there are plenty
of good tutorials to do that on the web.

## Create post API

To create post API, we can use endpoint `POST https://api.medium.com/v1/users/{{authorId}}/posts`

In the documentation is this example for that call:

```
POST /v1/users/5303d74c64f66366f00cb9b2a94f3251bf5/posts HTTP/1.1
Host: api.medium.com
Authorization: Bearer 181d415f34379af07b2c11d144dfbe35d
Content-Type: application/json
Accept: application/json
Accept-Charset: utf-8

{
  "title": "Liverpool FC",
  "contentFormat": "html",
  "content": "<h1>Liverpool FC</h1><p>You’ll never walk alone.</p>",
  "canonicalUrl": "http://jamietalbot.com/posts/liverpool-fc",
  "tags": ["football", "sport", "Liverpool"],
  "publishStatus": "public"
}
```

Which can be translated to curl command as:

```
curl --request POST "https://api.medium.com/v1/<user-id>/posts"\
     --header "Authorization: <your-integration-token>\
     --header "Content-Type: application/json"\
     --header "Accept: application/json"\
     --header "Accept-Charset: utf-8"
     --data <EOF
{
  "title": "Liverpool FC",
  "contentFormat": "html",
  "content": "<h1>Liverpool FC</h1><p>You’ll never walk alone.</p>",
  "canonicalUrl": "http://jamietalbot.com/posts/liverpool-fc",
  "tags": ["football", "sport", "Liverpool"],
  "publishStatus": "draft"
}
EOF;
```

Where attributes are:

- title - title that is showed in post listing
- contentFormat - format in which we are uploading post, either HTML or markdown
- content - content of our post
- tags - array of tags to use
- publishStatus - public if we want article to be published or draft
- canonicalUrl - origin URL of post if it was posted first elsewhere

I'm ignoring `canonicalUrl`, since it is optional and don't need it for now.

## Retrieve user id

To be able to call s API we also need to know our user id, I did not find
other way of obtaining besides calling endpoint `https://api.medium.com/v1/me`
You can use this curl command:

```sh
curl -s 'https://api.medium.com/v1/me' --header 'Authorization: Bearer <access-token>'
```

Or, if you want to call it from Emacs:

```elisp
(defun medium-retrieve-user-id ()
  (interactive)
  (let ((access-token (medium-retrieve-secret))
        (command-template
         "curl -s 'https://api.medium.com/v1/me' --header 'Authorization: Bearer %s'"))
    (shell-command (format command-template access-token))))
```

It should return JSON that looks like this:

```
{
  "data": {
    "id": <yout-user-id>,
    "username": "cernakmartin3",
    "name": "Martin Cernak",
    "url": "https://medium.com/@cernakmartin3",
    "imageUrl": ""
  }
}
```

## Emacs interactive command

Emacs has notion of [interactive functions](https://www.emacswiki.org/emacs/InteractiveFunction)
which can be called from Emacs mini-buffer. To create such function we just need 
to call macro interactive in beginning of function definition.
Another think we need is capability to execute HTTP request and return its result,
there are mainly two ways of doing so:

1. use Emacs builtin tooling,
2. use cURL CLI command that should be available everywhere (I'm not sure about windows).

Since I'm trying to use Emacs just as UI, I will go with second one. 
To wrap cURL so we can use it, we could use `shell-command`
which eats command as string and returns result of that command as (guess it)
string. So cURL call to API would look like this:

```elisp
(shell-command-to-string (string-join (list
  "curl --request POST \"https://api.medium.com/v1/<username>/posts\""
  "--header \"Authorization: <your-integration-token>\""
  "--header \"Content-Type: application/json\""
  "--header \"Accept: application/json\""
  "--header \"Accept-Charset: utf-8\""
  "--data {"
  "\"title\": \"Liverpool FC\","
  "\"contentFormat\": \"html\","
  "\"content\": \"<h1>Liverpool FC</h1><p>You’ll never walk alone.</p>\","
  "\"canonicalUrl\": \"http://jamietalbot.com/posts/liverpool-fc\","
  "\"tags\": [\"football\", \"sport\", \"Liverpool\"],"
  "\"publishStatus\": \"draft\""
  "}") "\n"))
```

Notice that I'm using `string-join` to simply make that big string more readable. 

Before publishing, we need to retrieve title and tags for published post,
title is easy, we can simply use interactive command shortcuts to ask 
for information during publishing. Tags would be nicer to have auto-complete
for different tags, but lets not complicate things too much for now.

To enter dynamic data to request, we need to turn it into string template and
then simply format it when doing request. To define constants in elisp, we could
use `defconst`. So our template will look like this:

```elisp
(defconst medium-create-post-curl-template
  (string-join
    (list
      "curl --request POST \"https://api.medium.com/v1/%s/posts\""
      "--header \"Authorization: %s\""
      "--header \"Content-Type: application/json\""
      "--header \"Accept: application/json\""
      "--header \"Accept-Charset: utf-8\""
      "--data %s) "\n"))
```

You could noticed that I replaced data with simple `%s`, mi plan here is to use separate
function to generate request body and use `json-encode` to generate valid JSON body.
Also data content is not wrapped in quotes since we want to use Emacs functions
for quoting. We pass requested JSON as plist to this function:

```elisp
(defun medium-create-post-json (title content tags)
  (json-encode
   (list
    :title title
    :content content
    :tags tags
    :contentFormat "markdown"
    :publishStatus "draft")))
```

Now we can define function that will take this template and format it to resulting
string which we can later feed to `shell-command`. We also need quote resulting
string for use in shell. For that we can use Emacs build in function `shell-quote-argument`.

```elisp
(defun medium-create-post-curl-command (user-id token title content tags)
  (format
   medium-create-post-curl-template
   user-id
   token
   (shell-quote-argument (medium-create-post-json title content tags))))
```

And simply for testing we can define interactive function that will just print
resulting formatted shell command:

```elisp
(defun medium-create-post ()
  (interactive)
  (print (medium-create-post-curl-command "$user" "$token" "$title" "$content" '("$tag1"))))
```

After call to this function, resulting command should be printed in mini-buffer and look
like this:

```elisp
"curl -s 
--request POST \"https://api.medium.com/v1/users/$user/posts\" 
--header \"Authorization: Bearer $token\" 
--header \"Content-Type: application/json\" 
--header \"Accept: application/json\" 
--header \"Accept-Charset: utf-8\" 
--data \\{\\\"title\\\"\\:\\\"\\$title\\\"\\,\\\"content\\\"\\:\\\"\\$content\\\"\\,
\\\"tags\\\"\\:\\[\\\"\\$tag1\\\"\\]\\,\\\"contentFormat\\\"\\:\\\"markdown\\\"\\,
\\\"publishStatus\\\"\\:\\\"draft\\\"\\}"
```

So now we need to edit `medium-create-post` so it will retrieve all data.
Interactive macro can retrieve argument that will describe in pattern how
to ask for data of user. Format is like this `<type-of-data><Prompt>\n`,
for type of data we will use `s` to retrieve raw string and `\n` is simply
delimiter of multiple prompts. To retrieve title and data we use pattern
`sTitle: \nTags: \n`. We also split tags by space so we can later encode
them as JSON array:

```elisp
(defun medium-create-post (title tags)
  (interactive "sTitle: \nsTags: \n")
  (setq tags (split-string tags " "))
  (print (medium-create-post-curl-command "$user-id" "$token" title "$content" tags)))
```

Now calling `medium-create-post` should ask for title and tags and command
printed after that should contain them.

To retrieve access token from auth-sources using function `auth-source-search`. 
This function will return list of `plist` of all matching data we saved in `~/.authinfo.gpg`. 
So we need to first retrieve credentials and then just secret from them using `plist-get`.
Lets create new function to retrieve secret, we will call it `medium-retrieve-secret`.
This function need to get list of matching secrets from auth-sources, then take
secret from first matching and finally, if returned secret is function, call function
or return secret directly:

```elisp
(defun medium-retrieve-secret ()
  (let* ((credentials (auth-source-search :login medium-username :host "api.medium.com"))
         (secret (plist-get (car credentials) :secret)))
    (if (functionp secret) (funcall secret) secret)))
```

Now we can call this function in `medium-create-post` and fill access token to generate
cURL command:

```elisp
(defun medium-create-post (title tags)
  (interactive "sTitle: \nsTags: \n")
  (setq tags (split-string tags " "))
  (let ((access-token (medium-retrieve-secret)))
    (print (medium-create-post-curl-command "$user-id" access-token title "$content" tags))))
```

We also need to be able to get user-id that we retrieved previously using API, 
we can simply define new variable for that. 
This variable is re-writable in `~/.emacs` file.

```elisp
(defvar medium-user-id <user-id-retrieved-using /me api call>)
```

Last thing to do is to retrieve content of current buffer and send it
as content of post. For that we can use function `buffer-string`.
So supposedly final debug function will look like this:

```elisp
(defun medium-create-post (title tags)
  (interactive "sTitle: \nsTags: \n")
  (setq tags (split-string tags " "))
  (let ((access-token (medium-retrieve-secret)))
    (print (medium-create-post-curl-command 
      medium-username access-token title (buffer-string) tags))))
```

Now we can run `shell-command` to actually run this and create our post on medium:

```elisp
(defun medium-create-post (title tags)
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
```

## Complete module

So complete code for this module looks like this, notice that I also added `provide` macro
so it can be placed in folder that Emacs is loading modules from and loaded using `require`
macro:

```elisp
(defvar medium-user-id <medium-user-id>)

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
```

# Afterword

Now when we run `medium-create-post` in markdown buffer, after filling title and tags 
it will create post on medium. If you would like to inspect source code of this
article and module it is about, you can find it on
[github](https://github.com/tino415/emacs-curl-medium-create-post).

Thank you for reading.

