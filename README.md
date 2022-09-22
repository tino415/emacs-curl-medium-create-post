# Emacs curl medium article upload

Recently I started to write articles for medium from time to time
and I choosed medium as platform where to write. But I'm also recent
emacs lower and I would prefer to write articles in text editor of my
choice. Luckily I recently discovered that medium have http API to
serve my needs.

## What we are building?

I looked through API and it looks like you can basicali do everithing
on medium using that API. But for start I would like to be able to write
article using markdown localy and then upload it to medium.
You can find specification of this API
[here](https://github.com/Medium/medium-api-docs).

## Integration token

In documentation there are two ways to authenticate api against medium,
1. Self issued access tokens
2. Web based authentication

Since they are mentioning that they are not allowing new integrations
to use Web based authentication, I'm not even triing, so lets go with
self issued access tokens.

Regarding what is said in documentation, I found out that I don't
need to write any email and section `Integration tokens` was already
alloweed in mi settings page. So go to `Profile > Settings > Integration Tokens`
and generate new integration token.

## Token storage

Emacs has mechanism to save secrets in gpg encrypted format.
Package providing this functionality is named
[auth sources](https://www.gnu.org/software/emacs/manual/html_node/auth/index.html).

To save integration token, simply create or add this line to `~/.authinfo.gpg`:

```
machine api.medium.com username <your-medium-username> password <your-integration-token>
```

After you save file, it should by automaticaly encrypted or it will ask which
gpg key to use. I assume GPG is already set up but if not, there are plenty
of good tutorials to do that on the web.

After this we can use following function to decrypt and retrieve password:

```
(auth-source-search :username <user-name> :host "api.medium.com")
```

## Create post API

To create post api, we can use endpoint
`POST https://api.medium.com/v1/users/{{authorId}}/posts`

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

Which can be translated to CURL as:

```
curl --request POST "https://api.medium.com/v1/<username>/posts"\
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
  "publishStatus": "public"
}
EOF;
```

Where attributes are:

- title - title that is showed in post listing
- contentFormat - format in which we are uploading post, either html or markdown
- content - content of our post
- tags - array of tags to use
- publishStatus - if it should by public, enter `public`
