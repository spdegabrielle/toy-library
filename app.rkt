#lang racket
 
(require web-server/servlet
         web-server/dispatch
         web-server/formlets
         "model.rkt"
         "respond.rkt")

(provide/contract (start (request? . -> . response?)))


(define greetings/hash
  (hash "en" "Hello!"
        "de" "Hallo!"
        "es" "¡Hola!"
        "pt" "Ola!"
        "jp" "こんにちは"
        "ar" "مرحبا"))

;; all available languages
(define languages
  (hash-keys greetings/hash))

;; the number of available language
(define num-languages
  (length languages))

;; -> string?
(define (random-language)
  (list-ref languages
            (random num-languages)))

;; request? -> response?
(define (hello req)
  (define lang (random-language))
  (define greeting (hash-ref greetings/hash lang))
  (set-location (respond/text #:body greeting)
                (url-generator hello+lang lang)))

;; request? string? -> response?
(define (hello+lang req lang)
  (define greeting (hash-ref greetings/hash
                             lang
                             #f))
  (cond ((string? greeting)
         (respond/text #:body greeting))
        (else
         (not-found))))

; (static-files-path "htdocs")
;
; start: request -> doesn't return
; Consumes a request and produces a page that displays
; all of the web content.
(define (start request)
  (render-users-page
   (initialize-app!
    (build-path (current-directory)
                "the-app-data.sqlite"))
   request))


(define-values (dispatcher url-generator)
  (dispatch-rules
   [("users") start]
   [("hello") hello] ;; http://localhost:8000/hello
   [("hello" (string-arg)) hello+lang] ;; http://localhost:8000/hello/en
   [("hello") #:method (regexp ".*") not-allowed]))


; new-post-formlet : formlet (values string? string?)
; A formlet for requesting a title and body of a post
(define new-post-formlet
  (formlet
   (#%#
    
    (label ([for "username"]) "Username:")
    ,{input-string . => . username}
    (label ([for "password"]) "password:")
    ,{(to-string
       (required
        (password-input #:attributes '([id "password"]))))
      . => . password})
   (values username password)))
 
; render-blog-page: blog request -> doesn't return
; Produces an HTML page of the content of the
; blog.
(define (render-users-page an-app request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My App users")
                  (script ([src "/htmx.min.js" ]))

                  
                  (link ((rel "stylesheet")
                         (href "/test-static.css")
                         (type "text/css")))


                  )
            (body
             (h1 "My Blog")
             ,(render-users an-app embed/url)
             (form ([action
                     ,(embed/url insert-post-handler)])

                   (fieldset
                    (legend "add user")
                    ,@(formlet-display new-post-formlet)
                    (br)
                    (input ([type "submit"] [style "float: right;"]))


                    ))))))
 
  (define (insert-post-handler request)
    (define-values (title body)
      (formlet-process new-post-formlet request))
    (app-add-user! an-app title body)
    (render-users-page an-app (redirect/get)))
  (send/suspend/dispatch response-generator))
 
; new-comment-formlet : formlet string
; A formlet for requesting a comment
(define new-comment-formlet
  (to-string (required (textarea-input))))
 
; render-post-detail-page: post request -> doesn't return
; Consumes a post and produces a detail page of the post.
; The user will be able to either insert new comments
; or go back to render-blog-page.
(define (render-post-detail-page a-blog a-post request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Post Details")
                  (link ((rel "stylesheet")
                         (href "/test-static.css")
                         (type "text/css"))))
            (body
             (h1 "User Details")
             (h2 ,(user-username a-post))
             (p ,(user-password a-post))

             (a ([href ,(embed/url back-handler)])
                "Back to the users page")))))
 
 
  (define (back-handler request)
    (render-users-page a-blog request))
  (send/suspend/dispatch response-generator))
 
 
; render-post: post (handler -> string) -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
; The fragment contains a link to show a detailed view of the post.
(define (render-user a-blog a-post embed/url)
  (define (view-post-handler request)
    (render-post-detail-page a-blog a-post request))
  `(li ([class "post"])
       (a ([href ,(embed/url view-post-handler)])
          ,(user-username a-post))
       ;(p ,(user-password a-post))
       ))
 
; render-posts: blog (handler -> string) -> xexpr
; Consumes a embed/url, produces an xexpr fragment
; of all its posts.
(define (render-users a-blog embed/url)
  (define (render-user/embed/url a-post)
    (render-user a-blog a-post embed/url))
  `(ul ([class "posts"])
       ,@(map render-user/embed/url (app-users a-blog))))
 
; render-as-itemized-list: (listof xexpr) -> xexpr
; Consumes a list of items, and produces a rendering as
; an unorderered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))
 
; render-as-item: xexpr -> xexpr
; Consumes an xexpr, and produces a rendering
; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))


(require web-server/servlet-env)


(serve/servlet dispatcher
               #:launch-browser? #t
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths 
               (list "htdocs" (build-path ".." "htdocs"))
               
               #:servlet-regexp #rx""
               #:servlet-path "/users"
               )