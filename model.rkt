#lang racket/base
(require racket/list
         db)
 
; A blog is a (blog db)
; where db is an sqlite connection
(struct blog (db))
 
; A post is a (post blog id)
; where blog is a blog and id is an integer?
(struct post (blog id))
 
; initialize-blog! : path? -> blog?
; Sets up a blog database (if it doesn't exist)
(define (initialize-app! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define app-users (blog db))
  (unless (table-exists? db "users")
    (query-exec db
     (string-append
      "CREATE TABLE users "
      "(id INTEGER PRIMARY KEY, username TEXT, password TEXT)"))
    (app-add-user!
     app-users "sd" "post")
    (app-add-user!
     app-users "admin" "secret"))

  app-users)
 
; blog-posts : blog -> (listof post?)
; Queries for the post ids
(define (app-users a-blog)
  (define (id->post an-id)
    (post a-blog an-id))
  (map id->post
       (query-list
        (blog-db a-blog)
        "SELECT id FROM users")))
 
; post-title : post -> string?
; Queries for the title
(define (user-username a-post)
  (query-value
   (blog-db (post-blog a-post))
   "SELECT username FROM users WHERE id = ?"
   (post-id a-post)))
 
; post-body : post -> string?
; Queries for the body
(define (user-password p)
  (query-value
   (blog-db (post-blog p))
   "SELECT password FROM users WHERE id = ?"
   (post-id p)))
 
; blog-insert-post!: blog? string? string? -> void
; Consumes a blog and a post, adds the post at the top of the blog.
(define (app-add-user! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO users (username, password) VALUES (?, ?)"
   title body))

 
(provide blog? app-users
         post? user-username user-password 
         initialize-app!
         app-add-user! )