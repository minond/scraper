#lang racket/base

(require racket/function
         racket/list
         racket/string
         racket/match
         racket/pretty
         threading
         json
         request/param
         web-server/servlet
         web-server/servlet-env
         xml/path
         (prefix-in h: html)
         (prefix-in x: xml))

(define (/content req)
  (let* ([url (request-binding-assoc 'url req)]
         [doc (page-document url)])
    (response/output
     #:code 200
     #:headers (list
                (header #"Content-Type" #"application/json"))
     (lambda (op)
       (display (jsexpr->string (hash 'url url)) op)))))

(define-values (app-dispatch app-url)
  (dispatch-rules
   [("content") #:method "post" /content]))

(define (start/servlet)
  (serve/servlet app-dispatch
                 #:launch-browser? #f
                 #:servlet-path "/"
                 #:port 8000
                 #:servlet-regexp #rx""))

(define (request-binding-assoc key req #:default [default #f])
  (cdr (or (assoc key (request-bindings req))
           (cons key default))))

; (start/servlet)

(struct element (tag score percentage children ref)
  #:transparent
  #:mutable)

(define p-worth (make-parameter 10))
(define lista-worth (make-parameter 4))
(define hs-worth (make-parameter 3))
(define article-worth (make-parameter 11))
(define main-worth (make-parameter 2))
(define worths `((,p-worth (p))
                 (,lista-worth (ul ol))
                 (,hs-worth (h1 h2 h3 h4 h5 h6))
                 (,main-worth (main))
                 (,article-worth (article))))

(define (calculate-element-score el)
  (let/cc return
    (unless (x:element? el)
      (return 0))
    (let ([el-tag (x:element-name el)])
      (for ([item worths])
        (let ([worth (car item)]
              [tags (cadr item)])
          (for ([tag tags])
            (when (equal? tag el-tag)
              (return (worth))))))
      0)))

(define (page-document url)
  (let* ([res (get (string->url url))]
         [bod (http-response-body res)]
         [doc (h:read-html-as-xml (open-input-string bod))])
    doc))

(define (element-string elem [acc ""])
  (cond
    [(x:entity? elem) acc]
    [(x:pcdata? elem)
     (string-append acc (x:pcdata-string elem))]
    [else
     (~>> (x:element-content elem)
          (map element-string)
          (apply string-append)
          (string-append acc)
          (regexp-replace* #px"^\\s+|\\s+$" _ ""))]))

(define (score-element el)
  (cond
    [(x:element? el)
     (match-define (cons children-score-total children)
       (foldl (lambda (el acc)
                (let* ([t (score-element el)]
                       [s (element-score t)])
                  (cons (+ s (car acc))
                        (cons t (cdr acc)))))
              (cons 0 null)
              (x:element-content el)))
     (define parent-score
       (+ children-score-total (calculate-element-score el)))
     (unless (zero? parent-score)
       (for ([el children])
         (let ([percentage
                (* (/ (element-score el) parent-score) 100.0)])
           (set-element-percentage! el percentage))))
     (element (x:element-name el)
              parent-score
              0
              children
              el)]
    [else
     (element (object-name el) 0 0 null el)]))

(define (find-elements tag root [acc null])
  (cond
    [(list? root)
     (append (flatten (map (curry find-elements tag) root))
             acc)]
    [(x:element? root)
     (if (eq? tag (x:element-name root))
         (cons root acc)
         (append (flatten (map (curry find-elements tag) (x:element-content root)))
                 acc))]
    [else acc]))

(define (find-element tag root)
  (let ([res (find-elements tag root)])
    (if (not (empty? res))
        (car res)
        #f)))

(define (find-article-root doc)
  (let* ([body (find-element 'body doc)]
         [scored (score-element body)])
    (find-highest-score scored)))

(define (find-highest-score elem)
  (let* ([root
          (if (zero? (element-percentage elem))
              (argmax element-score (element-children elem))
              elem)]
         [next
          (and root
               (findf (lambda (elem)
                        (> (element-percentage elem) 80))
                      (element-children root)))])
    (or (and next (find-highest-score next))
        (or root elem))))

(define (show elem [level 0])
  (let ([el (element-ref elem)]
        [padding (string-join (make-list level " "))])
    (cond
      [(x:element? el)
       (printf "~a~a (~a ~a%) {\n" padding (element-tag elem) (element-score elem) (element-percentage elem))
       (for ([ch (element-children elem)])
         (show ch (add1 level)))
       (printf "~a}\n" padding)]
      [else
       (printf "~a~a (0%)\n" padding (object-name el))])))



(define doc (page-document "https://shopify.engineering/scale-performance-testing"))
(define body (find-element 'body doc))

(show (find-article-root doc))

(with-output-to-file "ignore.txt" #:exists 'replace
  (lambda ()
    (show (find-article-root doc))
    (show (score-element body))))
