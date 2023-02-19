#lang racket/base

(require racket/function
         racket/list
         racket/string
         racket/match
         threading
         json
         request/param
         web-server/servlet
         web-server/servlet-env
         xml/path
         (prefix-in h: html)
         (prefix-in x: xml))

(struct article (sections))
(struct section (children))
(struct heading (text))
(struct paragraph (content))
(struct lista (items))
(struct text (text))
(struct image (src))
(struct link (url))

(define (page-document url)
  (let* ([res (get (string->url url))]
         [bod (http-response-body res)]
         [doc (h:read-html-as-xml (open-input-string bod))])
    ; [doc (h:read-html (open-input-string bod))])
    doc))

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

; (define (filter-elements pred elem [acc null])
;   ; (displayln "=============")
;   ; (displayln (pred elem))
;   (if (pred elem)
;       (cons elem acc)
;       (match elem
;         [(struct h:html-full (_ children))
;          ; (for ([elem children])
;          ;   (displayln elem))
;          (flatten (map (lambda (elem) (filter-elements pred elem acc)) children))]
;         ; (append (flatten (map (lambda (elem)
;         ;                         (filter-elements pred elem acc)) children)) acc)]
;         ; (map (lambda (elem)
;         ;        (filter-elements pred elem acc)) children)]
;
;         ; (for ([elem children])
;         ;   (displayln elem))
;         ; ]
;
;         [(struct h:html-element _) acc]
;         ; [(struct x:pcdata _) acc]
;
;         [else
;          acc])))

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
  (let ([res (find-elements tag elem)])
    (if (not (empty? res))
        (car res)
        #f)))

(define (find-article-root doc)
  (let* ([body (find-element 'body doc)])
    body))

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



(define elem (page-document "https://shopify.engineering/scale-performance-testing"))
(define main (car (find-elements 'main elem)))

; (define title (car (find-elements 'title elem)))
;
; (for ([p (find-elements 'p main)])
;   (displayln (element-string p)))

; (displayln (element-string main))

; title
; (printf "(element-string title) => \"~a\"\n" (element-string title))

; (displayln elem)

; (displayln (filter-elements h:title? elem))
; (define divs (filter-elements h:div? elem))
; (displayln (filter-elements h:article? elem))

; ; (displayln (filter-elements '(head title) elem))
; (let* ([elems (filter-elements '(article) elem)])
;   (for ([elem elems
;
; ; (displayln (cdddr elem))
;
; ; (displayln elem)
; ; (displayln (h:html-full-content (filter-elements h:title? elem)))
