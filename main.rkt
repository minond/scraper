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

(struct element (tag children score percentage ref)
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

(define (find-attr name lst)
  (findf (lambda (attr)
           (equal? name (x:attribute-name attr)))
         lst))

(define (read-attr attr [default (void)])
  (if (x:attribute? attr)
      (x:attribute-value attr)
      default))

(define (score-element el)
  (cond
    [(x:element? el)
     (match-define (cons children-score-total children)
       (foldl (lambda (el acc)
                (let* ([scored (score-element el)]
                       [score (element-score scored)])
                  (cons (+ score (car acc))
                        (append (cdr acc) (list scored)))))
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
              children
              parent-score
              0
              el)]
    [else
     (element (object-name el) null 0 0 el)]))

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
       (printf "~a~a (~a ~a%) {\n" padding
               (element-tag elem)
               (element-score elem)
               (element-percentage elem))
       (for ([ch (element-children elem)])
         (show ch (add1 level)))
       (printf "~a}\n" padding)]
      [(x:pcdata? el)
       (printf "~apcdata [~a]\n" padding (element-string el))]
      [else
       (printf "~a~a (0%)\n" padding (object-name el))])))

(struct heading (level text) #:transparent)
(struct paragraph (content) #:transparent)
(struct ordered-list (items) #:transparent)
(struct unordered-list (items) #:transparent)
(struct list-item (content) #:transparent)
(struct text (text) #:transparent)
(struct image (src alt) #:transparent)
(struct link (url) #:transparent)
(struct separator () #:transparent)

(define (extract-content elem)
  (match elem
    [(element 'div children _ _ _)
     (flatten
      (filter identity
              (map extract-content children)))]
    [(element 'ol children _ _ _)
     (ordered-list
      (flatten
       (filter identity
               (map extract-content children))))]
    [(element 'ul children _ _ _)
     (unordered-list
      (filter identity
              (map extract-content children)))]
    [(element 'p _ _ _ el) (paragraph (text (element-string el)))]
    [(element 'li _ _ _ el) (list-item (text (element-string el)))]
    [(element 'h1 _ _ _ el) (heading 1 (text (element-string el)))]
    [(element 'h2 _ _ _ el) (heading 2 (text (element-string el)))]
    [(element 'h3 _ _ _ el) (heading 3 (text (element-string el)))]
    [(element 'h4 _ _ _ el) (heading 4 (text (element-string el)))]
    [(element 'h5 _ _ _ el) (heading 5 (text (element-string el)))]
    [(element 'h6 _ _ _ el) (heading 6 (text (element-string el)))]
    [(element 'hr _ _ _ _) (separator)]
    [(element 'img _ _ _ (x:element _ _ _ attributes _))
     (let* ([src (read-attr (find-attr 'data-src attributes))]
            [alt (read-attr (find-attr 'alt attributes))])
       (image src alt))]
    [(element _ children _ _ (x:element _ _ _ _ _))
     (flatten
      (filter identity
              (map extract-content children)))]
    [else #f]))

(define doc (page-document "https://shopify.engineering/scale-performance-testing"))

(with-output-to-file "ignore.txt" #:exists 'replace
  (lambda ()
    (show (find-article-root doc))))

(void
 (extract-content (find-article-root doc)))

(with-output-to-file "ignore2.txt" #:exists 'replace
  (lambda ()
    (pretty-display
     (extract-content (find-article-root doc)))))
