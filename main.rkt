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

(define (pcdata-string el)
  (let ([str (x:pcdata-string el)])
    (if (equal? str "")
        #f
        str)))

(define (find-attr name lst)
  (findf (lambda (attr)
           (equal? name (x:attribute-name attr)))
         lst))

(define (read-attr attr [default #f])
  (if (x:attribute? attr)
      (x:attribute-value attr)
      default))

(define (attr name lst)
  (read-attr (find-attr name lst)))

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
         [main (find-element 'main doc)]
         [scored (score-element (or main body))])
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
       (printf "~apcdata [~a]\n" padding (or (pcdata-string el) ""))]
      [else
       (printf "~a~a (0%)\n" padding (object-name el))])))

(struct heading (attributes level content) #:transparent)
(struct paragraph (attributes content) #:transparent)
(struct pre (attributes content) #:transparent)
(struct code (attributes content) #:transparent)
(struct bold (attributes content) #:transparent)
(struct italic (attributes content) #:transparent)
(struct blockquote (attributes content) #:transparent)
(struct ordered-list (attributes items) #:transparent)
(struct unordered-list (attributes items) #:transparent)
(struct list-item (attributes content) #:transparent)
(struct text (text) #:transparent)
(struct entity (text) #:transparent)
(struct image (attributes src alt) #:transparent)
(struct video (attributes src) #:transparent)
(struct link (attributes href content) #:transparent)
(struct separator () #:transparent)

(struct id (value) #:transparent)

(define (extract-attributes el)
  (let* ([attributes (x:element-attributes el)]
         [id-value (attr 'id attributes)])
    (filter identity
            (list (and id-value (id id-value))))))

(define (extract-content/list lst)
  (flatten
   (filter identity
           (map extract-content lst))))

(define (extract-content elem)
  (match elem
    [(element 'img _ _ _ el)
     (let* ([attributes (x:element-attributes el)]
            [data (attr 'data-src attributes)]
            [src (attr 'src attributes)]
            [alt (attr 'alt attributes)])
       (image (extract-attributes el)
              (or src data)
              alt))]
    [(element 'video _ _ _ el)
     (let* ([attributes (x:element-attributes el)]
            [src (attr 'src attributes)])
       (video (extract-attributes el) src))]
    [(element 'pcdata _ _ _ el)
     (let ([str (pcdata-string el)])
       (and str (text str)))]
    [(element 'entity _ _ _ el)
     (let ([val (x:entity-text el)])
       (entity val))]
    [(element 'a children _ _ el)
     (let* ([attributes (x:element-attributes el)]
            [href (read-attr (find-attr 'href attributes))]
            [content (extract-content/list children)])
       (link (extract-attributes el) href content))]
    [(element 'hr _ _ _ _)
     (separator)]
    [(element 'div children _ _ _)
     (extract-content/list children)]
    [(element 'ol children _ _ el)
     (ordered-list (extract-attributes el)
                   (extract-content/list children))]
    [(element 'ul children _ _ el)
     (unordered-list (extract-attributes el)
                     (extract-content/list children))]
    [(element 'p children _ _ el)
     (paragraph (extract-attributes el)
                (extract-content/list children))]
    [(element 'pre children _ _ el)
     (pre (extract-attributes el)
          (extract-content/list children))]
    [(element 'code children _ _ el)
     (code (extract-attributes el)
           (extract-content/list children))]
    [(element 'b children _ _ el)
     (bold (extract-attributes el)
           (extract-content/list children))]
    [(element 'i children _ _ el)
     (italic (extract-attributes el)
             (extract-content/list children))]
    [(element 'blockquote children _ _ el)
     (blockquote (extract-attributes el)
                 (extract-content/list children))]
    [(element 'li children _ _ el)
     (list-item (extract-attributes el)
                (extract-content/list children))]
    [(element 'h1 children _ _ el)
     (heading (extract-attributes el)
              1 (extract-content/list children))]
    [(element 'h2 children _ _ el)
     (heading (extract-attributes el)
              2 (extract-content/list children))]
    [(element 'h3 children _ _ el)
     (heading (extract-attributes el)
              3 (extract-content/list children))]
    [(element 'h4 children _ _ el)
     (heading (extract-attributes el)
              4 (extract-content/list children))]
    [(element 'h5 children _ _ el)
     (heading (extract-attributes el)
              5 (extract-content/list children))]
    [(element 'h6 children _ _ el)
     (heading (extract-attributes el)
              6 (extract-content/list children))]
    [(element 'aside _ _ _ _)
     #f]
    [(element _ children _ _ (x:element _ _ _ _ _))
     (extract-content/list children)]
    [else #f]))

; (define doc (page-document "https://shopify.engineering/scale-performance-testing"))
; (define doc (page-document "https://www.quantamagazine.org/physicists-create-a-wormhole-using-a-quantum-computer-20221130/")) ; bad
; (define doc (page-document "https://bytebytego.com/courses/system-design-interview/scale-from-zero-to-millions-of-users")) ; bad, but bad for all extractors
; (define doc (page-document "https://accu.org/journals/overload/30/172/teodorescu/")) ; mostly works but is missing first image attributes and element ids for document href links, maybe tables as well
; (define doc (page-document "https://en.wikipedia.org/wiki/Cardinal_virtues")) ; missing bold, italic elements, should ignore "sidebar" table, need to fix relative links
; (define doc (page-document "https://solarianprogrammer.com/2018/01/12/writing-minimal-x86-64-jit-compiler-cpp-part-2/")) ; mostly working, needs pre/code elements and should also respect whitespace
; (define doc (page-document "https://www.resilience.org/stories/2020-06-08/collapse-of-civilisation-is-the-most-likely-outcome-top-climate-scientists/")) ; including shared buttons at top of page, look into removing, need blockquote elements
(define doc (page-document "https://minond.xyz/posts/adt-type-meaning"))
; (define doc (page-document "https://www.evanmiller.org/statistical-formulas-for-programmers.html"))

(with-output-to-file "ignore.txt" #:exists 'replace
  (lambda ()
    ; (pretty-display doc)
    (show (find-article-root doc))
    ; (show (score-element (find-element 'main doc)))
    ; (show (score-element doc))))
    ))

(void
 (extract-content (find-article-root doc)))

(with-output-to-file "ignore2.txt" #:exists 'replace
  (lambda ()
    (pretty-display
     (extract-content (find-article-root doc)))))
