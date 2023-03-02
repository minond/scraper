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
         (prefix-in x: xml)
         (prefix-in : scribble/html/xml)
         (prefix-in : scribble/html/html))

(define (/content req)
  (let* ([url (request-binding-assoc 'url req)]
         [doc (download url)])
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

(define (download url)
  (let* ([res (get url)]
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

(define (attr name lst #:default [default #f])
  (read-attr (find-attr name lst) default))

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
  (find-highest-score/list
   (filter identity
           (list
            (find-element 'body doc)
            (find-element 'main doc)))))

(define (find-highest-score/list lst)
  (let* ([scored (map score-element lst)]
         [highs (map find-highest-score scored)])
    (argmax element-score highs)))

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
(struct superscript (attributes content) #:transparent)
(struct ordered-list (attributes items) #:transparent)
(struct unordered-list (attributes items) #:transparent)
(struct list-item (attributes content) #:transparent)
(struct text (text) #:transparent)
(struct entity (id) #:transparent)
(struct image (attributes src alt) #:transparent)
(struct video (attributes src) #:transparent)
(struct link (attributes href content) #:transparent)
(struct separator () #:transparent)

(struct id (value) #:transparent)

(define ignorable-tags
  '(aside header nav script style))
(define (ignorable-element? elem)
  (let* ([el (element-ref elem)]
         [attributes (if (x:element? el) (x:element-attributes el) empty)]
         [class (attr 'class attributes #:default "")])
    (or
     (string-contains? class "nomobile")
     (string-contains? class "sidebar")
     (string-contains? class "noprint")
     (string-contains? class "navbar")
     (string-contains? class "dpsp-networks-btns-share") ; WP plugin
     (string-contains? class "owl-carousel") ; WP plugin
     (string-contains? class "mw-editsection") ; Wikimedia edit links
     (equal? "navigation" (attr 'role attributes #:default "")))))

(define (absolute-url base-url relative-url)
  (url->string (combine-url/relative base-url relative-url)))

(define (extract-attributes el)
  (let* ([attributes (x:element-attributes el)]
         [id-value (attr 'id attributes)])
    (filter identity
            (list (and id-value (id id-value))))))

(define (element-content/list lst base-url)
  (flatten
   (filter identity
           (map (lambda~>
                 (element-content base-url)) lst))))

(define (element-content elem base-url)
  (if (ignorable-element? elem)
      #f
      (match elem
        [(element 'img _ _ _ el)
         (let* ([attributes (x:element-attributes el)]
                [data (attr 'data-src attributes)]
                [src (attr 'src attributes)]
                [alt (attr 'alt attributes)])
           (image (extract-attributes el)
                  (or (and src (absolute-url base-url src)) data)
                  alt))]
        [(element 'video _ _ _ el)
         (let* ([attributes (x:element-attributes el)]
                [src (attr 'src attributes)])
           (video (extract-attributes el)
                  (and src (absolute-url base-url src))))]
        [(element 'pcdata _ _ _ el)
         (let ([str (pcdata-string el)])
           (and str (text str)))]
        [(element 'entity _ _ _ el)
         (let ([id (x:entity-text el)])
           (entity id))]
        [(element 'a children _ _ el)
         (let* ([attributes (x:element-attributes el)]
                [href (read-attr (find-attr 'href attributes))]
                [content (element-content/list children base-url)])
           (link (extract-attributes el)
                 (and href (absolute-url base-url href))
                 content))]
        [(element 'hr _ _ _ _)
         (separator)]
        [(element 'ol children _ _ el)
         (ordered-list (extract-attributes el)
                       (element-content/list children base-url))]
        [(element 'ul children _ _ el)
         (unordered-list (extract-attributes el)
                         (element-content/list children base-url))]
        [(element 'p children _ _ el)
         (paragraph (extract-attributes el)
                    (element-content/list children base-url))]
        [(element 'pre children _ _ el)
         (pre (extract-attributes el)
              (element-content/list children base-url))]
        [(element 'code children _ _ el)
         (code (extract-attributes el)
               (element-content/list children base-url))]
        [(element (? (lambda~> (member '(b strong)))) children _ _ el)
         (bold (extract-attributes el)
               (element-content/list children base-url))]
        [(element 'i children _ _ el)
         (italic (extract-attributes el)
                 (element-content/list children base-url))]
        [(element 'blockquote children _ _ el)
         (blockquote (extract-attributes el)
                     (element-content/list children base-url))]
        [(element 'sup children _ _ el)
         (superscript (extract-attributes el)
                      (element-content/list children base-url))]
        [(element 'li children _ _ el)
         (list-item (extract-attributes el)
                    (element-content/list children base-url))]
        [(element 'h1 children _ _ el)
         (heading (extract-attributes el)
                  1 (element-content/list children base-url))]
        [(element 'h2 children _ _ el)
         (heading (extract-attributes el)
                  2 (element-content/list children base-url))]
        [(element 'h3 children _ _ el)
         (heading (extract-attributes el)
                  3 (element-content/list children base-url))]
        [(element 'h4 children _ _ el)
         (heading (extract-attributes el)
                  4 (element-content/list children base-url))]
        [(element 'h5 children _ _ el)
         (heading (extract-attributes el)
                  5 (element-content/list children base-url))]
        [(element 'h6 children _ _ el)
         (heading (extract-attributes el)
                  6 (element-content/list children base-url))]
        [(element tag children _ _ (x:element _ _ _ _ _))
         (and (not (member tag ignorable-tags))
              (element-content/list children base-url))]
        [else #f])))

(define (extract-content doc url)
  (element-content (find-article-root doc) url))

(define (render-page elem-or-lst)
  (:xml->string
   (:html
    (:head
     (:meta 'charset: 'utf-8)
     (:script
      'type: "text/javascript"
      'src: "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
     (:script "
       MathJax.Hub.Config({
         TeX: { equationNumbers: { autoNumber: 'AMS' } },
         CommonHTML: { linebreaks: { automatic: true } },
         'HTML-CSS': { linebreaks: { automatic: true } },
         SVG: { linebreaks: { automatic: true } }
       });")
     (:style "
      html,
      body {
        width: 100%;
        margin: 0;
      }
      img,
      video {
        max-width: 75%;
      }
      main {
        max-width: 90%;
        margin: 0 auto;
        padding: 1em;
      }
     "))
    (:body
     (:element 'main
               (render-content elem-or-lst))))))

(define headings-by-level
  (hash
   1 :h1
   2 :h2
   3 :h3
   4 :h4
   5 :h5
   6 :h6))

(define (attributes-arguments attributes)
  (let ([id (findf id? attributes)])
    (if id `('id: ,(id-value id)) empty)))

(define (render-element tagf attributes content)
  (eval `(,tagf ,@(attributes-arguments attributes)
                ',(render-content content))))

(define (render-content elem-or-lst)
  (if (list? elem-or-lst)
      (for/list ([elem elem-or-lst])
        (render-content elem))
      (match elem-or-lst
        [(heading attributes level content)
         ((hash-ref headings-by-level level)
          (render-content content))]
        [(paragraph attributes content)
         (render-element :p attributes content)]
        [(link attributes href content)
         (eval `(:a ,@(attributes-arguments attributes)
                    'href: ,href
                    ',(render-content content)))]
        [(bold attributes content)
         (render-element :b attributes content)]
        [(italic attributes content)
         (render-element :i attributes content)]
        [(code attributes content)
         (render-element :code attributes content)]
        [(ordered-list attributes content)
         (render-element :ol attributes content)]
        [(unordered-list attributes content)
         (render-element :ul attributes content)]
        [(list-item attributes content)
         (render-element :li attributes content)]
        [(blockquote attributes content)
         (render-element :blockquote attributes content)]
        [(superscript attributes content)
         (render-element :sup attributes content)]
        [(pre attributes content)
         (render-element :pre attributes content)]
        [(separator)
         (:hr)]
        [(video attributes src)
         (eval `(:element 'video
                          ,@(attributes-arguments attributes)
                          'src: ,src
                          'autoplay: "autoplay"
                          'muted: "true"
                          'loop: "true"
                          '(#f)))]
        [(image attributes src alt)
         (eval `(:img ,@(attributes-arguments attributes)
                      'src: ,src
                      'alt: ,alt))]
        [(entity id)
         (:entity id)]
        [(text text)
         text]
        [else
         ; (printf "[error] unimplemented ~a\n" elem-or-lst)
         ""])))

; (define url (string->url "https://shopify.engineering/scale-performance-testing"))
; (define url (string->url "https://www.quantamagazine.org/physicists-create-a-wormhole-using-a-quantum-computer-20221130/")) ; bad, displaying javascript code
; (define url (string->url "https://bytebytego.com/courses/system-design-interview/scale-from-zero-to-millions-of-users")) ; bad, but bad for all extractors
; (define url (string->url "https://accu.org/journals/overload/30/172/teodorescu/")) ; mostly works but is missing first image attributes and element ids for document href links, maybe tables as well
; (define url (string->url "https://en.wikipedia.org/wiki/Cardinal_virtues")) ; missing bold, italic elements, should ignore "sidebar" table, need to fix relative links, odd thing happening with references list items going outside of the ol element
; (define url (string->url "https://solarianprogrammer.com/2018/01/12/writing-minimal-x86-64-jit-compiler-cpp-part-2/")) ; mostly working, needs pre/code elements and should also respect whitespace
; (define url (string->url "https://www.resilience.org/stories/2020-06-08/collapse-of-civilisation-is-the-most-likely-outcome-top-climate-scientists/")) ; including shared buttons at top of page, look into removing, need blockquote elements
; (define url (string->url "https://minond.xyz/posts/adt-type-meaning"))
(define url (string->url "https://www.evanmiller.org/statistical-formulas-for-programmers.html"))

(define doc (download url))

(with-output-to-file "ignore.txt" #:exists 'replace
  (lambda ()
    ; (pretty-display doc)
    (show (find-article-root doc))
    ; (show (score-element (find-element 'main doc)))
    ; (show (score-element doc))))
    ))

(void
 (extract-content doc url))

(with-output-to-file "ignore2.txt" #:exists 'replace
  (lambda ()
    (pretty-display
     (extract-content doc url))))

(with-output-to-file "ignore3.html" #:exists 'replace
  (lambda ()
    (display
     (render-page
      (extract-content doc url)))))
