(import (only-in :std/net/s3
                 S3-get-bucket
                 S3Bucket
                 S3Bucket-exists?
                 S3Bucket-put!
                 S3Client)
        (only-in :std/cli/getopt
                 call-with-getopt
                 option)
        (only-in :std/srfi/128
                 make-comparator)
        (only-in :std/srfi/113
                 set-empty?
                 set-contains?
                 set-adjoin!
                 set
                 list->set
                 set->list
                 set-difference)
        (only-in :std/misc/hash
                 hash-filter)
        (only-in :std/srfi/19
                 current-date
                 date?
                 date->string)
        (only-in :std/net/address
                 ip4-address->string)
        (only-in :std/net/uri
                 uri-decode)
        (only-in :std/sugar
                 try
                 catch
                 finally
                 let-hash
                 hash
                 when-let)
        (only-in :std/net/httpd
                 start-http-server!
                 make-static-http-mux
                 http-request
                 http-response?
                 http-request-client
                 http-request-method
                 http-request-path
                 http-request-params
                 http-response-write)
        (only-in :std/pregexp
                 pregexp-match
                 pregexp-split)
        (only-in :std/format
                 format
                 printf
                 eprintf)
        (only-in :std/text/json
                 write-json)
        (only-in :std/net/request
                 http-get
                 request-status
                 request-content))

(export main)

(def (main . args)
  (call-with-getopt run args
     program: "s3-hotlink-cache"
     help: "S3 Hotlink Cache"
     (option 'port "-p" "--port"
       help: "The port on which the service will listen"
       default: "8080")
     (option 'allowed-domains "-d" "--allowed-domains"
       help: "The only domain from which the service will accept asset URLs"
       value: (cut string-split <> #\,))
     (option 's3-endpoint "-e" "--s3-endpoint"
       help: "The endpoint of your S3-compatible object storage (without the protocol)")
     (option 's3-bucket "-b" "--s3-bucket"
       help: "The name of the S3 bucket where assets will be stored")
     (option 'access-key "-a" "--access-key"
       help: "Your S3 access key ID")
     (option 'secret-key-env "-s" "--secret-key-env"
       help: "The environment variable that stores your S3 secret access key"
       value: (cut getenv <>))
     (option 's3-bucket-region "-r" "--s3-bucket-region"
       help: "The region in which the bucket resides")))

(def (assert-required-options (opt :~ hash-table?))
  (define cmp
    (make-comparator symbol? symbol=? #f symbol-hash))
  (define REQUIRED_OPTIONS
    (set cmp 'allowed-domains
             's3-endpoint
             's3-bucket
             'access-key
             'secret-key-env
             's3-bucket-region))
  (define present-options
    (list->set cmp (hash-keys (hash-filter opt (lambda (_ v) v)))))
  (define missing-options
    (set-difference REQUIRED_OPTIONS present-options))
  (unless (set-empty? missing-options)
    (error "Missing required command-line options" (set->list missing-options))))

(def (run (opt :~ hash-table?))
  (assert-required-options opt)
  (let* ((ctx
          (init opt))
         (address
          (string-append "0.0.0.0:" (hash-ref ctx 'port)))
         (mux
          (make-static-http-mux (hash ("/" (cut handle-request ctx <> <>)))))
         (httpd
          (start-http-server! address mux: mux)))
    (eprintf "Now listening on ~a...\n" address)
    (thread-join! httpd)))

(def (init (opt :~ hash-table?))
  (hash-merge
    (hash (bucket
           (let-hash opt
             (S3-get-bucket (S3Client endpoint:    .s3-endpoint
                                       access-key: .access-key
                                       secret-key: .secret-key-env
                                       region:     .s3-bucket-region)
                            .s3-bucket)))
          (hit-cache
           (set (make-comparator string? string=? #f string-hash))))
    opt))

(defrule (with-request-log req body ...)
  (let ($now (current-date))
    (try
      body
      ...
      (finally (log-request req $now)))))

(def (handle-request (ctx :~ hash-table?) (req : http-request) (res :~ http-response?))
  (with-request-log req
    (define url (origin-url req))
    (cond
      ((not url)
       (http-response-write res 400 [] "A valid 'url' parameter was not provided"))
      ((not (allowed-domain? ctx url))
       (http-response-write res 400 [] "The 'url' parameter does not come from an allowed domain"))
      ((set-contains? (hash-ref ctx 'hit-cache) url)
       (redirect res (get-cache-address ctx url)))
      (else
       (try
         (sync-blob! ctx url)
         (redirect res (get-cache-address ctx url))
         (catch _ (http-response-write res 400 [] "The blob at 'url' could not be synchronized")))))))

(def (get-cache-address (ctx :~ hash-table?) (origin-url : :string))
  (let-hash ctx
    (format "https://~a/~a/~a" .s3-endpoint .s3-bucket origin-url)))

(def (redirect (res :~ http-response?) (location : :string))
  (http-response-write
   res
   301
   [["Location"       . location]
    `("Cache-Control" . ,(format "public, max-age=~d" (* 60 60 24 7)))]
   #f))

(def (sync-blob! (ctx :~ hash-table?) (url : :string))
  (define bucket (hash-ref ctx 'bucket))
  (using (bucket : S3Bucket)
    (if (bucket.exists? url)
      (set-adjoin! (hash-ref ctx 'hit-cache) url)
      (begin
        (eprintf "Downloading '~a'...\n" url)
        (let ((req (http-get url)))
          (when (<= 200 (request-status req) 299)
            (bucket.put! url (request-content req))
            (set-adjoin! (hash-ref ctx 'hit-cache) url)))))))

(def (date->cfl-string (date :~ date?))
  (date->string (current-date) "~d/~b/~Y:~H:~M:~S ~z"))

(def (http-request-line (req : http-request))
  (let ((path (http-request-path req))
        (params (http-request-params req)))
    (if params
      (format "~a?~a" path params)
      path)))

(def (log-request (req : http-request) (date :~ date?))
  (write-json
   (hash (ip        (ip4-address->string (car (http-request-client req))))
         (timestamp (date->cfl-string date))
         (method    (http-request-method req))
         (params    (uri-decode (http-request-params req)))
         (path      (uri-decode (http-request-path req)))
         (protocol  "HTTP/1.1")))
  (newline))

(def (params->plist (params : :string))
  (pregexp-split "[&=]" params))

(def (origin-url (req : http-request))
  (and-let* ((params (http-request-params req))
             (pl (params->plist params))
             (url (pget "url" pl))
             (decoded-url (uri-decode url)))
    decoded-url))

(def (url-domain (url : :string))
  (when-let (matches (pregexp-match "^https?://([^:/]+)" url))
    (second matches)))

(def (allowed-domain? (ctx :~ hash-table?) (url : :string))
  (member (url-domain url)
          (hash-ref ctx 'allowed-domains)))
