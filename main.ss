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
                 make-default-comparator)
        (only-in :std/srfi/113
                 set-empty?
                 set
                 list->set
                 set->list
                 set-difference)
        (only-in :std/misc/hash
                 hash-filter)
        (only-in :std/srfi/19
                 current-date
                 date->string)
        (only-in :std/net/address
                 ip4-address->string)
        (only-in :std/net/uri
                 uri-decode)
        (only-in :std/sugar
                 hash
                 when-let)
        (only-in :std/net/httpd
                 start-http-server!
                 make-static-http-mux
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
        (only-in :std/net/request
                 http-get
                 request-status
                 request-content))

(export main)

(def allowed-domain #f)
(def s3-endpoint #f)
(def bucket-name #f)
(def bucket #f)

(def (main . args)
  (call-with-getopt run args
     program: "s3-hotlink-cache"
     help: "S3 Hotlink Cache"
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
       help: "The environment variable that stores your S3 secret access key")
     (option 's3-bucket-region "-r" "--s3-bucket-region"
       help: "The region in which the bucket resides")))

(def (assert-required-options opt)
  (define cmp
    (make-default-comparator))
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

(def (run opt)
  (assert-required-options opt)
  (handler-init!)
  (let* ((address
          "0.0.0.0:8080")
         (mux
          (make-static-http-mux (hash ("/" handle-request))))
         (httpd
          (start-http-server! address mux: mux)))
    (eprintf "Now listening on ~a...\n" address)
    (thread-join! httpd)))

(def (handler-init!)
  (set! allowed-domain (getenv "ALLOWED_DOMAIN"))
  (set! s3-endpoint (getenv "S3_ENDPOINT"))
  (set! bucket-name (getenv "S3_BUCKET"))
  (set! bucket (S3-get-bucket (S3Client endpoint: s3-endpoint) bucket-name)))

(def (handle-request req res)
  (log-request req)
  (define url (origin-url req))
  (cond
    ((not url)
     (http-response-write res 400 [] "A valid 'url' parameter was not provided"))
    ((not (allowed-domain? url))
     (http-response-write res 400 [] "The 'url' parameter does not come from an allowed domain"))
    ((sync-blob bucket url)
     => (lambda (location)
          (http-response-write res
                               301
                               [["Location" . location]
                                `("Cache-Control" . ,(format "public, max-age=~d" (* 60 60 24 7)))]
                               #f)))
    (else
     (http-response-write res 400 [] "The blob at 'url' could not be synchronized"))))

(def (get-cache-address origin-url)
  (format "https://~a/~a/~a" s3-endpoint bucket-name origin-url))

(def (sync-blob bucket url)
  (using (bucket : S3Bucket)
    (if (bucket.exists? url)
      (get-cache-address url)
      (begin
        (eprintf "Downloading '~a'...\n" url)
        (let ((req (http-get url)))
          (when (<= 200 (request-status req) 299)
            (bucket.put! url (request-content req))
            (get-cache-address url)))))))

(def (date->cfl-string date)
  (date->string (current-date) "~d/~b/~Y:~H:~M:~S ~z"))

(def (http-request-line req)
  (let ((path (http-request-path req))
        (params (http-request-params req)))
    (if params
      (format "~a?~a" path params)
      path)))

(def (log-request req)
  (printf "~a ~a ~a [~a] \"~a ~a ~a\" ~a ~a\n"
          (ip4-address->string (car (http-request-client req)))
          "-"
          "-"
          (date->cfl-string (current-date))
          (http-request-method req)
          (uri-decode (http-request-line req))
          "HTTP/1.1"
          "-"
          "-"))

(def (params->plist params)
  (pregexp-split "[&=]" params))

(def (origin-url req)
  (and-let* ((params (http-request-params req))
             (pl (params->plist params))
             (url (pget "url" pl))
             (decoded-url (uri-decode url)))
    decoded-url))

(def (url-domain url)
  (when-let (matches (pregexp-match "^https?://([^:/]+)" url))
    (second matches)))

(def (allowed-domain? url)
  (equal? allowed-domain (url-domain url)))
