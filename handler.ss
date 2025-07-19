(import (only-in :std/net/s3
                 S3-get-bucket
                 S3Bucket
                 S3Bucket-exists?
                 S3Bucket-put!
                 S3Client)
        (only-in :std/srfi/19
                 current-date
                 date->string)
        (only-in :std/net/address
                 ip4-address->string)
        (only-in :std/net/uri
                 uri-decode)
        (only-in :std/sugar
                 if-let
                 when-let)
        (only-in :std/net/httpd
                 http-request-client
                 http-request-method
                 http-request-headers
                 http-request-path
                 http-request-params
                 http-response-write)
        (only-in :std/pregexp
                 pregexp-match
                 pregexp-split)
        (only-in :std/format
                 format
                 printf)
        (only-in :std/net/request
                 http-get
                 request-status
                 request-content)
        (only-in :std/config
                 config-get!))

(export handle-request handler-init!)

(def allowed-domain #f)
(def s3-endpoint #f)
(def bucket-name #f)
(def bucket #f)

(def (handler-init! cfg)
  (set! allowed-domain (getenv "ALLOWED_DOMAIN"))
  (set! s3-endpoint (getenv "S3_ENDPOINT"))
  (set! bucket-name (getenv "S3_BUCKET"))
  (set! bucket (S3-get-bucket (S3Client endpoint: s3-endpoint) bucket-name))
  (printf "Now listening on ~a...\n" (config-get! cfg listen:)))

(def (handle-request req res)
  (log-request req)
  (define url (origin-url req))
  (cond
    ((not url)
     (http-response-write res 400 [] "A valid 'url' parameter was not provided"))
    ((not (allowed-domain? url))
     (http-response-write res 400 [] "The 'url' parameter does not come from an allowed domain"))
    ((sync-blob bucket url)
     => (lambda (location) (http-response-write res 301 [["Location" . location]] #f)))
    (else
     (http-response-write res 400 [] "The blob at 'url' could not be synchronized"))))

(def (get-cache-address origin-url)
  (format "https://~a/~a/~a" s3-endpoint bucket-name origin-url))

(def (sync-blob bucket url)
  (using (bucket : S3Bucket)
    (if (bucket.exists? url)
      (get-cache-address url)
      (begin
        (printf "Downloading '~a'...\n" url)
        (let* ((request (http-get url))
               (status (request-status request))
               (blob (request-content request)))
          (when (<= 200 status 299)
            (bucket.put! url blob)
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
