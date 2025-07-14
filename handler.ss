(import (only-in :std/net/s3
                 S3-get-bucket
                 S3Bucket
                 S3Bucket-exists?
                 S3Client)
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
                 printf)
        (only-in :std/config
                 config-get!))

(export handle-request handler-init!)

(defrule (if-not-let bindings fbody tbody)
  (if-let bindings tbody fbody))

(defrule (if-not condition fbody tbody)
  (if condition tbody fbody))

(def allowed-origin #f)
(def bucket #f)

(def (sync-blob bucket url)
  (using (bucket : S3Bucket)
    (unless (bucket.exists? url)
      (printf "Downloading '~a'...\n" url))
    (printf "'~a' is ready!\n" url)))

(def (handler-init! cfg)
  (set! allowed-origin (getenv "ALLOWED_ORIGIN"))
  (set! bucket (make-s3-bucket-client))
  (printf "Now listening on ~a...\n"
          (config-get! cfg listen:)))

(def (make-s3-bucket-client)
  (S3-get-bucket
   (S3Client endpoint: (getenv "S3_ENDPOINT"))
   (getenv "S3_BUCKET")))

(def (log-request req)
  (printf "~a - ~a ~a ~a\n"
          (http-request-client req)
          (http-request-method req)
          (http-request-path req)
          (http-request-headers req)))

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

(def (allowed-origin? url)
  (equal? allowed-origin (url-domain url)))

(def (handle-request req res)
  (log-request req)
  (if-not-let (url (origin-url req))
    (http-response-write res 400 [] "A valid 'url' parameter was not provided")
    (if-not (allowed-origin? url)
      (http-response-write res 400 [] "The 'url' parameter does not come from an allowed origin")
      (begin
        (sync-blob bucket url)
        (http-response-write res 200 [] url)))))
