(import (only-in :std/net/s3
                 S3Client)
        (only-in :std/net/httpd
                 http-request-method
                 http-request-headers
                 http-request-body
                 http-response-write))
        (only-in :std/format
                 printf)
        (only-in :std/config
                 config-get!))

(export handle-request handler-init!)

(def (handler-init! cfg)
  (printf "Now listening on ~a...\n"
          (config-get! cfg listen:)))

(def (make-s3-client)
  (S3Client endpoint: (getenv "S3_ENDPOINT")))

(def (handle-request req res)
  (displayln (http-request-method req) " " (http-request-headers req) " " (http-request-body req))
  (http-response-write res 200 [] "Hello, World!"))
