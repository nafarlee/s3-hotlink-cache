(import (only-in :std/net/s3
                 S3Client)
        (only-in :std/net/httpd
                 http-request-method
                 http-request-headers
                 http-request-body
                 http-response-write))

(export handle-request)

(def (make-s3-client)
  (S3Client endpoint: (getenv "S3_ENDPOINT")))

(def (handle-request req res)
  (displayln (http-request-method req) " " (http-request-headers req) " " (http-request-body req))
  (http-response-write res 200 [] "Hello, World!"))
