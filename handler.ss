(import (only-in :std/net/s3
                 S3-get-bucket
                 S3Bucket
                 S3Bucket-exists?
                 S3Client)
        (only-in :std/net/httpd
                 http-request-client
                 http-request-method
                 http-request-headers
                 http-request-path
                 http-response-write)
        (only-in :std/format
                 printf)
        (only-in :std/config
                 config-get!))

(export handle-request handler-init!)

(def (sync-blob bucket origin-url)
  (using (bucket : S3Bucket)
    (unless (bucket.exists? origin-url)
      (printf "Downloading '~a'...\n" origin-url))
    (printf "'~a' is ready!\n" origin-url)))

(def (handler-init! cfg)
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

(def (handle-request req res)
  (log-request req)
  (http-response-write res 200 [] "Hello, World!"))
