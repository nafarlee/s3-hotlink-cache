#!/usr/bin/env gxi
(import :std/net/s3)
(import :std/iter)

(def (google)
  (S3Client endpoint: "storage.googleapis.com"
            access-key: #f
            secret-key: #f
            region: "us"))

(def (aws)
  (S3Client access-key: #f
            secret-key: #f))

(def (test client)
  (using (client : S3)
    (for (b (client.list-buckets))
      (displayln b))))

(def (main . args)
  (test (google))
  (test (aws)))
