#!/usr/bin/env gxi
(import :std/net/s3)
(import :std/iter)

(def (google)
  (S3Client endpoint: "storage.googleapis.com"
            access-key: "GOOG1EW7G7UYJZFJ7APAPNULRKHUHOFQJ3QGRVPUKRAU466LRGVUYCESSNMA5"
            secret-key: (getenv "GOOGLE_SECRET_KEY")
            region: "us"))

(def (aws)
  (S3Client access-key: "AKIA4LRCGLTARCL24T4C"
            secret-key: (getenv "AWS_SECRET_KEY")))

(def (test client)
  (using (client : S3)
    (for (b (client.list-buckets))
      (displayln b))))

(def (main . args)
  (test (google))
  (test (aws)))
