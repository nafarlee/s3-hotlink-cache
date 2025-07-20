# S3 Hotlink Cache

A caching reverse-proxy meant for hotlinked assets. Ensures requested files are
stored in a public S3-compatible bucket before responding with a permanent
redirect

## Usage

```sh
# Server Terminal

$ docker run -itp=8080:8080 --env-file=.env "ghcr.io/nafarlee/s3-hotlink-cache:$TAG"
Now listening on (0.0.0.0:8080)...
```

```sh
# Client Terminal

# For the first request, the proxy will:
#   1. Check if the file exists in the bucket, noting that it doesn't
#   2. Download the file, then upload to the public bucket
#   3. Respond with a permanent redirect to the file in the public bucket
$ curl -L localhost:8080 --get --data-urlencode "url=https://example.com/hello-world.txt"
Hello, World!

# For the second request, the proxy will:
#   1. Check if the file exists in the bucket, noting that it does
#   2. Respond with a permanent redirect to the file in the public bucket
$ curl -L localhost:8080 --get --data-urlencode "url=https://example.com/hello-world.txt"
Hello, World!
```

## Configuration

The proxy is configured entirely through environment variables.

| Variable                | Required? | Description                                                              | Example                                                         |
| ---                     | ---       | ---                                                                      | ---                                                             |
| `ALLOWED_DOMAIN`        | Yes       | The only domain from which the service will accept asset URLs            | `example.com`                                                   |
| `S3_ENDPOINT`           | Yes       | The endpoint of your S3-compatible object storage, without the protocol  | `storage.googleapis.com`                                        |
| `S3_BUCKET`             | Yes       | The name of the S3 bucket where assets will be stored                    | `my-bucket`                                                     |
| `AWS_ACCESS_KEY_ID`     | Yes       | Your S3 access key ID                                                    | `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX` |
| `AWS_SECRET_ACCESS_KEY` | Yes       | Your S3 secret access key                                                | `YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY`                      |
| `AWS_DEFAULT_REGION`    | Yes       | The region in which the bucket resides                                   | `us`                                                            |
