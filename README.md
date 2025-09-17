# S3 Hotlink Cache

A caching reverse-proxy meant for hotlinked assets. Ensures requested files are
stored in a public S3-compatible bucket before responding with a permanent
redirect

## Usage

```sh
# Server Terminal

$ docker run \
    -itp=8080:8080 \
    --env-file=.env \
    "ghcr.io/nafarlee/s3-hotlink-cache:$TAG" \
    --allowed-domains 'example.com' \
    --s3-bucket 'my-bucket' \
    --s3-endpoint 'storage.googleapis.com' \
    --access-key 'MYACCESSKEY' \
    --s3-bucket-region 'us' \
    --secret-key-env 'S3_KEY'
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

## Reference

```sh
s3-hotlink-cache: S3 Hotlink Cache

Usage: s3-hotlink-cache [option ...]

Options:
 -p --port <port>                         The port on which the service will listen [default: 8080]
 -d --allowed-domains <allowed-domains>   The only domain from which the service will accept asset URLs [default: #f]
 -e --s3-endpoint <s3-endpoint>           The endpoint of your S3-compatible object storage (without the protocol) [default: #f]
 -b --s3-bucket <s3-bucket>               The name of the S3 bucket where assets will be stored [default: #f]
 -a --access-key <access-key>             Your S3 access key ID [default: #f]
 -s --secret-key-env <secret-key-env>     The environment variable that stores your S3 secret access key [default: #f]
 -r --s3-bucket-region <s3-bucket-region> The region in which the bucket resides [default: #f]
 -h --help                                display help
```
