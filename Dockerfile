FROM gerbil/gerbilxx@sha256:021aed81ff707494dc0f732413692dff31d88b953f68351d793687d92697d0d3 AS builder
COPY main.ss build.ss gerbil.pkg .
RUN gerbil build --release
FROM alpine
COPY --from=builder /src/.gerbil/bin/main .
ENTRYPOINT ["./main"]
