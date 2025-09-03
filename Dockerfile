FROM gerbil/gerbilxx@sha256:021aed81ff707494dc0f732413692dff31d88b953f68351d793687d92697d0d3 AS builder
COPY main.ss build.ss gerbil.pkg .
RUN gerbil build --release
FROM alpine@sha256:4bcff63911fcb4448bd4fdacec207030997caf25e9bea4045fa6c8c44de311d1
COPY --from=builder /src/.gerbil/bin/main .
EXPOSE 8080
ENTRYPOINT ["./main"]
