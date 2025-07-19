FROM gerbil/gerbilxx@sha256:021aed81ff707494dc0f732413692dff31d88b953f68351d793687d92697d0d3
COPY handler.ss build.ss gerbil.pkg .
RUN gerbil build \
      --optimized
RUN gerbil httpd \
      --gerbil-path .gerbil \
      config \
      --handlers '(("/" . :s3-hotlink-cache/handler))'
ENTRYPOINT ["gerbil", "httpd", "--gerbil-path", ".gerbil", "server"]
