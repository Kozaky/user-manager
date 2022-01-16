FROM haskell:8.10.7 as builder

WORKDIR /example-servant-persistent

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./example-servant-persistent.cabal /example-servant-persistent/example-servant-persistent.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Build Application Code
COPY . /example-servant-persistent
RUN cabal build

FROM debian:buster-slim
ARG APP=/usr/src/app

RUN apt-get update \
    && apt-get install -y ca-certificates tzdata \
    && apt-get install -y libnuma-dev \
    && apt-get install -y netbase \
    && rm -rf /var/lib/apt/lists/*


ENV TZ=Etc/UTC \
    APP_USER=appuser

EXPOSE 3000

RUN groupadd $APP_USER \
    && useradd -g $APP_USER $APP_USER \
    && mkdir -p ${APP}

COPY --from=builder /example-servant-persistent/dist-newstyle/build/aarch64-linux/ghc-8.10.7/example-servant-persistent-0.1.0.0/x/example-servant-persistent/build/example-servant-persistent/example-servant-persistent ${APP}/example-servant-persistent

RUN chown -R $APP_USER:$APP_USER ${APP}

USER $APP_USER
WORKDIR ${APP}

CMD ["./example-servant-persistent"]