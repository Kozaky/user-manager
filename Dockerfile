FROM haskell:8.10.7 as builder

WORKDIR /user-manager

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./user-manager.cabal /user-manager/user-manager.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Build Application Code
COPY . /user-manager
RUN cabal build
RUN strip /user-manager/dist-newstyle/build/aarch64-linux/ghc-8.10.7/user-manager-0.1.0.0/x/user-manager/build/user-manager/user-manager

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

COPY --from=builder /user-manager/dist-newstyle/build/aarch64-linux/ghc-8.10.7/user-manager-0.1.0.0/x/user-manager/build/user-manager/user-manager ${APP}/user-manager

RUN chown -R $APP_USER:$APP_USER ${APP}

USER $APP_USER
WORKDIR ${APP}

CMD ["./user-manager"]