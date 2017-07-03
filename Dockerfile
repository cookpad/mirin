FROM fpco/stack-build:lts-8.18

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/mirin/bin"
ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libmysqlclient-dev

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

RUN mkdir -p /opt/mirin/src
RUN mkdir -p /opt/mirin/bin
WORKDIR /opt/mirin/src

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/mirin/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./package.yaml /opt/mirin/src/package.yaml
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/mirin/src
RUN stack --no-terminal build

RUN stack --no-terminal --local-bin-path /opt/mirin/bin install

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash mirin
RUN chown -R mirin:mirin /opt/mirin
USER mirin
ENV PATH "$PATH:/opt/stack/bin:/opt/mirin/bin"

WORKDIR /opt/mirin

COPY ./settings.yml /opt/mirin/settings.yml

CMD ["/opt/mirin/bin/Mirin"]
