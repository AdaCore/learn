# STAGE: Compile preloader
from gcc as preloader_stage

# Create the preloader
COPY preloader.c .
RUN gcc -shared -o preloader.so -fPIC preloader.c

# STAGE: Install gnat community
FROM ubuntu:latest as gnat_stage

# Install dependencies
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update \
    && apt-get install --no-install-recommends -y \
    build-essential \
    ca-certificates \
    git \
    libdbus-1-3 \
    libfontconfig \
    libx11-xcb-dev \
    make \
    wget \
    && rm -rf /var/lib/apt/lists/*

# Install GNAT Community
RUN git clone https://github.com/AdaCore/gnat_community_install_script.git /gnat_installer/script \
    && wget -q https://community.download.adacore.com/v1/4d99b7b2f212c8efdab2ba8ede474bb9fa15888d?filename=gnat-2020-20200429-x86_64-linux-bin -O /gnat_installer/actual \
    && sh /gnat_installer/script/install_package.sh /gnat_installer/actual /gnat com.adacore.spark2014_discovery,com.adacore.gnat \
    && rm -rf /gnat_installer

# STAGE: final
FROM ubuntu:latest

# Copy artifacts from previous stages
COPY --from=preloader_stage preloader.so /
COPY --from=gnat_stage /gnat /gnat

# Install deps
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update \
    && apt-get install --no-install-recommends -y \
    libc6-dev \
    && rm -rf /var/lib/apt/lists/*

# Create users
# Prevent unprivileged from writing to /tmp
# create app location
RUN useradd -r runner \
    && useradd -r unprivileged \
    && chmod 775 /var/tmp /tmp \
    && mkdir -p /workspace/sessions \
    && chown runner /workspace/sessions

# Update PATH
ENV PATH="/gnat/bin:${PATH}"

# Make unprivileged the default run user
USER unprivileged
