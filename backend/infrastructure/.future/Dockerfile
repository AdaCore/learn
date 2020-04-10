# our base image
FROM ubuntu:bionic

# Install dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    build-essential \
    ca-certificates \
    git \
    wget \
    libfontconfig \
    libx11-xcb-dev \
    libdbus-1-3

# Create the preloader
COPY preloader.c /root/
RUN gcc -shared -o /preloader.so -fPIC /root/preloader.c \
    && rm /root/preloader.c

# Create users
# Allow runner to run unprivileged
RUN useradd -r runner \
    && useradd -r unprivileged
#     && echo "runner ALL=(unprivileged) NOPASSWD:ALL" > /etc/sudoers.d/runner

# Prevent unprivileged from writing to /tmp
RUN chmod 775 /var/tmp /tmp

# create app location
RUN mkdir -p /workspace/sessions \
    && chown runner /workspace/sessions

# Install GNAT Community
RUN git clone https://github.com/AdaCore/gnat_community_install_script.git /gnat_installer/script \
    && wget -q https://community.download.adacore.com/v1/0cd3e2a668332613b522d9612ffa27ef3eb0815b?filename=gnat-community-2019-20190517-x86_64-linux-bin -O /gnat_installer/actual \
    && sh /gnat_installer/script/install_package.sh /gnat_installer/actual /gnat com.adacore.spark2014_discovery,com.adacore.gnat \
    && rm -rf /gnat_installer/

# Update PATH
ENV PATH="/gnat/bin:${PATH}"
