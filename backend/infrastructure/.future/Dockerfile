# our base image
FROM alpine:latest

# Install dependencies
RUN apk add build-base
RUN apk add make
RUN apk add git
RUN apk add sudo

# Create the preloader
ADD preloader.c /root/
RUN gcc -shared -o /preloader.so -fPIC /root/preloader.c

# Create users
RUN adduser --system runner
RUN adduser --system unprivileged

# Allow runner to run unprivileged
RUN echo "runner ALL=(unprivileged) NOPASSWD:ALL" > /etc/sudoers.d/runner

# create app location
RUN mkdir -p /workspace/sessions
RUN chown runner /workspace/sessions

# Install GNAT Community
RUN git clone https://github.com/AdaCore/gnat_community_install_script.git
ADD https://community.download.adacore.com/v1/0cd3e2a668332613b522d9612ffa27ef3eb0815b?filename=gnat-community-2019-20190517-x86_64-linux-bin /root/package/
RUN ["sh", "gnat_community_install_script/install_package.sh", "/root/package", "/gnat", "com.adacore.spark2014_discovery,com.adacore.gnat"]

# Prevent unprivileged from writing to /tmp
RUN chmod 775 /tmp
RUN chmod 775 /var/tmp
RUN chgrp runner /tmp
RUN chgrp runner /var/tmp
RUN ifconfig eth0 down

# Update PATH
ENV PATH="/gnat/bin:${PATH}"
