# our base image
FROM alpine

# Install dependencies
RUN apk add build-base
RUN apk add make
RUN apk add python3
RUN apk add libfontconfig
RUN apk add libx11-xcb-dev

# Create the preloader
ADD container_payload/preloader.c /root/
RUN gcc -shared -o /preloader.so -fPIC preloader.c

# Install GNAT Community
RUN git clone https://github.com/AdaCore/gnat_community_install_script.git
ADD http://mirrors.cdn.adacore.com/art/5cdffc5409dcd015aaf82626 /root/package/
RUN ["sh", "gnat_community_install_script/install_package.sh", "/root/package", "/gnat"]

# Create users
RUN useradd -ms runner
RUN useradd -ms unprivileged

# Allow runner to run unprivileged
RUN echo "runner ALL=(unprivileged) NOPASSWD:ALL" > /etc/sudoers.d/runner

# Prevent unprivileged from writing to /tmp
RUN chmod 775 /tmp
RUN chmod 775 /var/tmp
RUN chgrp runner /tmp
RUN chgrp runner /var/tmp
RUN mkdir -p /workspace/sessions
RUN chown runner /workspace/sessions

# Update PATH
ENV PATH="/gnat/bin:${PATH}"

# TODO: figure out how to mount tmp from host into container
