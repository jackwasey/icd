ARG DOCKER_IMAGE

FROM ${DOCKER_IMAGE}
MAINTAINER "Jack Wasey" jack@jackwasey.com
WORKDIR /usr/local/src

ARG ICD_PROJECT
ARG R_PKG_NAME
ARG GITHUB_URL
ARG GITHUB_USER
ARG GITHUB_REPO
ARG GIT_BRANCH
ARG GIT_URL
ARG R_CMD

COPY in_docker_base.sh .
RUN ./in_docker_base.sh

COPY in_docker_get_icd.sh .
COPY in_docker_build_check.sh .

ENV ASAN_OPTIONS 'abort_on_error=1'
ENV _R_CHECK_DOC_SIZES 'false'
ENV _R_CHECK_CRAN_INCOMING_ 'false'
ENV _R_CHECK_FORCE_SUGGESTS_ 'false'

CMD ./in_docker_build_check.sh

