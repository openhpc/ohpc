FROM centos:7

ARG TRAVIS_COMMIT_RANGE
ENV TRAVIS_COMMIT_RANGE=$TRAVIS_COMMIT_RANGE
ENV PYTHON_BINARY=python36

RUN yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
RUN yum install -y git ${PYTHON_BINARY}

COPY . /ohpc
WORKDIR /ohpc

RUN ${PYTHON_BINARY} tests/travis/check_spec.py ${TRAVIS_COMMIT_RANGE}
