FROM gapsystem/gap-docker

COPY --chown=1000:1000 ./anthroplogic/gap $HOME

USER gap

WORKDIR $HOME
