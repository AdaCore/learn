#!/bin/sh

trap "exit" INT TERM
trap "kill 0" EXIT

flask run --host=0.0.0.0 &
celery worker -A celery_worker.celery -E --loglevel=DEBUG &

wait
6