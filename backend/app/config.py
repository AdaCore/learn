import os

class Config(object):
    #CELERY_BROKER_URL = 'redis://0.0.0.0:6379/0'
    #CELERY_RESULT_BACKEND = 'redis://0.0.0.0:6379/0'

    CELERY_BROKER_URL = 'pyamqp://guest@localhost//'
    CELERY_RESULT_BACKEND = 'rpc://'
    TASK_TIME_LIMIT = 30

