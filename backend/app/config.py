
class Config(object):

    CELERY_BROKER_URL = 'pyamqp://guest@localhost//'
    CELERY_RESULT_BACKEND = 'rpc://'
    TASK_TIME_LIMIT = 30
