import os

class BaseConfig():
    FLASK_ENV = 'production'
    TESTING = False
    DEBUG = False
    TASK_TIME_LIMIT = 30

    # Container settings
    CONTAINER_NAME = "safecontainer"

    CELERY_BROKER_URL = os.environ.get('RABBITMQ_BROKER', 'pyamqp://guest@localhost//')
    CELERY_RESULT_BACKEND = os.environ.get('RABBITMQ_RESULTS_BACKEND', 'rpc://')

    SECRET_KEY = os.environ.get('FLASK_SECRET_KEY', None)


class DevConfig(BaseConfig):
    FLASK_ENV = 'development'
    DEBUG = True


class ProductionConfig(BaseConfig):
    FLASK_ENV = 'production'


class TestConfig(BaseConfig):
    FLASK_ENV = 'development'
    TESTING = True
    DEBUG = True
    # make celery execute tasks synchronously in the same process
    CELERY_ALWAYS_EAGER = True
