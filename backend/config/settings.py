import os

class BaseConfig():
    FLASK_ENV = 'production'
    TESTING = False
    DEBUG = False
    TASK_TIME_LIMIT = 30

    # Container settings
    CONTAINER_NAME = "safecontainer"

    SECRET_KEY = os.environ.get('FLASK_SECRET_KEY', None)
    LOGLEVEL = "DEBUG"


class DevConfig(BaseConfig):
    FLASK_ENV = 'development'
    DEBUG = True


class ProductionConfig(BaseConfig):
    FLASK_ENV = 'production'
    LOGLEVEL = "INFO"


class TestConfig(BaseConfig):
    FLASK_ENV = 'development'
    TESTING = True
    DEBUG = True
    # make celery execute tasks synchronously in the same process
    CELERY_ALWAYS_EAGER = True
