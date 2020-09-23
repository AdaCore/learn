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

    MAIL_SERVER = os.environ.get('MAIL_SERVER')
    MAIL_PORT = int(os.environ.get('MAIL_PORT') or 25)
    MAIL_USE_TLS = os.environ.get('MAIL_USE_TLS') is not None
    MAIL_USERNAME = os.environ.get('MAIL_USERNAME')
    MAIL_PASSWORD = os.environ.get('MAIL_PASSWORD')


class DevConfig(BaseConfig):
    FLASK_ENV = 'development'
    DEBUG = True


class ProductionConfig(BaseConfig):
    FLASK_ENV = 'production'
    LOGLEVEL = "WARNING"


class TestConfig(BaseConfig):
    FLASK_ENV = 'development'
    TESTING = True
    DEBUG = True
    # make celery execute tasks synchronously in the same process
    CELERY_ALWAYS_EAGER = True
