from flask import Flask
import logging
import os

import config
from .widget import widget_routes, tasks, celery

logger = logging.getLogger(__name__)

config_dict = {
    "development": "config.settings.DevConfig",
    "production": "config.settings.ProductionConfig",
    "testing": "config.settings.TestConfig"
}


def create_celery():
    """
    Entry point for a celery works
    :return:
        Returns an initialized celery object
    """
    return create(mode='celery')


def create_app():
    """
    Entry point for the flask app
    :return:
        Returns an initialized flask object
    """
    return create(mode='app')


def create(mode='app'):
    """
    The initialization method for the celery and flask apps. Sets config states and registers blueprints
    :param mode:
        The mode (celery, app)
    :return:
        Returns the flask app or celery object depending on the mode
    """
    assert isinstance(mode, str), f'bad mode type "{type(mode)}"'
    assert mode in ('app', 'celery'), f'bad mode "{mode}"'

    app = Flask(__name__, instance_relative_config=False)

    config_name = os.getenv('FLASK_CONFIGURATION', 'development')
    app.config.from_object(config_dict[config_name])

    app.logger.info(f'Starting {mode} in {config_name} environment')
    configure_celery(app, tasks.celery)

    # register blueprints
    app.register_blueprint(widget_routes.widget_bp)

    if mode == 'app':
        return app
    elif mode == 'celery':
        return celery


def configure_celery(app, celery):
    """
    The configuration method for the celery object. Sets celery settings and creates the task objects from decorators
    :param app:
        The flask app to read configuration from
    :param celery:
        The celery object to configure
    """
    app.logger.info('Configuring Celery')

    # set broker url and result backend from app config
    celery.conf.update(app.config)
    celery.conf.update(
        broker_url = os.environ.get('RABBITMQ_BROKER', 'pyamqp://guest@localhost//'),
        result_backend = os.environ.get('RABBITMQ_RESULTS_BACKEND', 'rpc://')
    )

    # subclass task base for app context
    # https://flask.palletsprojects.com/en/1.1.x/patterns/celery/

    class ContextTask(celery.Task):
        def __call__(self, *args, **kwargs):
            with app.app_context():
                return self.run(*args, **kwargs)

    celery.Task = ContextTask

    # run finalize to process decorated tasks
    celery.finalize()
