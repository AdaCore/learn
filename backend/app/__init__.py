from flask import Flask

from .config import Config
from .widget import widget_routes, tasks, celery


def create_celery(debug=False):
    """
    Entry point for a celery works
    :param debug:
        Sets the logging and app state
    :return:
        Returns an initialized celery object
    """
    return create(debug=debug, mode='celery')


def create_app(debug=False):
    """
    Entry point for the flask app
    :param debug:
        Sets the logging and app state
    :return:
        Returns an initialized flask object
    """
    return create(debug=debug, mode='app')


def create(debug=False, mode='app'):
    """
    The initialization method for the celery and flask apps. Sets config states and registers blueprints
    :param debug:
        The logging and app state
    :param mode:
        The mode (celery, app)
    :return:
        Returns the flask app or celery object depending on the mode
    """
    assert isinstance(mode, str), 'bad mode type "{}"'.format(type(mode))
    assert mode in ('app','celery'), 'bad mode "{}"'.format(mode)

    app = Flask(__name__, instance_relative_config=False)
    app.debug = debug

    app.config.from_object(Config)
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
    app.logger.debug('Configuring Celery')
    # set broker url and result backend from app config
    celery.conf.broker_url = app.config['CELERY_BROKER_URL']
    celery.conf.result_backend = app.config['CELERY_RESULT_BACKEND']

    # subclass task base for app context
    # https://flask.palletsprojects.com/en/1.1.x/patterns/celery/

    class ContextTask(celery.Task):
        def __call__(self, *args, **kwargs):
            with app.app_context():
                return self.run(*args, **kwargs)

    celery.Task = ContextTask

    # run finalize to process decorated tasks
    celery.finalize()
