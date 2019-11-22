# Learn Backend Architecture

## Frameworks

- Flask: server framework
- Celery: task queue
- LXC: Linux containers

---
## Flask

Flask is a lightweight WSGI web application framework.

### Entrypoint

Production: wsgi.py creates the app.
Development: dev.py creates the app and calls run.


### App

The main (only) app in this flask project is widget.

**Entrypoint:** app/__init__.py
**Config:** app/config.py

config.py contains the configurations strings passed to the app context. The init and configuration steps occur in the create methods in app/__init__.py.

#### Blueprints

There is one blueprint registered with the app call widget. Widget is the REST interface for the learn backend API.
When the blueprint is registered with the app, the routes defined in the widget_routes.py are registered with the wsgi application.

#### Routes

- /run_program/
    - this route is called via POST with a JSON object payload. The payload is passed to the celery task queue to spawn a task. When the task is queued, a reference id is returned which defines the task. This task id is passed back to the frontend so the API user can poll the task for completion.
- /check_output/
    - this route is called via POST with a JSON object payload. The payload contains a task id previously obtained via the run_program route. This route is used to poll a running task for status. 
    - this route communicates with the running task via a message queue to receive intermediate task results.
- /download/
    - this route is called via POST with a JSON object describing files that the user would like to zip. The files are inserted into the base project. After the project is computed, the files are zipped and passed back to the user.

## Celery

Celery is an asynchronous task queue/job queue based on distributed message passing. It is focused on real-time operation, but supports scheduling as well.

The execution units, called tasks, are executed concurrently on a single or more worker servers using multiprocessing, Eventlet, or gevent. Tasks can execute asynchronously (in the background) or synchronously (wait until ready).

### Entrypoint

tasks.py is loaded into the celery command line tool. The tasks decorated in tasks.py are registered in the queue and the application waits for incoming tasks.

### Message Queue

The flask app and celery queues communicate with each other over RabbitMQ. Tasks are dispatched from the flask routes and are distributed by the celery engine to registered celery workers. The celery workers communicate back to the flask app via the same message queues.

### Tasks

- run_program
    - This task handles the container and the build/run/prove workflow.
    - Incoming messages contain the source files that should be executed.
    - A project is created and the files are inserted.
    - The data is passed to the container to be executed.
    - Stdout and stderr from the container are passed back to flask via message queues.
    
## LXC

Linux Containers are currently being used to sandbox the build/run/prove workflow for security protection.

We are using the pylxd Python interface to the lxc/lxd tools. 

LXC configuration and provisioning occur via the infrastructure/Makefile and the infrastructure/cloud-config.yml.
