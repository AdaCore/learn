#!/usr/bin/env python
from app import create_celery

# Main entry point for celery worker tasks
celery = create_celery()