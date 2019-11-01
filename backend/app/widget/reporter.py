import json
import logging

from celery.messaging import establish_connection
from kombu.compat import Publisher

logger = logging.getLogger(__name__)


class MQReporter():

    def __init__(self, task_id, lab_ref=None, quiet=False):
        self.lab_ref = lab_ref
        self.quiet = quiet
        self.container_name = container_name

        self.connection = establish_connection()
        self.publisher = Publisher(connection=connection,
                                   exchange="learn",
                                   routing_key=task_id,
                                   exchange_type="direct")

    def __del__(self):
        self.publisher.close()
        self.connection.close()

    def __send(self, msg, tag):
        obj = {"msg": {
                        "type": tag,
                        "data": msg,
                      }
              }
        if self.lab_ref:
            obj["ref"] = self.lab_ref

        obj_str = json.dumps(obj)

        logger.debug("Updating status with msg:{}".format(obj_str))
        self.publisher.send(obj_str)

    def stdout(self, msg):
        self.__send(msg, "stdout")

    def stderr(self, msg):
        self.__send(msg, "stderr")

    def console(self, cmds):
        if not self.quiet:
            self.__send(" ".join(cmds), "console")

    def lab(self, success, cases):
        self.__send({"success": success, "cases": cases}, "lab")
