import json
import logging

from celery.messaging import establish_connection
from kombu.compat import Publisher

logger = logging.getLogger(__name__)


class MQReporter():

    def __init__(self, task_id, lab_ref=None):
        self.lab_ref = lab_ref

        self.cache = {
            "stdout": "",
            "stderr": "",
            "console": ""
        }

        self.connection = establish_connection()
        self.publisher = Publisher(connection=self.connection,
                                   exchange="learn",
                                   routing_key=task_id,
                                   exchange_type="direct")

    def __del__(self):
        # Flush cache before closing connection
        for key, value in self.cache.items():
            if value != "":
                self.__send(value, key)
        self.publisher.close()
        self.connection.close()

    def __cache_send(self, msg, tag):
        if msg != "":

            if "\n" in msg:
                if msg.endswith("\n"):
                    fixed_msg = self.cache[tag] + msg
                    self.cache[tag] = ""
                else:
                    msg_list = msg.splitlines()
                    fixed_msg = self.cache[tag] + "\n".join(msg_list[:-1])
                    logger.debug("Caching {} to send later".format(msg_list[-1]))
                    self.cache[tag] = msg_list[-1]
                self.__send(fixed_msg, tag)
            else:
                logger.debug("Caching {} to send later".format(msg))
                self.cache[tag] += msg

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
        self.__cache_send(msg, "stdout")

    def stderr(self, msg):
        self.__cache_send(msg, "stderr")

    def console(self, cmds):
        self.__send(" ".join(cmds), "console")

    def lab(self, success, cases):
        self.__send({"success": success, "cases": cases}, "lab")
