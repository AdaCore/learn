import json

from celery.utils.log import get_task_logger

from .container import TMPFS

logger = get_task_logger(__name__)


class MQReporter:
    """
    This class is used to report messages over the mq channel in celery. The intention is to use this to report
    intermediate information from a celery task back to the main flask application to update the frontend of the
    status of a task.

    Attributes
    ----------
    lab_ref
        A ref number to associate messages to a lab if needed
    cache
        A dict of cached messages to be saved until new messages with the same tag are received with a newline
    connection
        The connection to the celery messaging system
    publisher
        The publisher which creates the channel with the routing key and will be used to send messages

    Methods
    -------
    stdout(msg)
        Adds a message to the stdout cache to be reported to flask
    stderr(msg)
        Adds a message to the stderr cache to be reported to flask
    console(cmds)
        Reports a cmd list to flask uncached
    lab(success, cases)
        Reports lab data to flask uncached
    """

    def __init__(self, app, task_id, lab_ref=None):
        """
        Constructor for MQReporter class
        :param task_id:
            the task id to associate these messages to
        :param lab_ref:
            if these messages are associated with lab data, this info will be added to the message
        """
        self.lab_ref = lab_ref

        self.cache = {
            "stdout": "",
            "stderr": "",
            "console": ""
        }

        self.string_replace = {
            TMPFS: ""
        }

        self.app = app
        self.task_id = task_id

    def __del__(self):
        """
        Flushes the cache and closes the connections before deconstruction
        """
        for key, value in self.cache.items():
            if value != "":
                self.__send(value, key)

    def __cache_send(self, msg, tag):
        """
        This function is used to aggregate stdout and stderr reports. The aggregation is necessary because the stdout
        and stderr methods are bound directly to the pylxd execute handlers. These handlers get strings directly from
        the process execution and occasionally suffer from timing problems where a single character or a small substring
        of a message gets reported one at a time. This function aggregated these results and caches the messages until
        it sees a newline. It then sends the cached string up to the newline.

        For example, if the cache has 'Hello' and we receive 'World\nSup' the string 'Hello World\n' will be sent and
        'Sup' will be stored in the cache.
        :param msg:
            The msg to cache/send
        :param tag:
            The tag, or type of msg to cache/send
        """
        if msg != "":
            if "\n" in msg:
                # if the msg ends with a \n then we just send the cache and the msg and clear the cache
                if msg.endswith("\n"):
                    fixed_msg = self.cache[tag] + msg
                    self.cache[tag] = ""
                # otherwise we find the newline, send the cache and everything up to the newline and cache the rest
                else:
                    msg_list = msg.splitlines()
                    fixed_msg = self.cache[tag] + "\n".join(msg_list[:-1])
                    logger.debug(f"Caching {msg_list[-1]} to send later")
                    self.cache[tag] = msg_list[-1]
                self.__send(fixed_msg, tag)
            else:
                logger.debug(f"Caching {msg} to send later")
                self.cache[tag] += msg

    def __send(self, msg, tag):
        """
        Format the object we will send over the publisher and send
        :param msg:
            The msg to send
        :param tag:
            The type of msg we are sending
        """
        re_msg = self.__replace_strings(msg)
        obj = {"msg": {
                        "type": tag,
                        "data": re_msg,
                      }
              }
        if self.lab_ref:
            obj["ref"] = self.lab_ref

        obj_str = json.dumps(obj)

        logger.debug(f"Updating status with msg:{obj_str}")
        with self.app.connection_or_acquire() as conn:
            queue = conn.SimpleBuffer(self.task_id)
            queue.put(obj_str)
            queue.close()

    def __replace_strings(self, msg):
        fixed_string = msg
        for key, value in self.string_replace.items():
            fixed_string = fixed_string.replace(key, value)
        return fixed_string

    def stdout(self, msg):
        """
        Cache/send a stdout msg
        :param msg:
            The msg to cache/send
        """
        self.__cache_send(msg, "stdout")

    def stderr(self, msg):
        """
        Cache/send a stderr msg
        :param msg:
            Thre msg to cache/send
        """
        self.__cache_send(msg, "stderr")

    def console(self, cmds):
        """
        Send a console list
        :param cmds:
            The cmd list we want to print
        """
        self.__send(" ".join(cmds), "console")

    def lab(self, success, cases):
        """
        Send a lab msg
        :param success:
            Did the lab succeed
        :param cases:
            The lab case data
        """
        self.__send({"success": success, "cases": cases}, "lab")
