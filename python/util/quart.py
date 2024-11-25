from werkzeug.routing import BaseConverter

class TimestampConverter(BaseConverter):
    regex = r"(?:[0-9]{4}\.[0-9]{2}\.[0-9]{2}-[0-9]{2}\.[0-9]{2}\.[0-9]{2})"
