from typing import List

class Resource:
    """A Resource, or file

    Resource represents a file object and has a filename (basename) and
    contents.
    """
    def __init__(self, basename: str, content: List[str] = None):
        """Constructs a Resource

        Args:
            basename (str): The filename to give the Resource
            content (List[str], optional): Initial contents. Defaults to [].
        """
        self.basename = basename
        if content:
            self.__content = content
        else:
            self.__content = []

    def append(self, content: str):
        """Appends a string to the Resource's contents

        Args:
            content (str): The content to append
        """
        self.__content.append(content)

    @property
    def content(self) -> str:
        """Property to get Resource contents

        Returns:
            str: The contents of the Resource as a string
        """
        return "\n".join(self.__content)
