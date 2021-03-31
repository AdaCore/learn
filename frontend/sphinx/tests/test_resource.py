import unittest

from widget.resource import Resource


class TestResource(unittest.TestCase):
    def test_constructor(self):
        resourceA = Resource("ResourceA", ["A", "B"])
        resourceB = Resource("ResourceB")
        self.assertEqual(resourceA.basename, "ResourceA", msg="basename not stored properly")
        self.assertEqual(resourceB.basename, "ResourceB", msg="basename not stored properly")
        self.assertEqual(resourceA.content, "A\nB", msg="content not parsed correctly")
        self.assertEqual(resourceB.content, "", msg="content not parsed correctly")

    def test_append(self):
        resourceC = Resource("ResourceC", ["A", "B"])
        resourceC.append("C")
        self.assertEqual(resourceC.content, "A\nB\nC", msg="string not appended properly")


if __name__ == '__main__':
    unittest.main()