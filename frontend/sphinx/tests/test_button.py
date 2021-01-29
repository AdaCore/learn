import unittest

from widget.button import Button, button_dictionary


class TestButton(unittest.TestCase):
    def test_constructor(self):
        button = Button("run_button")
        self.assertEqual(button.name, button_dictionary["run_button"]["name"], msg="button name != dictionary lookup")
        self.assertEqual(button.title, button_dictionary["run_button"]["title"], msg="button title != dictionary lookup")
        self.assertEqual(button.mode, button_dictionary["run_button"]["mode"], msg="button mode != dictionary lookup")


if __name__ == '__main__':
    unittest.main()