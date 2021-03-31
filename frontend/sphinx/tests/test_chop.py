import unittest

from widget.chop import manual_chop, cheapo_gnatchop, real_gnatchop
from widget.resource import Resource


class TestManual_Chop(unittest.TestCase):
    input = """Garbage
More garbage
!fake_file.txt
blahblah


nothing

!test.c
test.c

test.c

!test.h
test.h

test.h"""

    def test_manual_chop(self):
        split = self.input.splitlines()
        inTest = manual_chop(split)
        self.assertEqual(len(inTest), 2, msg="c chop returned more/less than 2 resources")
        self.assertEqual(inTest[0].basename, "test.c", msg="basename parsing failed")
        self.assertEqual(inTest[1].basename, "test.h", msg="basename parsing failed")
        self.assertEqual(inTest[0].content, "\n".join(split[9:13]), msg=f"content parsing failed for {inTest[0].basename}")
        self.assertEqual(inTest[1].content, "\n".join(split[14:]), msg=f"content parsing failed for {inTest[1].basename}")


class TestCheapo_Gnatchop(unittest.TestCase):
    input = """Garbage
More garbage
!fake_file.txt
blahblah


nothing

package body A is
   A Body

   A Body
end A;

package A is
    A Spec

    A Spec
end A;

procedure Main is
   main procedure
end Main;
"""

    def test_cheapo_gnatchop(self):
        split = self.input.splitlines()
        inTest = cheapo_gnatchop(split)
        self.assertEqual(len(inTest), 3, msg="cheapo chop returned more/less than 3 resources")
        self.assertEqual(inTest[0].basename, "a.adb", msg="basename parsing failed")
        self.assertEqual(inTest[1].basename, "a.ads", msg="basename parsing failed")
        self.assertEqual(inTest[2].basename, "main.adb", msg="basename parsing failed")
        self.assertEqual(inTest[0].content, "\n".join(split[8:14]), msg=f"content parsing failed for {inTest[0].basename}")
        self.assertEqual(inTest[1].content, "\n".join(split[14:20]), msg=f"content parsing failed for {inTest[1].basename}")
        self.assertEqual(inTest[2].content, "\n".join(split[20:]), msg=f"content parsing failed for {inTest[2].basename}")


class TestReal_Gnatchop(unittest.TestCase):
    good_input = """package body A is

end A;

package A is

end A;

procedure Main is
begin
   null;
end Main;
"""

    bad_input = """Garbage
More garbage
!fake_file.txt
blahblah


nothing

package body A is
   A Body

   A Body
end A;

package A is
    A Spec

    A Spec
end A;

procedure Main is
   main procedure
end Main;
"""

    def test_real_gnatchop(self):
        split = self.good_input.splitlines()
        inTest = real_gnatchop(split)
        self.assertEqual(len(inTest), 3, msg="real chop returned more/less than 3 resources")
        self.assertEqual(inTest[0].basename, "a.adb", msg="basename parsing failed")
        self.assertEqual(inTest[1].basename, "a.ads", msg="basename parsing failed")
        self.assertEqual(inTest[2].basename, "main.adb", msg="basename parsing failed")
        self.assertEqual(inTest[0].content, "\n".join(split[0:3]), msg=f"content parsing failed for {inTest[0].basename}")
        self.assertEqual(inTest[1].content, "\n".join(split[4:7]), msg=f"content parsing failed for {inTest[1].basename}")
        self.assertEqual(inTest[2].content, "\n".join(split[8:]), msg=f"content parsing failed for {inTest[2].basename}")

    def test_real_gnatchop_error(self):
        split = self.bad_input.splitlines()
        with self.assertRaises(Exception):
            real_gnatchop(split)


if __name__ == '__main__':
    unittest.main()
